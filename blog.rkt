#lang at-exp racket

(require file/sha1
         racket/cmdline
         racket/date
         racket/runtime-path
         "plugin.rkt"
         "post.rkt"
         "render.rkt")

(define-runtime-path posts-path "_posts")
(define-runtime-path output-path "_build")
(define-runtime-path templates-path "_templates")
(define-runtime-path cache-path "_cache")
(define-runtime-path config-file "config.yaml")

(make-directory* cache-path)

(define print-progress
  (let ([current-progress #f])
    (λ (new-progress)
      (when (not (equal? current-progress new-progress))
        (set! current-progress new-progress)
        (displayln current-progress)))))

(printf "Loading site config...\n")
(define site
  (if (file-exists? config-file)
      (with-input-from-file config-file read-post)
      (make-hash)))
(site "content" #:delete #t)

(define files-to-parse
  (command-line
   #:program "blog.rkt"
   #:once-each 
   [("--bypass-cache") "Regenerate all pages, ignoring currently cached versions" (site "bypass-cache" #t)]
   #:args files
   (map (λ (file) (regexp-replace* #px"\\\\+" (path->string (path->complete-path (string->path file))) "/")) files)))

(printf "Loading plugins...\n")
(load-plugins)

(printf "Loading templates...\n")
(define templates
  (for/hash ([path (in-directory templates-path)]
             #:when (regexp-match #px"\\.html?$" path))
    (values (regexp-replace* #px"\\.[^.]*$" (path->string (file-name-from-path path)) "")
            (file->string path))))

(define (render-template name #:environment [environment (hash)])
  (define template (hash-ref templates name (format "missing template: ~a" name)))
  (render template #:environment environment #:markdown? #f))

(hash-set! plugins 'render-template render-template)

(printf "Reading posts...\n")
(define posts
  (for/list ([path (in-directory posts-path)]
             #:when (regexp-match #px"\\.htm$" path))
    
    (define new-post (with-input-from-file path read-post))
    
    (new-post "path" (path->string path))
    
    (define attachment-path (regexp-replace* #px"\\.htm$" (path->string path) ""))
    (when (directory-exists? attachment-path)
      (new-post "files-path" attachment-path))
    
    new-post))

(parameterize ([date-display-format 'iso-8601])
  (set! posts (sort posts (λ (post1 post2)
                            (and (post1 "date")
                                 (post2 "date")
                                 (string<? (date->string (post1 "date"))
                                           (date->string (post2 "date"))))))))

(printf "Generating permalinks...\n")
(for ([post (in-list posts)])
  (with-handlers ([exn:fail? (λ (err) (printf "Failed in '~a': ~a\n" (post "title") (exn-message err)))])
    (define date (or @post{date} (current-date)))
    
    (define (0pad thing width) (~a thing  #:width width #:align 'right #:pad-string "0"))
    (define permalink (or @post{permalink} @site{permalink} "{yymmdd}-{title}"))
    
    (define-syntax-rule (-> from to)
      (when (regexp-match from permalink) 
        ((thunk (set! permalink (string-replace permalink from to))))))
    
    (-> "{year}"   (0pad (date-year date)  4))
    (-> "{month}"  (0pad (date-month date) 2))
    (-> "{day}"    (0pad (date-day date)   2))
    (-> "{yymmdd}" (string-append (0pad (remainder (date-year date) 100) 2) (0pad (date-month date) 2) (0pad (date-day date) 2)))
    (-> "{title}"  (slug @post{title}))
    
    (post "permalink" permalink)))

(printf "Formatting contents...\n")
(site "posts" posts)
(pre-all! site)
(set! posts (site "posts"))

(for ([post (in-list posts)])
  (with-handlers ([exn:fail? (λ (err) (printf "Failed in '~a': ~a\n" (post "title") (exn-message err)))])
    ; Fix for Cygwin style paths on Windows, make them always Unix style
    (define system-agnostic-path
      (and @post{path} (regexp-replace* #px"\\\\+" (path->string (path->complete-path @post{path})) "/")))

    ; Check if we already have a cached version of the post (if we're not bypassing the cache)
    ; Any files passed on the command line are always reprocessed (although state dependent pages may only partially work)
    (define cache-hash (sha1 (open-input-string (~a post))))
    (define cache-file (build-path cache-path (string-append cache-hash ".cache")))
    (define cache-file-more (build-path cache-path (string-append cache-hash ".more")))
    (cond
      [(and (not @post{do-not-cache})
            (not @site{bypass-cache})
            (not (member system-agnostic-path files-to-parse))
            (file-exists? cache-file))
       (post "content" (file->string cache-file))
       (post "more" 
             (if (file-exists? cache-file-more) 
                 (file->string cache-file-more)
                 (post "content")))]
      [else
       (print-progress (format "  Rendering ~a" (or (and @post{path} (file-name-from-path @post{path}))
                                                    @post{title}
                                                    "{unknown}")))
       
       ; Allow posts to access their own metadata and the site's
       (hash-set! plugins 'post post)
       (hash-set! plugins 'site site)
       
       (post "content" (regexp-replace* "<!--more-->" (post "content") "☃MORE☃")) ; HACK to avoid markdown removing comments
       
       ; Render the main body of the post
       (pre-render! post site)
       (post "content" (render (post "content") #:environment plugins #:markdown? (not @post{disable-markdown})))
       (post-render! post site)
       
       (post "content" (regexp-replace* "☃MORE☃" (post "content") "<!--more-->")) ; HACK to avoid markdown removing comments
       
       ; Split the section above <!--more-->
       ; If that doesn't exist, the entire post becomes the more section
       (post "more" (car (string-split (post "content") "<!--more-->")))
       (with-output-to-file cache-file-more #:exists 'replace (thunk (display @post{more})))
       
       ; Render the template around the content
       (post "content" (render-template (post "template" #:default "post") #:environment plugins))
       
       ; Update the cache file
       (with-output-to-file cache-file #:exists 'replace (thunk (display @post{content})))])))

(site "posts" posts)
(post-all! site)
(set! posts (site "posts"))

(printf "Writing posts...\n")
(system (format "rm -rf '~a'" output-path))
(make-directory output-path)
(for ([post (in-list posts)])
  (with-handlers ([exn? (λ (exn) (printf "Could not write '~a': ~a\n" (post "title") (exn-message exn)))])
    ; Make sure that the post's path exists
    (define path (build-path output-path @post{permalink}))
    (make-directory* path)
    (when @post{files-path}
      (for ([file (in-directory @post{files-path})])
        (copy-file file (build-path path (file-name-from-path file)))))

    ; Write the file to the system
    (with-output-to-file (build-path path "index.htm")
      (thunk
       (display @post{content})))))

(printf "Copying static content...\n")
(system "cp -r _static/* _build/")