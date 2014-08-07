#lang at-exp racket

(require file/sha1
         racket/cmdline
         racket/date
         "plugin.rkt"
         "post.rkt"
         "render.rkt")

(define posts-path     "_posts")
(define output-path    "_build")
(define templates-path "_templates")
(define cache-path     "_cache")
(define config-file    "config.yaml")

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
   [("--bypass-cache")
    "Regenerate all pages, ignoring currently cached versions"
    (site "bypass-cache" #t)]
   [("--deploy")       
    "Deploy to s3"                                            
    (site "bypass-cache" #t) 
    (site "deploying" #t)]
   #:args files
   (map (λ (file) (regexp-replace* #px"\\\\+" (path->string (path->complete-path (string->path file))) "/")) files)))

; Make sure that the site url is set, fall back to an empty string (absolute urls)
(cond
  [(site "deploying")
   (site "url" (or @site{deploy-url} @site{url} ""))]
  [else
   (site "url" (or @site{url} ""))])

(printf "Loading plugins...\n")
(load-plugins)

(printf "Loading templates...\n")
(define templates
  (for/hash ([path (in-directory templates-path)]
             #:when (regexp-match #px"\\.html?$" path))
    (values (regexp-replace* #px"\\.[^.]*$" (path->string (file-name-from-path path)) "")
            (file->string path))))

(define (render-template name #:environment [environment (hash)])
  (cond 
    [(equal? name "_blank")
     (render "@post{content}" #:environment environment)]
    [(hash-ref templates name #f)
     => (λ (template)
          (render template #:environment environment))]
    [else
     (format "missing template: ~a" name)]))

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

(set! posts (sort posts (λ (post1 post2)
                          (< (if (post1 "date") (date->seconds (post1 "date")) +inf.0)
                             (if (post2 "date") (date->seconds (post2 "date")) +inf.0)))))

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
    (-> "{slug}"   (or @post{slug} (slug @post{title})))
    
    (post "permalink" permalink)))

(plugins-set-site! site)
(hash-set! plugins 'site site)

(printf "Formatting contents...\n")
(site "posts" posts)
(pre-all! site)
(set! posts (site "posts"))

(for ([post (in-list posts)])
  (flush-output)
  (with-handlers ([exn:fail? (λ (err) (printf "Failed in '~a': ~a\n" (post "title") (exn-message err)))])
    ; Fix for Cygwin style paths on Windows, make them always Unix style
    (define system-agnostic-path
      (and @post{path} (regexp-replace* #px"\\\\+" (path->string (path->complete-path @post{path})) "/")))

    ; Check if we already have a cached version of the post (if we're not bypassing the cache)
    ; Any files passed on the command line are always reprocessed (although state dependent pages may only partially work)
    (define cache-hash (sha1 (open-input-string (~a post))))
    (define cache-file (build-path cache-path (string-append cache-hash ".cache")))

    ; Allow posts to access their own metadata
    (plugins-set-post! post)
    (hash-set! plugins 'post post)
    
    (print-progress (format "  Rendering ~a" 
                            (or (and @post{path} (file-name-from-path @post{path}))
                                @post{title}
                                "{unknown}")))

    (cond
      ; Cached
      [(and (not @post{do-not-cache})
            (not @site{bypass-cache})
            (not (member system-agnostic-path files-to-parse))
            (file-exists? cache-file))
       (with-input-from-file cache-file
         (thunk
           (post "content" (read))
           (post "rendered" (read))))]
      ; Not cached
      [else
       ; Render the main body of the post
       (pre-render! post site)
       (post "content" (render (post "content") #:environment plugins #:add-paragraphs #t))
       (post-render! post site)
    
       ; Render the template around the content
       (post "rendered" (render-template (post "template" #:default "post") #:environment plugins))
       
       ; Update the cache file
       (with-output-to-file cache-file
         #:exists 'replace 
         (thunk 
           (write @post{content})
           (write @post{rendered})))])
    
    ; Split the section above <!--more-->
    ; If that doesn't exist, the entire post becomes the more section
    (post "more" (car (string-split (post "content") "<!--more-->")))))

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
    (with-output-to-file (build-path path (or @post{permalink-filename} "index.htm"))
      (thunk
       (display @post{rendered})))))

(printf "Copying static content...\n")
(system "cp -r _static/* _build/")

(when (site "deploying")
  (printf "Deploying to s3...\n")
  
  (when (not @site{deploy-bucket})
    (error 'deploy "ERROR: deploy-bucket not specified"))
  
  (define s3cmd (find-executable-path "s3cmd"))
  (when (not s3cmd)
    (error 'deploy "ERROR: Cannot find s3cmd"))
  
  (current-directory output-path)
  (printf "Converting line endings...\n")
  (system "find . -type f -exec dos2unix {} \\; 2>1 > /dev/null")
  
  (printf "Uploading...\n")
  (system* s3cmd "sync" "--delete-removed" "." @site{deploy-bucket}))