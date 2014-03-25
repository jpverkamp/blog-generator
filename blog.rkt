#lang at-exp racket

(require racket/date
         racket/runtime-path
         "plugin.rkt"
         "post.rkt"
         "render.rkt")

(define-runtime-path posts-path "_posts")
(define-runtime-path output-path "_build")

(define (slugify str)
  (regexp-replace
   #px"--+"
   (list->string
    (for/list ([c (in-string (string-downcase str))])
      (cond
        [(or (char-alphabetic? c)
             (char-numeric? c)
             (member c '(#\.)))
         c]
        [else #\-])))
   "-"))

(printf "Loading plugins...\n")
(load-plugins)

(printf "Reading posts...\n")
(define posts
  (for/list ([path (in-directory posts-path)]
             #:when (regexp-match #px"\\.htm$" path))
    
    (define new-post (with-input-from-file path read-post))
    
    (new-post "path" (path->string path))
    
    (define attachment-path (regexp-replace #px"\\.htm" (path->string path) ""))
    (when (directory-exists? attachment-path)
      (new-post "files-path" attachment-path))
    
    new-post))

(parameterize ([date-display-format 'iso-8601])
  (set! posts (sort posts (λ (post1 post2)
                            (and (post1 "date")
                                 (post2 "date")
                                 (string<? (date->string (post1 "date"))
                                           (date->string (post2 "date"))))))))

(printf "Formatting contents...\n")
(for ([post (in-list posts)])
  (with-handlers ([exn:fail? (λ (err) 
                               (printf "Failed in '~a': ~a\n"
                                       (post "title")
                                       (exn-message err)))])
    (pre-render! post)
    (post "content" (render (post "content") #:environment plugins))
    (post-render! post)))

(printf "Writing posts...\n")
(system (format "rm -rf '~a'" output-path))
(make-directory output-path)
(for ([post (in-list posts)])
  (with-handlers ([exn? (λ (exn) (printf "Could not write '~a': ~a\n" (post "title") (exn-message exn)))])
    (define date @post{date})
    (define path 
      (build-path output-path
                  (~a (date-year date)  #:width 4 #:align 'right #:pad-string "0")
                  (~a (date-month date) #:width 2 #:align 'right #:pad-string "0")
                  (~a (date-day date)   #:width 2 #:align 'right #:pad-string "0")
                  (slugify @post{title})))
    
    (make-directory* path)
    (when @post{files-path}
      (for ([file (in-directory @post{files-path})])
        (copy-file file (build-path path (file-name-from-path file)))))
    
    (with-output-to-file (build-path path "index.htm")
      (thunk
       (display @post{content})))))