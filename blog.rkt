#lang at-exp racket

(require racket/runtime-path
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
    
    (define attachment-path (regexp-replace #px"\\.htm" (path->string path) ""))
    (when (directory-exists? attachment-path)
      (new-post "files-path" attachment-path))
    
    new-post))

(printf "Formatting contents...\n")
(for ([post (in-list posts)])
  (with-handlers ([exn:fail? (λ (err) 
                               (printf "Failed in '~a': ~a\n"
                                       (post "title")
                                       (exn-message err)))])
    (post "content" (render (post "content") #:environment plugins))))

(printf "Writing posts...\n")
(system (format "rm -rf '~a'" output-path))
(make-directory output-path)
(for ([post (in-list posts)])
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
      (display @post{content}))))