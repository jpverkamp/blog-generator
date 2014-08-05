#lang racket

(provide load-plugins
         plugins
         plugins-set-site!
         plugins-set-post!
         pre-all!
         pre-render!
         post-render!
         post-all!
         slug)

(require "render.rkt"
         "post.rkt")

(define plugin-path "_plugins")

(define current-site #f)
(define current-post #f)

(define (current-site-wrapper . args) (apply current-site args))
(define (current-post-wrapper . args) (apply current-post args))

(define (plugins-set-site! site) (set! current-site site))
(define (plugins-set-post! post) (set! current-post post))

; Store any ordering requests for plugins
(define plugins-before (make-hash))
(define plugins-after (make-hash))

(define (make-listener-iterator type hash #:global global)
  (define (rotate ls)
    (cond
      [(or (null? ls) (null? (cdr ls))) ls]
      [else
       (reverse (cons (car ls) (reverse (cdr ls))))]))
  
  (define (sorted-plugins ls)
    (let loop ([done '()] [todo (rotate ls)] [loop-protection ls])
      (cond
        [(null? todo) (reverse done)]
        [else
         (define current (car todo))
         (cond
           ; The first plugin can be done now, go for it!
           [(and  ; We've done everything in this plugin's after list
             (andmap (λ (each) (member each done)) (hash-ref plugins-after current '()))
             ; No one in todo has this as a before
             (not (ormap (λ (each) (member current (hash-ref plugins-before each '()))) todo)))
            (loop (cons current done) (rotate (cdr todo)) (cdr todo))]
           ; We've looped through all current plugins. Error!
           [(equal? todo loop-protection)
            (error 'sorted-plugins "cannot order plugins")]
           ; Try the next arrangement
           [else
            (loop done (rotate todo) loop-protection)])])))
   
  (if global
      (λ (site)
        (set! current-site site)
        (for ([name (in-list (sorted-plugins (hash-keys hash)))])
          ((hash-ref hash name) site)))
      (λ (post site)
        (set! current-site site)
        (set! current-post post)
        (for ([name (in-list (sorted-plugins (hash-keys hash)))])
          ((hash-ref hash name) post site)))))

; Plugins that are called on the entire site before any posts are run
(define pre-all-listeners (make-hash))
(define pre-all!     (make-listener-iterator #:global #t 'pre-all pre-all-listeners))

; Plugins that are called before the renderer has run (content is a ScribbleHTML string)
(define pre-render-listeners (make-hash))
(define pre-render!  (make-listener-iterator #:global #f 'pre-render pre-render-listeners))

; Plugins that are called after the renderer has run (content is an HTML string)
(define post-render-listeners (make-hash))
(define post-render! (make-listener-iterator #:global #f 'post-render post-render-listeners))

; Plugins that are called on the entire site after any posts are run
(define post-all-listeners (make-hash))
(define post-all!    (make-listener-iterator #:global #t 'post-all post-all-listeners))

; Eventually this will be a hash of plugin name -> value
(define plugins (make-hash))
(define plugins-loaded #f)

; This will be available to plugins in order to register them to our system
(define (make-plugin-function type hash)
  (λ (name function #:before [before #f] #:after [after #f])
    (when (hash-has-key? hash name)
      (printf "{duplicate plugin: ~a} " name))
    
    (when before (hash-set! plugins-before name (if (list? before) before (list before))))
    (when after (hash-set! plugins-after name (if (list? after) after (list after))))
    
    (printf "~a (~a) " name type)
    (hash-set! hash name function)))

(define register-plugin             (make-plugin-function 'in-line     plugins))
(define register-pre-all-plugin     (make-plugin-function 'pre-all     pre-all-listeners))
(define register-pre-render-plugin  (make-plugin-function 'pre-render  pre-render-listeners))
(define register-post-render-plugin (make-plugin-function 'post-render post-render-listeners))
(define register-post-all-plugin    (make-plugin-function 'post-all    post-all-listeners))

; Special case the slug plugin
(define (slug str)
  (let* ([str (string-downcase str)]
         [str (regexp-replace* #px"'" str "")]
         [str (regexp-replace* #px"[^a-z0-9]+" (string-downcase str) "-")]
         [str (string-trim str)])
    str))

(register-plugin 'slug slug)

; Load all racket files in the _plugins directory
(define (load-plugins)
  (when (not plugins-loaded)
    (for ([file (in-directory plugin-path)]
          #:when (regexp-match #px".rkt$" file))
      (printf "  ~a: " (file-name-from-path file))
      
      (parameterize ([current-namespace (make-base-namespace)])
        (namespace-require 'racket)
        (namespace-require 'racket/date)
        (namespace-require 'racket/list)
        (namespace-require 'xml)
        
        ; Allow plugin files to register themselves
        (eval `(define register-plugin ,register-plugin))
        (eval `(define register-pre-all-plugin ,register-pre-all-plugin))
        (eval `(define register-pre-render-plugin ,register-pre-render-plugin))
        (eval `(define register-post-render-plugin ,register-post-render-plugin))
        (eval `(define register-post-all-plugin ,register-post-all-plugin))
        
        ; Allow plugins access to the rendering function
        ; Set a default environment to the other plugins so they can nest
        (define (plugin-render to-render #:environment [env (hash)] #:markdown? [markdown? #f])
          (define plugins+env (for/fold ([env (hash-copy plugins)]) ([(k v) (in-hash env)]) (hash-set env k v)))
          (render to-render #:environment plugins+env #:markdown? markdown?))
        (eval `(define render ,plugin-render))
        
        ; Add the slug plugin
        (eval `(define slug ,slug))
        
        ; Allow plugins to make new posts
        (eval `(define empty-post ,empty-post))
        
        ; Allow access to the current site and post from within the plugin
        (eval `(define site ,current-site-wrapper))
        (eval `(define post ,current-post-wrapper))
        
        ; Read and evaluate each expression in the plugin file in turn
        (with-input-from-file file
          (thunk
           (define sexp `(let () ,@(for/list ([sexp (in-list (port->list))]) sexp) (void)))
           (eval sexp))))
      
      (newline))
    
    ; Don't reload plugins even if this function is called more than once
    (set! plugins-loaded #t)))
