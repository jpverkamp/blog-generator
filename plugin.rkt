#lang racket

(provide load-plugins
         plugins
         pre-all!
         pre-render!
         post-render!
         post-all!)

(require racket/runtime-path
         "render.rkt")

(define-runtime-path plugin-path "_plugins")

; Plugins that are called on the entire site before any posts are run
(define pre-all-listeners (make-hash))
(define (pre-all! posts site) (for ([(name listener) (in-hash pre-all-listeners)]) (listener posts site)))

; Plugins that are called before the renderer has run (content is a ScribbleHTML string)
(define pre-render-listeners (make-hash))
(define (pre-render! post site) (for ([(name listener) (in-hash pre-render-listeners)]) (listener post site)))

; Plugins that are called after the renderer has run (content is an HTML string)
(define post-render-listeners (make-hash))
(define (post-render! post site) (for ([(name listener) (in-hash post-render-listeners)]) (listener post site)))

; Plugins that are called on the entire site after any posts are run
(define post-all-listeners (make-hash))
(define (post-all! posts site) (for ([(name listener) (in-hash post-all-listeners)]) (listener posts site)))

; Eventually this will be a hash of plugin name -> value
(define plugins (make-hash))
(define plugins-loaded #f)

; This will be available to plugins in order to register them to our system
(define (make-plugin-function type hash)
  (λ (name function)
    (when (hash-has-key? hash name)
      (error type "Duplicate plugin detected: ~a" name))
    (printf "~a (~a) " name type)
    (hash-set! hash name function)))

(define register-plugin             (make-plugin-function 'in-line     plugins))
(define register-pre-all-plugin     (make-plugin-function 'pre-all     pre-all-listeners))
(define register-pre-render-plugin  (make-plugin-function 'pre-render  pre-render-listeners))
(define register-post-render-plugin (make-plugin-function 'post-render post-render-listeners))
(define register-post-all-plugin    (make-plugin-function 'post-all    post-all-listeners))

; Special case the slug plugin
(define (slug str)
  (string-trim (regexp-replace* #px"[^a-z0-9]+" (string-downcase str) "-") "-"))

(register-plugin 'slug slug)

; Load all racket files in the _plugins directory
(define (load-plugins)
  (when (not plugins-loaded)
    (for ([file (in-directory plugin-path)]
          #:when (regexp-match #px".rkt$" file))
      (printf "  ~a: " (file-name-from-path file))
      
      (parameterize ([current-namespace (make-base-namespace)])
        (namespace-require 'racket)
        
        ; Allow plugin files to register themselves
        (eval `(define register-plugin ,register-plugin))
        (eval `(define register-pre-all-plugin ,register-pre-all-plugin))
        (eval `(define register-pre-render-plugin ,register-pre-render-plugin))
        (eval `(define register-post-render-plugin ,register-post-render-plugin))
        (eval `(define register-post-all-plugin ,register-post-all-plugin))
        
        ; Allow plugins access to the rendering function
        (eval `(define render ,render))
        
        ; Add the slug plugin
        (eval `(define slug ,slug))
        
        ; Read and evaluate each expression in the plugin file in turn
        (with-input-from-file file
          (thunk
           (define sexp `(let () ,@(for/list ([sexp (in-list (port->list))]) sexp)))
           (eval sexp))))
      
      (newline))
    
    (set! plugins-loaded #t)))
