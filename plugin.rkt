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
(define pre-all-listeners '())
(define (pre-all! post site) (for ([listener (in-list pre-all-listeners)]) (listener post site)))

; Plugins that are called before the renderer has run (content is a ScribbleHTML string)
(define pre-render-listeners '())
(define (pre-render! post site) (for ([listener (in-list pre-render-listeners)]) (listener post site)))

; Plugins that are called after the renderer has run (content is an HTML string)
(define post-render-listeners '())
(define (post-render! post site) (for ([listener (in-list post-render-listeners)]) (listener post site)))

; Plugins that are called on the entire site after any posts are run
(define post-all-listeners '())
(define (post-all! post site) (for ([listener (in-list post-all-listeners)]) (listener post site)))

; Eventually this will be a hash of plugin name -> value
(define plugins #f)

; This will be available to plugins in order to register them to our system
(define (register-plugin name function)
  (when (hash-has-key? plugins name)
    (error 'register-plugin "Duplicate plugin detected: ~a" name))
  (printf "  Plugin registered: ~a\n" name)
  (hash-set! plugins name function))

(define (register-pre-all-plugin function)     (set! pre-all-listeners (cons function pre-all-listeners)))
(define (register-pre-render-plugin function)  (set! pre-render-listeners (cons function pre-render-listeners)))
(define (register-post-render-plugin function) (set! post-render-listeners (cons function post-render-listeners)))
(define (register-post-all-plugin function)    (set! post-all-listeners (cons function post-all-listeners)))

; Load all racket files in the _plugins directory
(define (load-plugins)
  (when (not plugins)
    (set! plugins (make-hash))
    (for ([file (in-directory plugin-path)]
          #:when (regexp-match #px".rkt$" file))
      (parameterize ([current-namespace (make-base-namespace)])
        ; Allow plugin files to register themselves
        (eval `(define register-plugin ,register-plugin))
        (eval `(define register-pre-all-plugin ,register-pre-all-plugin))
        (eval `(define register-pre-render-plugin ,register-pre-render-plugin))
        (eval `(define register-post-render-plugin ,register-post-render-plugin))
        (eval `(define register-post-all-plugin ,register-post-all-plugin))
        
        ; Allow plugins access to the rendering function
        (eval `(define render ,render))
        
        ; Read and evaluate each expression in the plugin file in turn
        (with-input-from-file file
          (thunk
           (define sexp `(let () ,@(for/list ([sexp (in-list (port->list))]) sexp)))
           (eval sexp)))))))
