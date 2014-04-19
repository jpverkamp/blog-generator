#lang racket

(provide load-plugins
         plugins
         pre-render!
         post-render!)

(require racket/runtime-path
         "render.rkt")

(define-runtime-path plugin-path "_plugins")

; Plugins that are called before the renderer has run (content is a ScribbleHTML string)
(define pre-render-listeners '())
(define (pre-render! post)
  (for ([listener (in-list pre-render-listeners)])
    (listener post)))

; Plugins that are called after the renderer has run (content is an HTML string)
(define post-render-listeners '())
(define (post-render! post)
  (for ([listener (in-list post-render-listeners)])
    (listener post)))

; Eventually this will be a hash of plugin name -> value
(define plugins #f)

; This will be available to plugins in order to register them to our system
(define (register-plugin name function)
  (when (hash-has-key? plugins name)
    (error 'register-plugin "Duplicate plugin detected: ~a" name))
  (printf "  Plugin registered: ~a\n" name)
  (hash-set! plugins name function))

(define (register-pre-render-plugin function)
  (set! pre-render-listeners (cons function pre-render-listeners)))

(define (register-post-render-plugin function)
  (set! post-render-listeners (cons function post-render-listeners)))

; Load all racket files in the _plugins directory
(define (load-plugins)
  (when (not plugins)
    (set! plugins (make-hash))
    (for ([file (in-directory plugin-path)]
          #:when (regexp-match #px".rkt$" file))
      (parameterize ([current-namespace (make-base-namespace)])
        ; Allow plugin files to register themselves
        (eval `(define register-plugin ,register-plugin))
        (eval `(define register-pre-render-plugin ,register-pre-render-plugin))
        (eval `(define register-post-render-plugin ,register-post-render-plugin))
        
        ; Allow plugins access to the rendering function
        (eval `(define render ,render))
        
        ; Read and evaluate each expression in the plugin file in turn
        (with-input-from-file file
          (thunk
           (define sexp `(let () ,@(for/list ([sexp (in-list (port->list))]) sexp)))
           (eval sexp)))))))
