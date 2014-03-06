#lang racket

(provide load-plugins
         plugins)

(require racket/runtime-path)

(define-runtime-path plugin-path "_plugins")

; Eventually this will be a hash of plugin name -> value
(define plugins #f)

; This will be available to plugins in order to register them to our system
(define (register-plugin name function)
  (when (hash-has-key? plugins name)
    (error 'register-plugin "Duplicate plugin detected: ~a" name))
  (printf "  Plugin registered: ~a\n" name)
  (hash-set! plugins name function))

; Load all racket files in the _plugins directory
(define (load-plugins)
  (when (not plugins)
    (set! plugins (make-hash))
    (for ([file (in-directory plugin-path)]
          #:when (regexp-match #px".rkt$" file))
      (parameterize ([current-namespace (make-base-namespace)])
        (eval `(define register-plugin ,register-plugin))
        (with-input-from-file file
          (thunk
            (for ([sexp (in-list (port->list))])
              (eval sexp))))))))
