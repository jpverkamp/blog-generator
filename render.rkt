#lang at-exp rackjure

(provide render)

(require (only-in markdown parse-markdown)
         scribble/reader
         sxml/html
         xml
         "post.rkt")

; Turn anything into a string (xexprs that are elements get parsed, everything else is ~a'd)
(define (stringify thing)
  (match thing
    [(? void?)
     ""]
    [(? string?)
     thing]
    [(and (? list?) (? xexpr?))
     (xexpr->string thing)]
    [any
     (~a thing)]))

; Process all chunks, evaluating at-expressions and turning everything else into a string
(define (run-at-exps ls)
  (apply
   string-append
   (for/list ([chunk (in-list ls)] #:break (eof-object? chunk))
     (stringify (eval chunk)))))

; Flatten a mixed list of strings and x-expressions to an html string
(define (flatten-to-html ls)
  (string-join (map stringify ls) "\n"))

; Remove extra whitespace from an 
(define (strip-excess-whitespace str)
  (regexp-replace* #px"\n\n+" str "\n\n"))

; Parse a string or port containing xexprs
(define (render to-render #:environment [env (hash)])
  (cond
    ; Strings should be read and parsed
    [(string? to-render)
     (call-with-input-string to-render (λ (in) (render in #:environment env)))]
    
    ; Non-string xexprs should be stringed, then parsed
    ; This will catch at-expressions at the cost of being wicked slow
    ; TODO: Only recur if there is actually an at-expression in the string
    [(xexpr? to-render)
     (render to-render #:environment env)]
    
    ; Run at-expressions then parse as markdown with embedded html
    [(input-port? to-render)
     (parameterize ([current-namespace (make-base-namespace)])
       ; For html elements as functions
       (namespace-require 'html)
       
       ; Fix for @ expressions after they've been processed
       (eval `(define @ '@))
       
       ; Bind each of the plugins
       (for ([(k v) (in-hash env)])
         (if (procedure? v)
             (eval `(define ,k ,v))
             (eval `(define ,k ',v))))
       
       ; Thank you Rackjure...
       (~> to-render
           read-inside
           run-at-exps
           parse-markdown
           flatten-to-html
           string-trim))]))
