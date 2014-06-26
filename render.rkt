#lang at-exp rackjure

(provide render)

(require (only-in markdown parse-markdown)
         scribble/reader
         sxml/html
         "post.rkt")

; Create only one namespace per run
; TODO: Do we need to do this?
(define render-namespace (make-base-namespace))
(parameterize ([current-namespace render-namespace])
  ; For html elements as functions
  (namespace-require 'html)
  
  ; Fix for @ expressions after they've been processed
  (eval `(define @ '@)))

; Turn anything into a string (xexprs that are elements get parsed, everything else is ~a'd)
(define (stringify thing)
  (define (attrs key* val*) (string-join (map (λ (key val) (format "~a=\"~a\"" key val)) key* val*) " "))
  (define (str-body body*)  (apply string-append (map stringify body*)))
  
  (match thing
    [(or (? void?) '())
     ""]
    [(? string?) 
     thing]
    [`(,tag ((,key ,val) (,key* ,val*) ...) ,body ,body* ...)
     (string-append "<" (~a tag) " " (attrs (cons key key*) (cons val val*)) ">" (str-body (cons body body*)) "</" (~a tag) ">")]
    [`(,tag ((,key ,val) (,key* ,val*) ...))
     (string-append "<" (~a tag) " " (attrs (cons key key*) (cons val val*)) " />")]
    [`(,tag ,body ,body* ...)
     (string-append "<" (~a tag) ">" (str-body (cons body body*)) "</" (~a tag) ">")]
    [`(,tag)
     (string-append "<" (~a tag) " />")]
    [any
     (~a any)]))

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
(define (render to-render #:environment [env (hash)] #:markdown? [markdown? #f])
  (cond
    ; Strings should be read and parsed
    [(string? to-render)
     (call-with-input-string to-render (λ (in) (render in #:environment env #:markdown? markdown?)))]
    
    ; Run at-expressions then parse as markdown with embedded html
    [(input-port? to-render)
     (parameterize ([current-namespace render-namespace])
       ; Bind each of the plugins
       (for ([(k v) (in-hash env)])
         (if (procedure? v)
             (eval `(define ,k ,v))
             (eval `(define ,k ',v))))
       
       ; Thank you Rackjure...
       (cond
         [markdown?
          (~> to-render
              read-inside
              run-at-exps
              parse-markdown
              flatten-to-html
              string-trim
              strip-excess-whitespace)]
         [else
          (~> to-render
              read-inside
              run-at-exps
              string-trim
              strip-excess-whitespace)]))]
    
    ; Non-string xexprs should be stringed, then parsed
    ; This will catch at-expressions at the cost of being wicked slow
    ; TODO: Only recur if there is actually an at-expression in the string
    [else
     (render (stringify to-render) #:environment env #:markdown? markdown?)])) 
