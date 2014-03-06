#lang at-exp rackjure

(provide render)

(require scribble/reader
         sxml/html
         xml)

; Group things by newlines
(define (combine-by-newlines ls)
  ; Test if something is both a string and not a newline
  (define (string-not-newline? thing)
    (and (string? thing)
         (not (equal? thing "\n"))))
  
  (let loop ([ls ls])
    (match ls
      ; (string string . rest) -> (string+string . rest)
      [`(,(? string-not-newline? str1) ,(? string-not-newline? str2) . ,rest)
       (loop `(,(string-append str1 " " str2) . ,rest))]
      ; (string newline string . rest) -> (string+newline+string . rest)
      [`(,(? string-not-newline? str1) "\n" ,(? string-not-newline? str2) . ,rest)
       (loop `(,(string-append str1 "\n" str2) . ,rest))]
      ; Skip things that can't be combined
      [`(,first . ,rest)
       `(,first . ,(loop rest))]
      ; End when we're done
      [`()
       `()])))

; Parse a 'chunk' (as split by @ expressions)
; (or/c string? list?) -> (listof xexpr?)
(define (process-chunk chunk)
  (match chunk
    ; Strings / non-@-expressions are parsed to html xexprs
    [(? string?)
     (match (html->xexp chunk)
       [`(*TOP* . ,rest)
        rest]
       [(? list? ls)
        ls]
       [result
        (list result)])]
    ; Everything else is eval'ed:
    ; - functions that return void are ignored (return an empty list)
    ; - functions that return an html xexpr are parsed as such
    ; - functions that return a string are processed by the first step
    [_
     (match (eval chunk)
       [(? void?)
        '()]
       [(? string? subchunk)
        (process-chunk subchunk)]
       [(? list? ls)
        (list ls)])]))

; Process all chunks, evaluating at-expressions and/or parsing to xexprs
(define (run-at-exps ls)
  (apply
   append
   (filter
    (negate void?)
    (for/list ([chunk (in-list ls)] #:break (eof-object? chunk))
      (process-chunk chunk)))))

; Recursively remove the extra @ html-parsing inserts for attributes
(define (remove-@-attributes ls)
  (map (λ (each) (let recur ([chunk each])
                   (match chunk
                     [`(,name (@ (,keys ,vals) ...) . ,children)
                      `(,name ,(map list keys vals) . ,(map recur children))]
                     [`(,name . ,children)
                      `(,name . ,(map recur children))]
                     [any any])))
       ls))

; Group paragraphs together (start a new paragraph at a double newline)
(define (group-paragraphs ls)
  (let loop ([ls ls] [buffer '()])
    (match ls
      ; At the end, output a buffer if we haven't, nothing if we don't
      [(and '() (? (thunk* (null? buffer))))
       `()]
      [`()
       `((p ,@(reverse buffer)))]
      ; Double newline means output the buffer
      [(list-rest "\n" "\n" rest)
       `((p ,@(reverse buffer)) . ,(loop rest '()))]
      ; Single newlines are replaced with a space
      [(list-rest "\n" rest)
       (loop rest (cons " " buffer))]
      ; Anything else just gets stuck together
      [(list-rest first rest)
       (loop rest (cons first buffer))])))

; Remove paragraph tags if they have only a single child and it's not a string
(define (remove-non-paragraphs ls)
  (map (λ (each)
         (match each
           [`(p ,(? list? child))
            child]
           [_ each]))
       ls))

; Insert newlines for readability
(define (insert-newlines ls)
  (apply append (map (λ (each) (list each "\n")) ls)))        

; Parse a string or port containing xexprs
(define (render in/str #:environment [env (hash)])
  (define in (if (string? in/str) (open-input-string in/str) in/str))
  (parameterize ([current-namespace (make-base-namespace)])
    (namespace-require 'html)
    (eval `(define @ '@))
    (for ([(k v) (in-hash env)])
      (if (procedure? v)
          (eval `(define ,k ,v))
          (eval `(define ,k ',v))))
  
    (apply string-append 
           (map xexpr->string
                (~> in
                    read-inside
                    combine-by-newlines
                    run-at-exps
                    remove-@-attributes
                    group-paragraphs
                    remove-non-paragraphs
                    insert-newlines)))))

; Make it printable
(define (parsed->string ls)
  (apply string-append (map xexpr->string ls)))


