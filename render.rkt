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
  (namespace-require 'html))

; Turn anything into a string (xexprs that are elements get parsed, everything else is ~a'd)
(define (stringify thing)
  (define (attrs key* val*) (string-join (map (λ (key val) (format "~a=\"~a\"" key val)) key* val*) " "))
  (define (str-body body*)  (apply string-append (map stringify body*)))
  
  (define (voidable? tag)
    (member tag '(area base br col command embed hr img input keygen link meta param source track wbr)))
  
  (match thing
    [(or (? void?) '())
     ""]
    [(? string?) 
     thing]
    [`(,(? voidable? tag) ((,key ,val) (,key* ,val*) ...))
     (string-append "<" (~a tag) " " (attrs (cons key key*) (cons val val*)) " />")]
    [`(,tag ((,key ,val) (,key* ,val*) ...) ,body* ...)
     (string-append "<" (~a tag) " " (attrs (cons key key*) (cons val val*)) ">" (str-body body*) "</" (~a tag) ">")]
    [`(,(? voidable? tag))
     (string-append "<" (~a tag) " />")]
    [`(,tag ,body* ...)
     (string-append "<" (~a tag) ">" (str-body body*) "</" (~a tag) ">")]
    [any
     (~a any)]))

; Process all chunks, evaluating at-expressions and turning everything else into a string
(define (run-at-exps ls)
  (apply
   string-append
   (for/list ([index (in-naturals)]
              [chunk (in-list ls)] #:break (eof-object? chunk))
     (with-handlers ([exn? (λ (exn) 
                             (error 'render/run-at-exps
                                    "~a in chunk ~a: ~a"
                                    (exn-message exn)
                                    index
                                    chunk))])
       (stringify (eval chunk))))))

; Add paragraphs to a mostly html document
(define re-block
  (pregexp (string-append 
            "^\\s*<("
            (string-join (list "address" "article" "aside" "audio" "blockquote" "canvas"
                               "dd" "div" "dl" "fieldset" "figcaption" "figure" "footer"
                               "form" "h1" "h2" "h3" "h4" "h5" "h6" "header" "hgroup" 
                               "hr" "noscript" "ol" "output" "p" "pre" "section" "table"
                               "tfoot" "ul" "video"
                               ; Special cases for inline javascript / styles
                               "script" "style"
                               ; Special cases for XML
                               "\\?xml" "feed")
                         "|")
            ")[^>]*/?>")))

(define (add-paragraphs str)
  (define last-line (make-parameter (list 0 #f)))
  
  (with-handlers ([exn? (λ (exn)
                          (error 'render:add-paragraphs
                                 "~a at line ~a: ~a"
                                 (exn-message exn)
                                 (first (last-line))
                                 (second (last-line))))])
    (with-input-from-string str
      (λ ()
        (let loop ([output '()]
                   [buffer '()]
                   [match-stack '()])
          ; Loop while moving the buffer to output as a paragraph
          (define (buffer->output [last-line #f])
            (define new-buffer (if last-line (cons last-line buffer) buffer))
            (cond
              [(null? new-buffer)
               output]
              [else
               (cons (string-append "<p>" 
                                    (string-join (map string-trim (reverse new-buffer)) " ")
                                    "</p>")
                     output)]))
          
          (define line (read-line))
          (last-line (list (+ 1 (first (last-line))) line))
          
          (cond
            ; At the end, put the strings back together
            ; Output buffer one last time if necessary
            [(eof-object? line)
             (string-join (reverse (buffer->output)) "\n")]
            
            ; If we currently have a match stack, check for that first
            ; If we close a tag, take it off
            [(and (not (null? match-stack))
                  (regexp-match (third (first match-stack)) line))
             (cond
               ; Are we closing a paragraphizing tag? Make sure we get the last paragraph
               [(first (first match-stack)) 
                (match-define (list (list-rest before after) (list tag))
                  (list (regexp-split (third (first match-stack)) line)
                        (regexp-match (third (first match-stack)) line)))
                (loop `(,@after ,tag . ,(buffer->output before)) '() (cdr match-stack))]
               ; Nope, continue on with our regularly scheduled text
               [else
                (loop (cons line (buffer->output)) '() (cdr match-stack))])]
            ; If we open a nested tag, add another copy to the stack
            [(and (not (null? match-stack))
                  (regexp-match (second (first match-stack)) line))
             (loop (cons line (buffer->output)) '() (cons (first match-stack) match-stack))]
            ; If we still have a stack and we're not paragraphizing, add directly to output
            [(and (not (null? match-stack))
                  (not (first (first match-stack))))
             (loop (cons line output) '() match-stack)]
            
            ; Do not paragraphize <!--more-->
            [(equal? "<!--more-->" line)
             (loop (cons line (buffer->output)) '() match-stack)]
            ; Empty lines end the current buffer
            [(equal? "" (string-trim line))
             (loop (buffer->output) '() match-stack)]
            ; Opening block level tags 
            [(regexp-match re-block line)
             => (λ (match)
                  (define tag (cadr match))
                  (cond
                    ; We match the end as well
                    [(or (regexp-match (pregexp (~a "</" tag ".*?>")) line)
                         (regexp-match (pregexp (~a "<" tag ".*?/>")) line))
                     (loop (cons line (buffer->output)) '() match-stack)]
                    ; Nope, add to the stack
                    [else
                     (loop (cons line (buffer->output))
                           '()
                           (cons (list (regexp-match #px"<!--allow-paragraphs-->" line)
                                       (pregexp (~a "<" tag ".*?>"))
                                       (pregexp (~a "</" tag ".*?>")))
                                 match-stack))]))]
            ; Anything else gets added to the buffer
            [else
             (loop output (cons line buffer) match-stack)]))))))

; Flatten a mixed list of strings and x-expressions to an html string
(define (flatten-to-html ls)
  (string-join (map stringify ls) "\n"))

; Remove extra whitespace from an 
(define (strip-excess-whitespace str)
  (regexp-replace* #px"\n\n+" str "\n\n"))

; Parse a string or port containing xexprs
(define (render to-render #:environment [env (hash)] #:add-paragraphs [add-paragraphs? #f])
  (cond
    ; Strings should be read and parsed
    [(string? to-render)
     (call-with-input-string to-render (λ (in) (render in #:environment env #:add-paragraphs add-paragraphs?)))]
    
    ; Run at-expressions then parse as markdown with embedded html
    [(input-port? to-render)
     (parameterize ([current-namespace render-namespace])
       ; Bind each of the plugins
       (for ([(k v) (in-hash env)])
         (if (procedure? v)
             (eval `(define ,k ,v))
             (eval `(define ,k ',v))))
       
       (define maybe-add-paragraphs (if add-paragraphs? add-paragraphs identity))
       
       ; Thank you Rackjure...
       (~> to-render
           read-inside
           run-at-exps
           maybe-add-paragraphs
           string-trim
           strip-excess-whitespace))]
    
    ; Non-string xexprs should be stringed, then parsed
    ; This will catch at-expressions at the cost of being wicked slow
    ; TODO: Only recur if there is actually an at-expression in the string
    [else
     (render (stringify to-render) #:environment env #:add-paragraphs add-paragraphs?)]))
