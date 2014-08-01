(define (codeblock lang . body*)
  (define (html-encode line)
    (let* ([line (regexp-replace* #px"<" line "\\&lt;")]
           [line (regexp-replace* #px">" line "\\&gt;")])
      line))
  
  (when (equal? lang "scheme")
    (set! lang "lisp"))
  
  `(pre ((class ,lang))
        (code ,@(map html-encode body*))))

(register-plugin 'codeblock codeblock)

(define (code-ify-backticks post site)
  (when (post "content")
    (post "content" 
          (regexp-replace* #px"`([^\n`]+)`" 
                           (post "content") 
                           (λ (_ text) (~a "<code>" text "</code>"))))))

(register-pre-render-plugin 'code-ify-backticks code-ify-backticks)