(define (codeblock lang . body*)
  (when (equal? lang "scheme")
    (set! lang "lisp"))
  
  `(pre ((class ,lang))
        (code ,@body*)))

(register-plugin 'codeblock codeblock)

(define (code-ify-backticks post site)
  (when (post "content")
    (post "content" 
          (regexp-replace* #px"`([^\n`]+)`" 
                           (post "content") 
                           (λ (_ text) (~a "<code>" text "</code>"))))))

(register-pre-render-plugin 'code-ify-backticks code-ify-backticks)