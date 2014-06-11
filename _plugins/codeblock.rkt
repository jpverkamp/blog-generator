(define (codeblock lang . body*)
  (when (equal? lang "scheme")
    (set! lang "lisp"))
  
  `(pre ((class ,lang))
        (code ,@body*)))

; ((class ,(format "prettyprint linenums lang-~a" lang))) 

(register-plugin 'codeblock codeblock)