(define (codeblock lang . body*)
  `(pre ((lang ,lang))
        (code ,@body*)))

(register-plugin 'codeblock codeblock)