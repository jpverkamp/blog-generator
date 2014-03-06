(define (code lang . body*)
  `(pre ((lang ,lang))
        (code ,@body*)))

(register-plugin 'code code)