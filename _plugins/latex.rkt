(define (latex . body*)
  `(span "$$" ,(apply string-append body*) "$$"))

(register-plugin 'latex latex)
