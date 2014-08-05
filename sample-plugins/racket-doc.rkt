(define (racket-doc symbol)
  `(code (a ((href ,(format "http://docs.racket-lang.org/search/index.html?q=~a" symbol))) ,symbol)))

(register-plugin 'racket-doc racket-doc)
