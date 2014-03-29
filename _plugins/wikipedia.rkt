(define wikipedia 
  (case-lambda
    [(text)
     `(a ((href ,(format "https://en.wikipedia.org/wiki/~a" (regexp-replace #px" " text "_")))) ,text)]
    [(text title)
     `(a ((href ,(format "https://en.wikipedia.org/wiki/~a" (regexp-replace #px" " text "_")))) ,text)]))

(register-plugin 'wikipedia wikipedia)