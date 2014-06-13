(define (latex . body*)
  (match body*
    [`(inline . ,body*)
     `(span "\\\\(" ,(apply string-append body*) "\\\\)")]
    [`(block . ,body*)
     `(div "$$" ,(apply string-append body*) "$$")]
    [any ; default to block mode
     `(div "$$" ,(apply string-append body*) "$$")]))

(register-plugin 'latex latex)
