(define (add-nested-categories! posts site)
  (define categories (make-hash))
  
  (for ([post (in-list posts)] #:when (post "categories"))
    (for*/fold ([h categories]) ([category (in-list (post "categories"))]
                                 [part (in-list (string-split category "/"))])
      (hash-ref! h part (make-hash))))
  
  (site "categories" categories))

(register-pre-all-plugin 'nested-categories add-nested-categories!)