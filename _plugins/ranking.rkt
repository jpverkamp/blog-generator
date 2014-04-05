(define rankings
  (make-hash))

(let ([post #f])
  
  (define (ranking index list-title)
    (define title (post "title"))
    (define new-list
      (let loop ([i 1] [ls (hash-ref rankings list-title '())])
        (cond
          [(or (= i index) (null? ls))
           (cons title ls)]
          [else
           (cons (car ls) (loop (+ i 1) (cdr ls)))])))
    
    (hash-set! rankings list-title new-list)
    
    `(div (h3 ,list-title)
          (ol ,@(for/list ([item (in-list (hash-ref rankings list-title))])
                  `(li (a ((href (local-link ,item))) ,item))))))
  
  (register-plugin 'ranking ranking #:pre-render (λ (new-post) (set! post new-post))))