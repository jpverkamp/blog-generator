(define rankings
  (make-hash))

(define (ranking index list-title)
  (define new-list
    (let loop ([i 1] [ls (hash-ref rankings list-title '())])
      (cond
        [(or (= i index) (null? ls))
         (cons `(,(post "title") ,(~a (or (site "url") " ") "/" (post "permalink"))) ls)]
        [else
         (cons (car ls) (loop (+ i 1) (cdr ls)))])))
  
  (hash-set! rankings list-title new-list)
  
  `(div (h3 ((class "ranking-title")) ,list-title)
        (ol ,@(for/list ([item (in-list (hash-ref rankings list-title))])
                (match-define `(,title ,link) item)
                `(li (a ((href ,link)) ,title))))))
  
(register-plugin 'ranking ranking)