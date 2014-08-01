(define my-site #f)
(define my-post #f)

(define (embed . args)
  (define classes 
    (cond
      [(> (length args) 1)
       (string-join (map ~a (reverse (cdr (reverse args)))) " ")]
      [else
       #f]))
  
  (define src
    (last args))
  
  (define absolute-path (~a (or (my-site "url") "/") "/" (or (my-post "permalink") ".") "/" src))
  (cond
    [(regexp-match #px"\\.(png|jpg|jpeg|gif)$" src)
     `(a ((data-toggle "lightbox") (href ,absolute-path) ,@(if classes `((class ,classes)) `())) 
         (img ((src ,absolute-path))))]
    [else
     `(a ((href ,absolute-path) ,@(if classes `((class ,classes)) `()))
         ,src)]))

(register-pre-render-plugin 
 'embed-get-post
 (λ (post site) 
   (set! my-post post)
   (set! my-site site)))
   
(register-plugin 'embed embed)