(define (embed #:full-size [full-size #f] . args)
  (define classes 
    (cond
      [(> (length args) 1)
       (string-join (map ~a (reverse (cdr (reverse args)))) " ")]
      [else
       #f]))
  
  (define src
    (last args))
  
  (define absolute-path (~a (or (site "url") "/") "/" (or (post "permalink") ".") "/" src))

  (cond
    [(regexp-match #px"\\.(png|jpg|jpeg|gif)$" src)
     `(a ((data-toggle "lightbox") 
          (href ,(or full-size absolute-path))
          ,@(if classes `((class ,classes)) `()))
         (img ((src ,absolute-path))))]
    [full-size
     `(a ((data-toggle "lightbox") 
          (href ,(or full-size absolute-path))
          ,@(if classes `((class ,classes)) `()))
         ,src)]
    [else
     `(a ((href ,(or full-size absolute-path))
          ,@(if classes `((class ,classes)) `()))
         ,src)]))
   
(register-plugin 'embed embed)