(define my-site #f)
(define my-post #f)

(define (embed src)
  (define absolute-path (~a (or (my-site "url") "/") "/" (or (my-post "permalink") ".") "/" src))
  (cond
    [(regexp-match #px"\\.(png|jpg|jpeg|gif)$" src)
     `(a ((href ,absolute-path)) (img ((src ,absolute-path))))]
    [else
     `(a ((href ,absolute-path)) ,src)]))

(register-pre-render-plugin 
 'embed-get-post
 (λ (post site) 
   (set! my-post post)
   (set! my-site site)))
   

(register-plugin 'embed embed)