; Add categories to each post, assuming they are nested with / delims
(define (add-date-categories! site)
  ; Add date based 'categories' to each post
  (define (0pad thing width) (~a thing  #:width width #:align 'right #:pad-string "0"))
  (for ([post (in-list (site "posts"))] #:when (post "date"))
    (define date (post "date"))
    (define ymd-category (format "Archives/~a/~a" (0pad (date-year date) 4) (0pad (date-month date) 2)))
    (post "categories" (cons ymd-category (or (post "categories") '())))))

(register-pre-all-plugin 'add-date-categories! add-date-categories! #:before 'nested-categories)