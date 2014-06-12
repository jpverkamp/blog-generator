(define site-ref #f)

; Add categories to each post, assuming they are nested with / delims
(define (add-nested-categories! posts site)
  (define categories (make-hash))
  (set! site-ref site)
  
  (for ([post (in-list posts)] #:when (post "categories"))
    (for ([category (in-list (post "categories"))])
      (for*/fold ([h categories]) ([part (in-list (string-split category "/"))])
        (hash-ref! h part (make-hash)))))
  
  (site "categories" categories))

(register-pre-all-plugin 'nested-categories add-nested-categories!)

; Inline plugin to render a nested navbar of first and second level categories in the bootstrap style
(define (generate-navbar categories)
  (define (make-category-link . levels)
    (regexp-replace* #px"/+" (string-join (list* (or (site-ref "url") "") "category" (map slug levels)) "/") "/"))
  
  `(ul ((class "nav navbar-nav"))
       ,@(for/list ([category (in-list (sort (hash-keys categories) string<?))])
           `(li ((class "dropdown"))
                (a ((href ,(make-category-link category)) (class "dropdown-toggle")) ,category (span ((class "caret"))))
                (ul ((class "dropdown-menu"))
                    ,@(for/list ([sub-category (in-list (sort (hash-keys (hash-ref categories category (hash))) string<?))])
                        `(li (a ((href ,(make-category-link category sub-category))) ,sub-category))))))))

(register-plugin 'navbar generate-navbar)

; Generate category summary pages
(define (generate-category-pages posts site)
  (displayln "generating category pages"))

(register-post-all-plugin 'generate-category-pages generate-category-pages)