(define site-ref #f)
(define categories #f)

; Add categories to each post, assuming they are nested with / delims
(define (add-nested-categories! site)
  (set! categories (make-hash))
  (set! site-ref site)
  
  ; Create a nested category hash with a key *posts* at each level listing all posts
  (for ([post (in-list (site "posts"))] #:when (post "categories"))
    (for ([category (in-list (post "categories"))])
      (define last-hash
        (for*/fold ([h categories]) ([part (in-list (string-split category "/"))])
          (hash-set! h '*posts* (set-add (hash-ref h '*posts* (set)) post))
          (hash-ref! h part (make-hash))))
      (hash-set! last-hash '*posts* (set-add (hash-ref last-hash '*posts* (set)) post))))
  
  ; Generate a page for each subcategory
  (let loop ([h categories] [path "/"] [name #f])
    (when name
      (define new-post (empty-post))
      (new-post "title" name)
      (new-post "do-not-cache" #t)
      (new-post "disable-markdown" #t)
      (new-post "template" "category-list")
      (new-post "permalink" (string-join (cons "category" (map slug (string-split path "/"))) "/"))
      
      (new-post "template" "post")
      (new-post "content" "@generate-category-listing{@post{category}}")
      
      #;(new-post "template" "category-list")
      #;(new-post "content" "")
      
      (new-post "category" path)
      (site "posts" (cons new-post (site "posts"))))
    
    (for ([category (in-list (hash-keys h))] #:when (string? category))
      (loop (hash-ref h category (hash))
            (string-append path category "/")
            category)))
  
  (site "categories" categories))

(register-pre-all-plugin 'nested-categories add-nested-categories!)

; Inline plugin to render a nested navbar of first and second level categories in the bootstrap style
(define (generate-navbar)
  (define (make-category-link . levels)
    (string-join (list* (or (site-ref "url") "") "category" (map slug levels)) "/"))
  
  (define (sorted-string-hash-keys hash) (sort (filter string? (hash-keys hash)) string<?))
  
  `(ul ((class "nav navbar-nav"))
       ,@(for/list ([category (in-list (sorted-string-hash-keys categories))])
           `(li ((class "dropdown"))
                (a ((href ,(make-category-link category)) (class "dropdown-toggle")) ,category (span ((class "caret"))))
                (ul ((class "dropdown-menu"))
                    ,@(for/list ([sub-category (in-list (sorted-string-hash-keys (hash-ref categories category (hash))))])
                        `(li (a ((href ,(make-category-link category sub-category))) ,sub-category))))))))

(register-plugin 'nested-category-navbar generate-navbar)

; Add a function to generate a listing of all posts in a given (potentially nested) category
; Possible values for sort-by: 'date 'title 'shuffle
(define (generate-category-listing category #:sort-by [sort-by 'date] #:sort-ascending [sort-ascending? #t])
  (set! category (string-trim category "/"))
  
  (define parent-cateogry
    (let ([parts (string-split (string-trim category "/") "/")])
      (and (> (length parts) 1)
           (cadr (reverse parts)))))
  
  (define cathash 
    (for/fold ([h categories]) ([part (in-list (string-split category "/"))])
      (hash-ref h part (make-hash))))
  
  (define post-list (set->list (hash-ref cathash '*posts* (set))))
  
  (define sorted-post-list 
    ((if sort-ascending? identity reverse)
     (case sort-by
       [(date) 
        (parameterize ([date-display-format 'iso-8601])
          (sort post-list (λ (post1 post2) 
                            (string<? (~a (date->string (post1 "date"))) 
                                      (~a (date->string (post2 "date")))))))]
       [(title)
        (sort post-list (λ (post1 post2)
                          (string<? (~a (slug (post1 "title")))
                                    (~a (slug (post2 "title"))))))]
       [(shuffle)
        (sort post-list (λ (post1 post2) (zero? (random 2))))]
       [else post-list])))
  
  `(div
    ,@(if parent-cateogry
          `((h2 "Parent category")
            (ul (li (a ((href "..")) ,parent-cateogry))))
          `())
    ,@(if (> (hash-count cathash) 1)
          `((h2 "Subcategories")
            (ul ,@(for/list ([subcat (in-list (sort (filter string? (hash-keys cathash)) string<?))])
                    `(li (a ((href ,(slug subcat))) ,subcat)))))
          `())
    (h2 "Posts")
    (ul ,@(for/list ([post (in-list sorted-post-list)])
            `(li (a ((href ,(string-append (or (site-ref "url") "") "/" (post "permalink")))) ,(post "title")))))))

(register-plugin 'generate-category-listing generate-category-listing)

