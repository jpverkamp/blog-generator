(define site-ref #f)
(define categories #f)

(define (snoc x ls) (reverse (cons x (reverse ls))))

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
      (new-post "content" "
@subcategories{@post{category}}

<h2>Posts</h2>
@post-list['include-post 5]{@post{category}}")
      
      (new-post "category" path)
      (site "posts" (snoc new-post (site "posts"))))
    
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

; List the parent category and all subcategories for a given (potentially nested) cateogry
(define (subcategories [category ""])
  (set! category (string-trim category "/"))
  
  (define parent-cateogry
    (let ([parts (string-split (string-trim category "/") "/")])
      (and (> (length parts) 1)
           (cadr (reverse parts)))))
  
  ; Get the posts for the category we're talking about
  (define cathash 
    (for/fold ([h categories]) ([part (in-list (string-split category "/"))])
      (hash-ref h part (make-hash))))
  (define post-list (set->list (hash-ref cathash '*posts* (set))))
  
  `(ul ((class "list-inline"))
       ,@(if parent-cateogry
             `((li "[↑ " (a ((href "..")) ,parent-cateogry) "]"))
             `())
       ,@(if (> (hash-count cathash) 1)
             (for/list ([subcat (in-list (sort (filter string? (hash-keys cathash)) string<?))])
               `(li "[" (a ((href ,(slug subcat))) ,subcat) "]"))
             `())))

(register-plugin 'subcategories subcategories)

; Generate a listing of all posts in a given (potentially nested) category
; args:
; 'limit {number} - return only this many posts
; 'sort-by {date, title, shuffle} - sort the returned posts (before applying limit)
; 'sort-(ascending/descending) - sort by key ascending, ignored in shuffle
; 'include-post - include the post along with the content
(define (post-list #:limit                 [limit #f]
                   #:sort-by               [sort-by 'date] 
                   #:sort-ascending        [sort-ascending? #f] 
                   #:include-post          [include-post #f]
                   . args)
  (define category 
    (let loop ([args args])
      (match args
        [`(limit ,n . ,rest)        (set! limit n)            (loop rest)]
        [`(sort-by ,how . ,rest)    (set! sort-by how)        (loop rest)]
        [`(sort-ascending . ,rest)  (set! sort-ascending? #t) (loop rest)]
        [`(sort-descending . ,rest) (set! sort-ascending? #f) (loop rest)]
        [`(include-post 
           ,(? number? n) . ,rest)  (set! include-post n)     (loop rest)]
        [`(include-post . ,rest)    (set! include-post #t)    (loop rest)]
        [`(,category)               category]
        [`()                        ""])))
  
  (set! category (string-trim category "/"))
  
  (define parent-cateogry
    (let ([parts (string-split (string-trim category "/") "/")])
      (and (> (length parts) 1)
           (cadr (reverse parts)))))
  
  ; Get the posts for the category we're talking about
  (define cathash 
    (for/fold ([h categories]) ([part (in-list (string-split category "/"))])
      (hash-ref h part (make-hash))))
  (define post-list (set->list (hash-ref cathash '*posts* (set))))
  
  ; Sort the posts, by date, title, random, or default (which should also be date)
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
  
  ; Apply the limit (if it exists); make sure that if we have fewer than that we return everything
  (define final-post-list
    (if limit
        (take sorted-post-list (min (length sorted-post-list) limit))
        sorted-post-list))
  
  ; Posts within this category or subcategories
  (parameterize ([date-display-format 'iso-8601])
    `(ul ((class "list-unstyled"))
         ,@(for/list ([post (in-list final-post-list)]
                      [i (in-naturals)])
             `(li 
               ,@(cond
                   [(or (eq? include-post #t)
                        (and (number? include-post)
                             (< i include-post)))
                    `((h1 ((class "entry-title")) (a ((href ,(string-append (or (site-ref "url") "") "/" (post "permalink")))) ,(post "title")))
                      (span ((class "posted-on")) "[" ,(date->string (post "date")) "]")
                      (div ,(post "more"))
                      (hr))]
                   [else
                    `((span ((class "posted-on")) "[" ,(date->string (post "date")) "] ")
                      (a ((href ,(string-append (or (site-ref "url") "") "/" (post "permalink")))) ,(post "title")))]))))))

(register-plugin 'post-list post-list)

