(include "_plugins/published-on.rkt")
(include "_plugins/tag-list.rkt")

(define categories #f)

(define (snoc x ls) (reverse (cons x (reverse ls))))

; Add categories to each post, assuming they are nested with / delims
(define (add-nested-categories! site)
  (set! categories (make-hash))
  
  ; Create a nested category hash with a key *posts* at each level listing all posts
  (for ([post (in-list (site "posts"))] #:when (post "categories"))
    (for ([category (in-list (post "categories"))])
      (define last-hash
        (for*/fold ([h categories]) ([part (in-list (string-split category "/"))])
          (hash-set! h '*posts* (set-add (hash-ref h '*posts* (set)) post))
          (hash-ref! h part (make-hash))))
      (hash-set! last-hash '*posts* (set-add (hash-ref last-hash '*posts* (set)) post))))
  
  ; Generate a page for each subcategory along with an atom feed for each
  (let loop ([h categories] [path "/"] [name #f])
    (when name
      ; Basic post
      (define new-post (empty-post))
      (new-post "title" name)
      #;(new-post "do-not-cache" #t)
      (new-post "permalink" (string-join (cons "category" (map slug (string-split path "/"))) "/"))
      (new-post "template" "post")
      (new-post "content" "
@subcategories{@post{category}}

<h2>Posts</h2>
@post-list['include-post 5]{@post{category}}")
      
      (new-post "category" path)
      (site "posts" (snoc new-post (site "posts")))
      
      ; Atom feed
      (define atom-post (empty-post))
      (atom-post "title" (~a name " Feed"))
      #;(atom-post "do-not-cache" #t)
      (atom-post "template" "_blank")
      (atom-post "permalink" (~a (string-join (cons "category" (map slug (string-split path "/"))) "/") "/feed"))
      (atom-post "permalink-filename" "atom.xml")
      (atom-post "content" "<?xml version=\"1.0\" encoding=\"utf-8\"?>
@generate-atom[@post{category}]")
      (atom-post "category" path)
      (site "posts" (snoc atom-post (site "posts"))))
    
    (for ([category (in-list (hash-keys h))] #:when (string? category))
      (loop (hash-ref h category (hash))
            (string-append path category "/")
            category)))
  
  (site "categories" categories))

(register-pre-all-plugin 'nested-categories add-nested-categories!)

; Inline plugin to render a nested navbar of first and second level categories in the bootstrap style
(define (generate-navbar)
  (define (make-category-link . levels)
    (string-join (list* (or (site "url") "") "category" (map slug levels)) "/"))
  
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
    (define last-date-chunk #f)
    `(ul ((class "list-unstyled"))
         ,@(for/list ([post (in-list final-post-list)]
                      [i (in-naturals)])
             (cond
               [(or (eq? include-post #t)
                    (and (number? include-post)
                         (< i include-post)))
                `(li ((class "post-preview"))
                     (header
                      (h2 ((class "entry-title")) (a ((href ,(string-append (or (site "url") "") "/" (post "permalink")))) ,(post "title")))
                      (div ((class "entry-meta"))
                           ,(published-on (post "date"))))
                     (div ((class "preview")) ,(post "more"))
                     (hr))]
               [else
                (define date-chunk (and (post "date") (format "~a ~a" (vector-ref '#(#f "Jan" "Feb" "Mar" "Apr" "May" "June" "July" "Aug" "Sept" "Oct" "Nov" "Dec") (date-month (post "date"))) (date-year (post "date")))))
                `(li ((class "post-listing"))
                     ,@(cond
                         [(or (not date-chunk) (equal? last-date-chunk date-chunk)) `()]
                         [else
                          (set! last-date-chunk date-chunk)
                          `((h3 ,date-chunk))])
                     (a ((href ,(string-append (or (site "url") "") "/" (post "permalink")))) ,(post "title")))])))))

(register-plugin 'post-list post-list)

