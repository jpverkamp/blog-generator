(define (generate-previous-next post site)
  (cond 
    [(post "categories")
     ; Get top level categories for this post
     (define categories 
       (sort (set->list (for/set ([category (in-list (post "categories"))])
                          (first (string-split category "/"))))
             string<?))
     
     ; Get a sorted (by date) list of posts from a category
     (define (category-posts category)
       (filter (λ (post) (ormap (λ (post-category) (equal? category (first (string-split post-category "/"))))
                                (or (post "categories") '())))
               (site "posts")))
     
     ; Get the previous/next posts in a category
     (define (previous-post category)
       (for/first ([prev (in-list (category-posts category))]
                   [this (in-list (cdr (category-posts category)))]
                   #:when (eq? post this))
         prev))
     
     (define (next-post category)
       (for/first ([this (in-list (cons #f (category-posts category)))]
                   [next (in-list (category-posts category))]
                   #:when (eq? post this))
         next))
     
     ; Build bootstrap navbars
     (xexpr->string
      `(nav ((class "navbar navbar-default") (role "navigation"))
            ,@(for/list ([category (in-list categories)])
                `(ul ((class "nav navbar-nav nav-justified"))
                     ,@(cond
                        [(previous-post category) => (λ (prev) `((li (a ((href ,(string-append (or (site "url") "") "/" (prev "permalink")))) "← " ,(prev "title")))))]
                        [else '()])
                     ,(if (equal? category "")
                          `(li (a ((href ,(string-append (or (site "url") "") "/category/archives"))) "All Posts"))
                          `(li (a ((href ,(string-append (or (site "url") "") "/category/" (slug category)))) ,category)))
                     ,@(cond
                        [(next-post category) => (λ (next) `((li (a ((href ,(string-append (or (site "url") "") "/" (next "permalink")))) ,(next "title") " →"))))]
                        [else '()])))))]
    
    ; No categories, probably a category listing or the like
    [else ""]))

(register-plugin 'generate-previous-next generate-previous-next)
                
                