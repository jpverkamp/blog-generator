(define (add-tags site)
  ; Add tags for primary and last posts
  (for ([post (in-list (site "posts"))] #:when (post "categories"))
    (for ([category (in-list (post "categories"))])
      ; Don't generate tags for archive links
      (when (not (regexp-match #px"^Archive" category))
        ; Top level tags
        (define tl-tag (last (string-split category "/")))
        (define tl-tag-permalink (string-join (cons "category" (map slug (string-split category "/"))) "/"))
        (post "tags" (hash-set (or (post "tags") (hash)) tl-tag tl-tag-permalink))
  
        ; Add a tag link for the last level of the category to the category permalink
        (define ll-tag (first (string-split category "/")))
        (define ll-tag-permalink (string-join (list "category" (slug (first (string-split category "/")))) "/"))
        (post "tags" (hash-set (or (post "tags") (hash)) ll-tag ll-tag-permalink))))))

(register-pre-all-plugin 'add-tags add-tags #:after 'nested-categories)

; Generate a list of tags
(define (tag-list tags)
  (xexpr->string
   (match tags
     [#f ""]
     [(? hash?)
      `(ul ((class "tag-list list-inline"))
           ,@(for/list ([(tag permalink) (in-hash tags)])
               `(li (a ((href ,(string-append (or (site "url") "") "/" permalink))) ,tag))))]
     [(? list?)
      `(ul ((class "tag-list list-inline"))
           ,@(for/list ([tag (in-list tags)])
               `(li (a ((href ,(string-append (or (site "url") "") "/tag/" (slug tag)))) ,tag))))])))

(register-plugin 'tag-list tag-list)  
