(define (format-date date)
  (define (~pad thing digits) (~a thing #:width digits #:align 'right #:pad-string "0"))
  (~a (~pad (date-year date)   4) "-" 
      (~pad (date-month date)  2) "-" 
      (~pad (date-day date)    2) "T"
      (~pad (date-hour date)   2) ":"
      (~pad (date-minute date) 2) ":"
      "00"
      "Z"))

(define md5-as-bytes (dynamic-require 'file/md5 'md5))
(define (uuid str) 
  (define hash (bytes->string/utf-8 (md5-as-bytes str #t)))
  (~a "urn:uuid:"
      (substring hash 0  8)  "-"
      (substring hash 8  12) "-"
      (substring hash 12 16) "-"
      (substring hash 16 20) "-"
      (substring hash 20 32)))

(define (starts-with? haystack needle)
  (and (>= (string-length haystack) (string-length needle))
       (equal? needle (substring haystack 0 (string-length needle)))))

(define (generate-atom [filter-category ""])
  (set! filter-category (string-trim filter-category "/"))
  
  ; Generate a list of posts
  (define posts
    (for/list ([post (in-list (site "posts"))]
               #:when (for/first ([post-category (in-list (or (post "categories") '()))]
                                  #:when (starts-with? post-category filter-category))
                        #t))
      post))
  
  ; Sort the posts by date
  (set! posts (sort posts (λ (post1 post2)
                          (< (if (post1 "date") (date->seconds (post1 "date")) 0)
                             (if (post1 "date") (date->seconds (post1 "date")) 0)))))
  
  ; Limit the number of posts, if specified in config
  (when (and (site "atom-posts")
             (> (length posts) (site "atom-posts")))
    (set! posts (take posts (site "atom-posts"))))

  ; Generate the feed xml
  `(feed ((xmlns "http://www.w3.org/2005/Atom"))
         (title "jverkamp.com")
         (link ((href ,(site "url"))))
         (link ((rel "self") (href ,(~a (site "url") "/feed/"))))
         (updated ,(format-date ((car posts) "date")))
         (author (name "JP Verkamp"))
         (id ,(uuid (~a (site "url") filter-category)))
         
         ,@(for/list ([post (in-list posts)])
             `(entry
               (title ,(post "title"))
               (link ((href ,(string-append (site "url") (post "permalink")))))
               (id ,(uuid (~a (site "url") "/" (post "permalink"))))
               (updated ,(format-date (post "date")))
               (summary ((type "html")) ,(~a "<![CDATA[" (or (post "more") (post "content")) "]]>"))
               (content ((type "html")) ,(~a "<![CDATA[" (post "content") "]]>"))))))

(register-plugin 'generate-atom generate-atom)