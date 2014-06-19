(define (generate-atom site #:posts [posts #f])
  (when (not posts) (set! posts (site "posts")))

  `(feed ((xmlns "http://www.w3.org/2005/Atom"))
         (title "jverkamp.com")
         (link ,(site "url"))
         (updated "TODO: get the latest post")
         (author (name "JP Verkamp"))
         (id ,(format "tag:~a:~a" (site "url") "/"))
         
         ,@(for/list ([post (in-list posts)])
             `(entry
               (title ,(post "title"))
               (link ((href ,(string-append (site "url") (post "permalink")))))
               (id "TODO: make unique post ids")
               (updated "TODO: get the post update in the correct format")
               (summary "TODO: put pre more here")
               (content ((type "text/html")) (post "content"))))))