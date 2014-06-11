
(define my-footnotes '())

(define (footnote . line*)
  (set! my-footnotes (cons line* my-footnotes))
  `(span ((class "footnote")) (sup (a ((href ,(format "#footnote-~a" (length my-footnotes))))
                                      ,(format "[~a]" (length my-footnotes))))))

(define (clear-footnotes! post site)
  (set! my-footnotes '()))

(define (add-footnotes! post site)
  (post "footnotes"
        `(div ((id "footnotes"))
              (ol
               ,@(for/list ([i (in-naturals 1)]
                            [footnote (in-list (reverse my-footnotes))])
                   `(li (a ((name ,(format "footnote-~a" i))) "") ,@footnote))))))

(register-plugin 'footnote footnote)
(register-pre-render-plugin 'clear-footnote clear-footnotes!)
(register-post-render-plugin 'add-footnotes add-footnotes!)