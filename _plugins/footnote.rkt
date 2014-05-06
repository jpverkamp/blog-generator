
(define my-footnotes '())

(define (footnote . line*)
  (set! my-footnotes (cons line* my-footnotes))
  `(span ((class "footnote")) ,(format "~a" (length my-footnotes))))

(define (clear-footnotes! post site)
  (set! my-footnotes '()))

(define (add-footnotes! post site)
  (post "footnotes"
        `(div ((id "footnotes"))
              (ol
               ,@(for/list ([footnote (in-list (reverse my-footnotes))])
                   `(li ,@footnote))))))

(register-plugin 'footnote footnote)
(register-pre-render-plugin clear-footnotes!)
(register-post-render-plugin add-footnotes!)