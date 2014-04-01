
(define my-footnotes '())

(define (footnote . line*)
  (set! my-footnotes (cons line* my-footnotes))
  `(div (span ,(format "[~a]" (length my-footnotes)))))

(define (clear-footnotes! _)
  (set! my-footnotes '()))

(define (add-footnotes! post)
  (when (not (null? my-footnotes))
    (post "content" (string-append (post "content")
                                   (render
                                    `(div ((id "footnotes"))
                                          ,@(for/list ([i (in-naturals 1)]
                                                       [footnote (in-list my-footnotes)])
                                              `(span ,(format "[~a]" i) ,footnote))))))))

(register-plugin 'footnote footnote #:pre-render clear-footnotes! #:post-render add-footnotes!)