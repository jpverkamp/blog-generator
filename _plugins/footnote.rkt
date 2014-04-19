
(define my-footnotes '())

(define (footnote . line*)
  (set! my-footnotes (cons line* my-footnotes))
  `(span ((class "footnote")) ,(format "~a" (length my-footnotes))))

(define (clear-footnotes! _)
  (set! my-footnotes '()))

(define (add-footnotes! post)
  (when (not (null? my-footnotes))
    (post "content" 
          (string-append
           (post "content")
           (render 
            `(div ((id "footnotes"))
                  (ol
                   ,@(for/list ([footnote (in-list my-footnotes)])
                       `(li ,(format "~a" (apply string-append footnote))))))
            #:markdown? #f)))))

(register-plugin 'footnote footnote)
(register-pre-render-plugin clear-footnotes!)
(register-post-render-plugin add-footnotes!)