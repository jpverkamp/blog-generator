(define (flickr-gallery id)
  `(div ((class "flickr-gallery")
         (data-set-id ,(~a id))
         (data-per-page "30"))))

(register-plugin 'flickr-gallery flickr-gallery)

(define (flickr-image width height id #:title title)
  `(div (span ,(format "TODO: flickr-image for ~a @ ~a" title id))))

(register-plugin 'flickr-image flickr-image)