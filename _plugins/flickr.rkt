(define (flickr-gallery id)
  `(div (span ,(format "TODO: flickr-gallery for ~a" id))))

(register-plugin 'flickr-gallery flickr-gallery)

(define (flickr-image width height id #:title title)
  `(div (span ,(format "TODO: flickr-image for ~a @ ~a" title id))))

(register-plugin 'flickr-image flickr-image)