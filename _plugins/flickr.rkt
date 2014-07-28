(define (flickr-gallery id)
  `(div ((class "flickr-gallery")
         (data-set-id ,(~a id))
         (data-per-page "30"))))

(register-plugin 'flickr-gallery flickr-gallery)
