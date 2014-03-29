(define (photosynth id)
  `(div (span (format "TODO: photosynth for ~a" ,id))))

(register-plugin 'photosynth photosynth)