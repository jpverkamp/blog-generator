(define (photosynth id)
  `(iframe ((src ,(format "http://photosynth.net/embed.aspx?cid=~a&amp;delayLoad=true&amp;slideShowPlaying=false" id))
            (width "500")
            (height "300")
            (scrolling "no")
            (class "iframe-class")
            (frameborder "0"))))

(register-plugin 'photosynth photosynth)