(define (published-on d)
  (xexpr->string `(time ((class "entry-date") (datetime ,(parameterize ([date-display-format 'iso-8601]) (date->string d))))
                        (span ((class "year"))  ,(~a (date-year d)))
                        " "
                        (span ((class "month")) ,(vector-ref '#(#f "Jan" "Feb" "Mar" "Apr" "May" "June" "July" "Aug" "Sept" "Oct" "Nov" "Dec") (date-month d)))
                        " "
                        (span ((class "day"))   ,(~a (date-day d))))))

(register-plugin 'published-on published-on)