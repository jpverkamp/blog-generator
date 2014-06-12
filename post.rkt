#lang at-exp rackjure

(provide read-post
         empty-post)

(require yaml)

(struct post (data)
  #:transparent
  #:mutable
  #:property prop:procedure
  (let ()
    (define (<post> post key [val (void)] #:default [def #f] #:delete [delete? #f])
      (when (symbol? key) (set! key (symbol->string key)))
      (cond
        [delete?     (hash-remove! (post-data post) key)]
        [(void? val) (hash-ref     (post-data post) key def)]
        [else        (hash-set!    (post-data post) key val)]))
    <post>))

(define (empty-post) (post (make-hash)))

(define (read-post [in (current-input-port)])
  (match (string-split (port->string in) "---")
    [(or `(,head ,body)
         `(_ ,head ,body))
     (define data (string->yaml head))
     (hash-set! data "content" body)
     (post data)]
    [`(,head . ,body*)
     (define data (string->yaml head))
     (hash-set! data "content" (string-join body* "---"))
     (post data)]))
