#lang racket

(define (random-char)
  (define chars "ABCDEFGHIJKLMNOPQRSTUVWXYZ ")
  (string-ref chars (random (string-length chars))))

(define (random-string size)
  (list->string 
   (for/list ([i (in-range size)]) 
     (random-char))))

(define (mutate-string str #:mutation-rate [mutation-rate 0.05])
  (list->string
   (for/list ([c (in-string str)])
     (if (< (random) mutation-rate) (random-char) c))))

(define (string-diff str1 str2)
  (for/sum ([c1 (in-string str1)] [c2 (in-string str2)])
    (if (eq? c1 c2) 1 0)))

(define (evolve target #:pool-size [pool-size 100] #:mutation-rate [mutation-rate 0.05])
  ; 1) Start with a random string of 28 characters.
  (let loop ([generation 0] [best (random-string (string-length target))])
    (displayln (list generation best))
    (cond
      ; 5a) If any of the new strings has a perfect score (28), halt. 
      [(zero? (string-diff best target)) generation]
      ; 5b) Otherwise, take the highest scoring string, and go to step 2.
      [else
       (loop
        (+ generation 1)
        (first (sort 
                ; 2) Make 100 copies of the string (reproduce).
                (for/list ([i (in-range pool-size)])
                  ; 3) For each character in each of the 100 copies, with a probability of 5%, 
                  ; replace (mutate) the character with a new random character.
                  (mutate-string best #:mutation-rate mutation-rate))
                ; 4) Compare each new string with the target string “METHINKS IT IS LIKE A WEASEL”, 
                ; and give each a score (the number of letters in the string that are correct and in the correct position).
                (λ (str1 str2)
                  (< (string-diff str1 target) 
                     (string-diff str2 target))))))])))
      
      




