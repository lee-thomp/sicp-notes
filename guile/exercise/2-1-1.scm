(define-module (exercise 2-1-1)
  #:export (make-rat/better))

;;; Exercise 2.1
(define (make-rat/better n d)
  (let ((g (gcd n d)))
    (if (positive? d)
        (cons (/ n g) (/ d g))
        (cons (/ n g -1)
              (/ d g -1)))))
