(define-module (tests 2-1-1)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 format)
  #:use-module (exercise 2-1-1))

;;; Given:
(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (string-rat x)
  (format #f "~d/~d" (numer x) (denom x)))

(test-begin "Section 2.1.1")

(test-equal "1/2" (string-rat (make-rat 1 2)))
(test-equal "-1/2" (string-rat (make-rat -1 2)))

(test-expect-fail 2)
(test-equal "-1/2" (string-rat (make-rat 1 -2)))
(test-equal "1/2" (string-rat (make-rat -1 -2)))

(test-equal "1/2" (string-rat (make-rat/better 1 2)))
(test-equal "-1/2" (string-rat (make-rat/better -1 2)))
(test-equal "-1/2" (string-rat (make-rat/better 1 -2)))
(test-equal "1/2" (string-rat (make-rat/better -1 -2)))

(test-end "Section 2.1.1")
