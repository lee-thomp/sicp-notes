;;; -*- mode:scheme; indent-tabs-mode:nil; coding:utf-8 -*-
#lang racket

;;;; Exercise 2.4: (Pg. 125)

;;; Constructs a closure wrapping `X' and `Y' inside a function. Applying some
;;; selector function will 'select' either x or y.
(define (cons’ x y)
  (λ (m) (m x y)))

;;; >  (car’ (cons’ 'a 'b))
;;; β: (car’ (λ (m) (m 'a 'b)))
;;; β: ((λ (m) (m 'a 'b)) (λ (p q) p))
;;; β: ((λ (p q) p) 'a 'b)
;;; β: 'a
(define (car’ z)
  (z (λ (p q) p)))

;;; Similar to above.
(define (cdr’ z)
  (z (λ (p q) q)))

(display (car’ (cons’ 'x 'y)))


;;; The above is identical to the pure lambda calculus encoding of pairs:

;;; Use like so:
;;; `(cons 'a 'b)' => `((pair 'a) 'b)'
(define pair
  (λ (x)
    (λ (y)
      (λ (f) ((f x) y)))))

(define true
  ;; Alternatively the K combinator.
  (λ (x)
    (λ (y) x)))

(define false
  ;; Alternatively the combinator KI.
  (λ (x)
    (λ (y) y)))

(define first
  (λ (p)
    (p true)))

(define second
  (λ (p)
    (p false)))
