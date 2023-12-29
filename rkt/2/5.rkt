;;; -*- mode:scheme; indent-tabs-mode:nil; coding:utf-8 -*-
#lang racket

;;;; Exercise 2.5: (Pg. 125)

(require test-engine/racket-tests)

;;; Encode a pair of ints as 2ᴬ3ᴮ.
(define (cons’ a b)
  (* (expt 2 a) (expt 3 b)))

;;; Get the car of a pair by factoring out 2s.
(define (car’ pair)
  (letrec ([car-inner
            (λ (a b)
              ;; If number can't be neatly factored, all 2s have been removed.
              (if (zero? (modulo b 2))
                  (car-inner (add1 a) (/ b 2))
                  a))])
    (car-inner 0 pair)))

(define (cdr’ pair)
  (letrec ([cdr-inner
            (λ (a b)
              (if (zero? (modulo b 3))
                  (cdr-inner (add1 a) (/ b 3))
                  a))])
    (cdr-inner 0 pair)))

;;; Tests
(define ns (make-base-namespace))

(check-expect (cons’ 5 7)
              69984)

(check-expect (car’ (cons’ 8 6))
              8)

(check-expect (cdr’ (cons’ 13 7))
              7)

(test)
