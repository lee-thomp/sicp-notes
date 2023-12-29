;;; -*- mode:scheme; indent-tabs-mode:nil; coding:utf-8 -*-
#lang racket

;;;; Exercise 2.6: (Pg. 126)

(require test-engine/racket-tests)

;;; The zero numeral discards the applied function and just returns the `X' arg.
(define zero
  (λ (f)
    (λ (x) x)))

;;; SICP calls this `ADD-1', lambda calculus calls it `succ' for successor.
(define (succ n)
  (λ (f)
    (λ (x) (f ((n f) x)))))

;;; Church numerals express numbers as repeated function
;;; application/composition.
(define one
  (λ (f)
    (λ (x) (f x))))

(define two
  (λ (f)
    (λ (x) (f (f x)))))

;;; Add two church numerals: addition is accomplished by applying the function
;;; `F' `B' times to `X', then `A' times to the result.
(define (add-cn a b)
  (λ (f)
    (λ (x) ((a f) ((b f) x)))))


;;; Turns a ‘church numeral’ back into a natural number by using it to
;;; repeatedly increment 0.
(define (natify church-numeral)
  ((church-numeral add1) 0))

(check-expect (natify zero)
              0)

(check-expect (natify (succ zero))
              1)

(check-expect (natify (succ (succ zero)))
              2)

(check-expect (natify one)
              1)

(check-expect (natify two)
              2)

(check-expect (natify (add-cn one one))
              2)

(check-expect (natify (add-cn one two))
              3)

(check-expect (natify (add-cn two two))
              4)

;;; Run all above tests.
(test)
