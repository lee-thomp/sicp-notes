;;; -*- mode:scheme; indent-tabs-mode:nil; coding:utf-8 -*-
#lang racket

;;;; Exercise 2.18: (Pg. 140)

(require test-engine/racket-tests)

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

;;; The below operations are really just wrappers for list manipulation
;;; primitives.
(define first-denomination car)
(define except-first-denomination cdr)
(define no-more? empty?)

(define (cc amount coin-values)
  (cond [(zero? amount) 1]
        [(or (< amount 0) (no-more? coin-values)) 0]
        [else (+ (cc amount (except-first-denomination coin-values))
                 (cc (- amount (first-denomination coin-values)) coin-values))]))

;;; Taken from book example.
(check-expect (cc 100 us-coins)
              292)

(check-expect (cc 20 uk-coins)
              293)

(test)
