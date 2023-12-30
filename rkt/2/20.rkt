;;; -*- mode:scheme; indent-tabs-mode:nil; coding:utf-8 -*-
#lang racket

;;;; Exercise 2.19: (Pg. 142)

(require test-engine/racket-tests)

;;; Input args could be destructured using something like `x . xs' but then x
;;; would need to be consed back on at the end.
(define (same-parity . xs)
  (filter (if (odd? (car xs)) odd? even?)
          xs))

(check-expect (same-parity 1 2 3 4 5 6 7)
              '(1 3 5 7))

(check-expect (same-parity 2 3 4 5 6 7)
              '(2 4 6))

(test)
