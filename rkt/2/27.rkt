;;; -*- mode:scheme; indent-tabs-mode:nil; coding:utf-8 -*-
#lang racket

;;;; Exercise 2.18: (Pg. 140)

(require test-engine/racket-tests)

(define (deep-reverse lst)
  (define (deep-reverse-inner x y)
    (cond [(empty? x) y]
          ;; If sublist is actually an atom, return it.
          [(not (pair? x)) x]
          ;; Prepend (deep-reversed) cars, move to cdrs.
          [else
           (deep-reverse-inner (cdr x) (cons (deep-reverse-inner (car x) '()) y))]))
  (deep-reverse-inner lst '()))

;;; Test pulled from 2.18:
(check-expect (deep-reverse (list 1 4 9 16 25))
              '(25 16 9 4 1))

;;; Loosely taken from exercise.
(check-expect (deep-reverse '((1 2) (3 4)))
              '((4 3) (2 1)))

(test)
