;;; -*- mode:scheme; indent-tabs-mode:nil; coding:utf-8 -*-
#lang racket

;;;; Exercise 2.18: (Pg. 140)

(require test-engine/racket-tests)

;;; Reverse a list by consing elements from the front onto an empty list.
(define (reverse’ lst)
  (define (reverse-inner x y)
    (if (empty? x) y
        (reverse-inner (cdr x) (cons (car x) y))))
  (reverse-inner lst '()))

(check-expect (reverse’ (list 1 4 9 16 25))
              '(25 16 9 4 1))

(test)
