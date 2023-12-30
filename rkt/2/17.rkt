;;; -*- mode:scheme; indent-tabs-mode:nil; coding:utf-8 -*-
#lang racket

;;;; Exercise 2.17: (Pg. 139)

(require test-engine/racket-tests)

(define (last-pair lst)
  ;; `CDR' of a list containing a single element is the empty list.
  (let ([next (cdr lst)])
    (if (empty? next) lst
        (last-pair (cdr lst)))))

;;; Pulled straight from the book.
(check-expect (last-pair (list 23 72 149 34))
              '(34))

(test)
