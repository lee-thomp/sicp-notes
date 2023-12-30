;;; -*- mode:scheme; indent-tabs-mode:nil; coding:utf-8 -*-
#lang racket

;;;; Exercise 2.21: (Pg. 144)

(require test-engine/racket-tests)

(define (square x)
  (* x x))

(define (square-list items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list’ items)
  (map square items))

(check-expect (square-list (list 1 2 3 4))
              '(1 4 9 16))

(check-expect (square-list’ (list 1 2 3 4))
              '(1 4 9 16))

(test)
