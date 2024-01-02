;;; -*- mode:scheme; indent-tabs-mode:nil; coding:utf-8 -*-
#lang racket

;;;; Exercise 2.18: (Pg. 140)

(require test-engine/racket-tests)

(define (fringe lst)
  (cond [(empty? lst) '()]
        [(not (pair? lst)) (list lst)]
        [else (append (fringe (car lst)) (fringe (cdr lst)))]))


(define x (list (list 1 2) (list 3 4)))

(check-expect (fringe x)
              '(1 2 3 4))

(check-expect (fringe (list x x))
              '(1 2 3 4 1 2 3 4))

(test)
