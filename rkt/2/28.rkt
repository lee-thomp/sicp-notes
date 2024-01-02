;;; -*- mode:scheme; indent-tabs-mode:nil; coding:utf-8 -*-
#lang racket

;;;; Exercise 2.18: (Pg. 140)

(require test-engine/racket-tests)

;;; Mostly found via trial-and-error. :(
(define (fringe lst)
  (define (fringe-inner x result)
    (cond [(empty? x) result]
          [(not (pair? x)) (list x)]
          [else
           (fringe-inner
            (cdr x)                     ; Move to next item.
            (append result
                    ;; Take fringe of sub-list.
                    (fringe-inner (car x) '())))]))
  (fringe-inner lst '()))


(define x (list (list 1 2) (list 3 4)))

(check-expect (fringe x)
              '(1 2 3 4))

(check-expect (fringe (list x x))
              '(1 2 3 4 1 2 3 4))

(test)
