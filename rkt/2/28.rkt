;;; -*- mode:scheme; indent-tabs-mode:nil; coding:utf-8 -*-
#lang racket

;;;; Exercise 2.18: (Pg. 140)

(require test-engine/racket-tests)

(define (fringe lst)
  (define (fringe-inner x y)
    (cond [(empty? x) y]
          [(pair? x) (append (fringe (car x)) (fringe (cdr x)))]
          [else (append (list x) y)]))
  (fringe-inner lst '()))
