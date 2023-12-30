;;; -*- mode:scheme; indent-tabs-mode:nil; coding:utf-8 -*-
#lang racket

;;;; Exercise 2.18: (Pg. 140)

(require test-engine/racket-tests)

(define (reverse lst)
  (define (reverse-inner x y)
    (if (empty? x) y
        (reverse-inner (cdr x) (cons (car x) y))))
  (reverse-inner lst '()))
