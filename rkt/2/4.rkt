;;; -*- mode:scheme; indent-tabs-mode:nil; coding:utf-8 -*-
#lang racket

;;; Exercise 2.4: (Pg. 125)
(define (cons’ x y)
  (λ (m) (m x y)))

(define (car’ z)
  (z (λ (p q) p)))

(define (cdr’ z)
  (z (λ (p q) q)))

(display (car’ (cons’ 'x 'y)))
