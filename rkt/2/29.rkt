;;; -*- mode:scheme; indent-tabs-mode:nil; coding:utf-8 -*-
#lang racket

;;;; Exercise 2.29: (Pg. 152)

(require test-engine/racket-tests)

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

;;; Part a.
(define left-branch
  car)

(define right-branch
  cadr)

(define branch-length
  car)

(define branch-structure
   cadr)
