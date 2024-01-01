;;; -*- mode:scheme; indent-tabs-mode:nil; coding:utf-8 -*-
#lang racket

;;;; Exercise 2.23: (Pg. 146)

(require test-engine/racket-tests)

(define (for-each f lst)
  (if (empty? lst) #t
	  (begin
		(f (car lst))
		(for-each f (cdr lst)))))
