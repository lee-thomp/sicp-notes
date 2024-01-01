;;; -*- mode:scheme; indent-tabs-mode:nil; coding:utf-8 -*-
#lang racket

;;;; Exercise 2.24: (Pg. 149)

(require test-engine/racket-tests)

(check-expect (car (cdr (car (cdr (cdr '(1 3 (5 7) 9))))))
              7)

(check-expect (car (car '((7))))
              7)

(check-expect (car (cdr
                    (car (cdr
                          (car (cdr
                                (car (cdr
                                      (car (cdr
                                            (car (cdr
                                                  '(1 (2 (3 (4 (5 (6 7))))))))))))))))))
              7)

(test)
