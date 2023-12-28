;;; -*- mode:scheme; indent-tabs-mode:nil; coding:utf-8 -*-
#lang racket

;;;; Notes and exercises for Structure and Interpretation of Computer Programs.


;;; Tree-recursive fibonacci procedure:
(define (fib-tree n)
  (cond [(= n 0) 0]
        [(= n 1) 1]
        [else (+ (fib-tree (- n 1))
                 (fib-tree (- n 2)))]))

;;; Iterative-recursive fibonacci:
(define (fib-iter n)
  (letrec ([fib-iter-inner
            (λ (a b m)
              (cond [(= m 0) a]
                    [else (fib-iter-inner b (+ a b) (- m 1))]))])
    (fib-iter-inner 0 1 n)))


;;; Exercise 1.11, tree-recursive:
(define (f-tree n)
  (cond [(< n 3) n]
        [else (+ (f-tree (- n 1))
                 (* 2 (f-tree (- n 2)))
                 (* 3 (f-tree (- n 3))))]))

;;; Iterative-recursive:
(define (f-iter n)
  (letrec ([f-iter-inner
            (λ (a b c m)
              (cond [(= m 0) a]
                    [else (f-iter-inner b c (+ c
                                               (* 2 b)
                                               (* 3 a))
                                        (- m 1))]))])
    (f-iter-inner 0 1 2 n)))

;;; Explanation:
;;; f 0 = 0
;;; f 1 = 1
;;; f 2 = 2
;;; f 3 = f 2 + 2 (f 1) + 3 (f 0) = 2 + 2 + 0 = 4
;;; f 4 = f 3 + 2 (f 2) + 3 (f 1) = 4 + 4 + 3 = 11


;;; Exercise 1.12:
;;; This procedure generates a single element in pascal’s triangle, the `K'th
;;; element of row `N'.
(define (pascal n k)
  (cond [(= n k 0) 1]
        [(= n 0) 0]
        [(= k 0) 1]
        [(= n k) 1]
        [else (+ (pascal (sub1 n) (sub1 k)) (pascal (sub1 n) k))]))

;;; Generates a whole row of pascal’s triangle as a list.
(define (pascal-row n)
  (letrec ([pascal-row-inner
            (λ (m k)
              (cond [(< m k) '()]
                    [else (cons (pascal m k)
                                (pascal-row-inner m (add1 k)))]))])
    (pascal-row-inner n 0)))

;;; Creates pascal’s triangle up to the `N'th row as a list of lists.
(define (pascal-triangle n)
  (map pascal-row (range n)))


;;; Exercise 1.13:
(define (fib-by-φ n)
  (let ([φ (/ (+ 1 (sqrt 5)) 2)]
        [ψ (/ (- 1 (sqrt 5)) 2)])
    (/ (expt φ n) (sqrt 5))))

(define (fib-by-φ-ψ n)
  (let ([φ (/ (+ 1 (sqrt 5)) 2)]
        [ψ (/ (- 1 (sqrt 5)) 2)])
    (/ (- (expt φ n) (expt ψ n)) (sqrt 5))))


;;; Exercise 1.16:
(define (fast-expt-2 b n)
  (letrec ([fast-expt-inner
            (λ (c m a)
              (cond
               [(= m 0) a]
               [(even? m) (fast-expt-inner (sqr c) (/ m 2) a)]
               [(odd? m) (fast-expt-inner c (- m 1) (* c a))]
               [else (error "Something has gone wrong!")]))])
    (fast-expt-inner b n 1)))


;;; Exercise 1.17:
(define (fast-mult a b)
  (letrec ([double (λ (x) (arithmetic-shift x 1))]
           [halve  (λ (x) (arithmetic-shift x -1))]
           [fast-mult-inner
            (λ (c d)
              (cond [(= d 1) c]
                    [(= c 1) d]
                    [(even? c) (double (fast-mult-inner (halve c) d))]
                    [(even? d) (double (fast-mult-inner c (halve d)))]
                    [else (+ c (fast-mult-inner c (- d 1)))]))])
    (fast-mult-inner a b)))

;;; Exercise 1.18:
(define (fast-mult-iter a b)
  (letrec ([double (λ (x) (arithmetic-shift x 1))]
           [halve  (λ (x) (arithmetic-shift x -1))]
           [fast-mult-inner
            (λ (c d acc)
              (cond [(= d 0) acc]
                    [(even? d) (fast-mult-inner (double c) (halve d) acc)]
                    [(odd? d) (fast-mult-inner c (- d 1) (+ acc c))]
                    [else (error "Something went wrong")]))])
    (fast-mult-inner a b 0)))

;;; Modification of the above that tracks the number of steps also —returns
;;; multiple values. Shows a clear logarithmic step count as the product
;;; increases. Notably can take more or less steps depending on the ordering
;;; of operands.
(define (fast-mult-iter-steps a b)
  (letrec ([double (λ (x) (arithmetic-shift x 1))]
           [halve  (λ (x) (arithmetic-shift x -1))]
           [fast-mult-inner
            (λ (c d acc steps)
              (cond [(= d 0) (values acc steps)]
                    [(even? d)
                     (fast-mult-inner (double c) (halve d) acc (add1 steps))]
                    [(odd? d)
                     (fast-mult-inner c (- d 1) (+ acc c) (add1 steps))]
                    [else (error "Something went wrong")]))])
    (fast-mult-inner a b 0 0)))


;;; Exercise 1.19:
(define (T p q ab)
  (let ([a (car ab)]
        [b (cdr ab)])
    ;; Return a dotted pair.
    (cons (+ (* b q) (* a q) (* a p)) (+ (* b p) (* a q)))))

;;; Just runs `T' above twice.
(define (T² p q ab)
  (T p q
     (T p q ab)))

;;; Tpq =>  a <- bq         + aq              + ap,
;;;         b <- bp         + aq
;;;
;;; Insert:
;;; T²pq => a <- (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p,
;;;         b <- (bp + aq)p + (bq + aq + ap)q
;;;
;;; Expand:
;;;      => a <- bpq + aq²  + bq² + aq² + apq + bqp + aqp + ap²,
;;;         b <- bp² + aqp  + bq² + aq² + apq
;;;
;;; Collect b and a in new b, fiddle new a to match:
;;;      => a <- (bq² + 2bpq) + (aq² + 2apq) + (ap² + aq²),
;;;         b <- (bp² + bq²)  + (aq² + 2apq)
;;;
;;; Thus p′ is (p² + q²) and q′ is (q² + 2pq)
;;;
;;; For fibonacci where p = 0, q = 1: p′ = 1, q′ = 1.

;;; To demonstrate `T':
(define (demonstrate-T p q ab count)
  (let ([result (T p q ab)])
    (begin
      (display (format "(T ~s ~s ~s) => ~s~%" p q ab result))
      (cond [(= count 0)
             ;; Completely overkill to end up with a nicely formatted output.
             `(,(cdr result) ,(car result))]
            [else (cons (cdr ab)
                        (demonstrate-T p q result (sub1 count)))]))))

;;; For fibonacci:        `(demonstrate-T 0 1 '(1 . 0) n)',
;;; For double-fibonacci: `(demonstrate-T 1 1 '(1 . 0) n)'.

;;; Combined procedure:
(define (fib-1.19 n)
  (letrec ([fib-1.19-inner
            (λ (a b p q count)
              (cond [(= count 0) b]
                    [(even? count)
                     (fib-1.19-inner a
                                     b
                                     ;; p′ and q′:
                                     (+ (* p p) (* q q))
                                     (+ (* q q) (* 2 p q))
                                     (/ count 2))]
                    [else (fib-1.19-inner (+ (* b q) (* a q) (* a p))
                                          (+ (* b p) (* a q))
                                          p
                                          q
                                          (- count 1))]))])
    (fib-1.19-inner 1 0 0 1 n)))


;;; Fermat’s Little Theorem:
;;; If:     n is prime,
;;;         a ∈ ℕ, a < n
;;; Then:   aⁿ is congruent modulo a,
;;; Where:  Congruence modulo = both sides have the same remainder when divided
;;;         by n.


;;; Section 2


;;; Exercise 2.1:
(define numer
  car)

(define denom
  cdr)

(define (make-rat n d)
  (let ([g (gcd n d)])
    (cons (/ n g) (/ d g))))

(define (show-rat x)
  (display (format "~a/~a~%" (numer x) (denom x))))

;;; Better `MAKE-RAT' that handles signed values:
(define (make-rat n d)
  (let ([g (gcd n d)]
        [sign (if (xor (positive? n) (positive? d))
                  - +)])
    (cons (sign (abs (/ n g)))
          (abs (/ d g)))))


;;; Exercise 2.2:
(define make-point
  cons)

(define x-point
  car)

(define y-point
  cdr)

(define (show-point point)
  (format "(~a, ~a)" (x-point point) (y-point point)))

(define (print-point point)
  (displayln (show-point point)))

(define make-segment
  cons)

(define start-segment
  car)

(define end-segment
  cdr)

(define (show-segment segment)
  (format "~a -> ~a"
          (show-point (start-segment segment))
          (show-point (end-segment segment))))

(define (print-segment segment)
  (displayln (show-segment segment)))

(define (average . numbers)
  (/ (apply + numbers)
     (length numbers)))

(define (midpoint-segment segment)
  (make-point (average (x-point (start-segment segment))
                       (x-point (end-segment segment)))
              (average (y-point (start-segment segment))
                       (y-point (end-segment segment)))))

(define (show-midpoint segment)
  (show-point (midpoint-segment segment)))

(define (print-midpoint segment)
  (displayln (show-midpoint segment)))


;;; Exercise 2.3:
(define (length-segment segment)
  ;; C = √(A² + B² )
  (sqrt))
