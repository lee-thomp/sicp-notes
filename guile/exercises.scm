(use-modules (srfi srfi-1)
             (srfi srfi-64))


;;; Exercise 2.1
(define make-rat cons)
(define numer car)
(define denom cdr)

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (make-rat/better n d)
  (let ((g (gcd n d)))
    (if (positive? d)
        (cons (/ n g)
              (/ d g))
        (cons (/ n g -1)
              (/ d g -1)))))

(test-begin "Exercise 2.1")

(define (show-rat x)
  (format #f "~a/~a" (numer x) (denom x)))

(test-group "Initial `make-rat'"
  ;; `make-rat' works fine for select cases:
  (test-equal "1/2"  (show-rat (make-rat 1 2)))
  (test-equal "-1/2" (show-rat (make-rat -1 2)))

  ;; But not for all:
  (test-expect-fail 2)
  (test-equal "-1/2" (show-rat (make-rat 1 -2)))
  (test-equal "1/2"  (show-rat (make-rat -1 -2))))

(test-group "Improved `make-rat'"
  (test-equal "1/2"  (show-rat (make-rat/better 1 2)))
  (test-equal "-1/2" (show-rat (make-rat/better -1 2)))
  (test-equal "-1/2" (show-rat (make-rat/better 1 -2)))
  (test-equal "1/2"  (show-rat (make-rat/better -1 -2))))

(test-end "Exercise 2.1")


;;; Exercise 2.2

;;; The same implementation as for above can be used for just 'pairing' two data
;;; together
(define make-point cons)
(define x-point car)
(define y-point cdr)

;;; —and again:
(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define (midpoint-segment l)
  (define (average a b)
    (/ (+ a b) 2))
  (let ([start-x (x-point (start-segment l))]
        [start-y (y-point (start-segment l))]
        [end-x   (x-point (end-segment l))]
        [end-y   (y-point (end-segment l))])
    (make-point
     (average start-x end-x)
     (average start-y end-y))))

(test-begin "Exercise 2.2")

(define (show-point p)
  (format #f "(~a,~a)" (x-point p) (y-point p)))

(let* ([point-a (make-point 0 2)]
       [point-b (make-point 5 4)]
       [point-c (make-point 3 0)]
       [line-ab (make-segment point-a point-b)]
       [line-bc (make-segment point-b point-c)])
  (test-equal "(5/2,3)" (show-point (midpoint-segment line-ab)))
  (test-equal "(4,2)"   (show-point (midpoint-segment line-bc))))

(test-end "Exercise 2.2")


;;; Exercise 2.3

;;; —For this exercise I'll assume rectangles are always aligned to the axes,
;;; i.e. not rotated wrt. x and y.

;;; Due to scoping stuff the procedures for getting a width and height from a
;;; rectangle must be passed in as parameters to these functions:
(define (perimeter-rectangle w h r)
  (* 2 (+ (w r)
          (h r))))

(define (area-rectangle w h r)
  (* (w r)
     (h r)))

(test-begin "Exercise 2.3")

(test-group "Approach 1"
  ;; Suppose a rectangle is represented by any two opposing corners:
  (define make-rectangle cons)
  (define near-corner car)
  (define far-corner cdr)

  (define (width-rectangle r)
    (let ([x1 (x-point (near-corner r))]
          [x2 (x-point (far-corner r))])
      (abs (- x2 x1))))

  (define (height-rectangle r)
    (let ([y1 (y-point (near-corner r))]
          [y2 (y-point (far-corner r))])
      (abs (- y2 y1))))

  (define rectangle (make-rectangle
                     (make-point 0 0)
                     (make-point 1 1)))

  (test-eqv 4 (perimeter-rectangle width-rectangle height-rectangle rectangle))
  (test-eqv 1 (area-rectangle width-rectangle height-rectangle rectangle)))

(test-group "Approach 2"
  ;; In this wacky approach all rectangles are created 'golden' i.e. with
  ;; dimensions given by successive numbers in the Fibonacci sequence:
  (define make-rectangle identity)

  (define (fib n)
    (define (fib-inner a b c)
      (if (zero? c) a
          (fib-inner b (+ a b) (- c 1))))
    (fib-inner 0 1 n))

  (define (width-rectangle r)
    (fib r))

  (define (height-rectangle r)
    (fib (+ r 1)))

  (define rectangle (make-rectangle 5))

  (test-eqv 26 (perimeter-rectangle width-rectangle height-rectangle rectangle))
  (test-eqv 40 (area-rectangle width-rectangle height-rectangle rectangle)))

(test-end "Exercise 2.3")
