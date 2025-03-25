;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; sicp.lisp - some stuff from early in the sicp book

;;; exercise 1.4 p.27
(define (a-plus-abs-b a b)
  ((if (< b 0) - +) a b))

(a-plus-abs-b 1 1)
(a-plus-abs-b 1 -1)

;;; newton's method for sqrt p.30-31
(define (improve x guess) (* .5 (+ guess (/ x guess))))

(define (good-enough? guess x tol) (< (abs (- (* guess guess) x)) tol))

(define (sqrt-iter guess x tol)
  (if  (good-enough? guess x tol)
       guess
       (sqrt-iter (improve x guess) x tol)))

(define (sqrt x tol) (sqrt-iter 1. x tol))

(sqrt 2 1e-3)

;;; newton's method with private procedures p.38-39
(define (sqrt x tol)
  (define (improve. guess) (* .5 (+ guess (/ x guess))))
  (define (good-enough. guess) (< (abs (- (* guess guess) x)) tol))
  (define (sqrt-iter. guess)
    (if  (good-enough. guess)
         guess
         (sqrt-iter. (improve. guess))))
  (sqrt-iter. 1.))

(sqrt 2 1e-3)

;;; factorial p.41-43
(define (factorial1 n)
  (if (equal? n 0)
      1
      (* n (factorial1 (- n 1)))))

(factorial1 6)

(define (factorial2 n)
  (define (fact-iter product counter max-count)
    (if (< max-count counter)
        product
        (fact-iter  (* counter product)
                    (+ counter 1)
                    max-count)))
  (fact-iter 1 1 n))

(factorial2 6)

;;; fibonacci p.48-50
(define (fib1 n)
  (cond ((equal? n 0) 1)
        ((equal? n 1) 1)
        (#t (+ (fib1 (- n 1))
               (fib1 (- n 2))))))

(fib1 10)

(define (fib2 n)
  (define (fib-iter a b count)
    (if (equal? count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 1 n))

(fib2 10)

;;; change counting p. 52

(define (count-change amount)
  (define (first-denomination kinds-of-coins)
    (cond ((equal? kinds-of-coins 1)  1)
          ((equal? kinds-of-coins 2)  5)
          ((equal? kinds-of-coins 3)  10)
          ((equal? kinds-of-coins 4)  25)
          ((equal? kinds-of-coins 5)  50)))
  (define (cc amount kinds-of-coins)
    (cond ((equal? amount 0)  1)
          ((or (< amount 0) (equal? kinds-of-coins 0)) 0)
          (#t  (+ (cc amount
                      (- kinds-of-coins 1))
                  (cc (- amount (first-denomination kinds-of-coins))
                      kinds-of-coins)))))
  (cc amount 5))

;; the first thing you'll notice about this code is how slow it is. while
;; you're waiting, the second thing you'll notice is how slow it is.
;(count-change 100)
(count-change 25)

;;; summation p.77-78
(define (sum term a next b)
  (if (< b a)
      0
      (+ (term a)
         (sum term (next a) next b))))

(sum (lambda (x) x) 1 (lambda (x) (+ x 1)) 10)

;;; use of let, p.88
;;; test of let should print 4
(define x 3)
(let ((x 1) (y x)) (+ x y))

(define (f g) (g 2))
(define (square x) (* x x))
(f square)
(f (lambda (z) (* z (+ z 1))))

;;; bisection p.89-90
(define (bisection f a b tol)
  (define (bisection1 f a b tol)
    (define (close-enough? x y)
      (< (abs (- x y)) tol))
    (let
        ((midpoint (* .5 (+ a b))))
      (if (close-enough? a b)
          midpoint
          (let ((fc (f midpoint)))
            (cond ((< 0 fc) (bisection1 f a midpoint tol))
                  ((< fc 0) (bisection1 f midpoint b tol))
                  (#t midpoint))))))
  (let
      ((fa (f a)) (fb (f b)))
    (cond ((and (< fa 0) (< 0 fb)) (bisection1 f a b tol))
          ((and (< fb 0) (< 0 fa)) (bisection1 f b a tol))
          (#t (error "no bracket")))))

(bisection (lambda (x) (- (square x) 1)) 1.5 0 1e-2)

;;; pairs p.125
(define (kons x y) (lambda (m) (m x y)))
(define (kar z) (z (lambda (p q) p)))
(define (kdr z) (z (lambda (p q) q)))

(kons 11 42)
(kar (kons 11 42))
(kdr (kons 11 42))

;;;; EOF
