;; from https://github.com/ecraven/r7rs-benchmarks/blob/master/src/fibc.scm
;;; FIBC -- FIB using first-class continuations, written by Kent Dybvig

(define (zero? x) (equal? x 0))  ;; missing from lip

(define (succ n) (+ n 1))
(define (pred n) (- n 1))

;;; fib with peano arithmetic (using numbers) with call/cc

(define (addc x y k)
  (if (zero? y)
      (k x)
      (addc (succ x) (pred y) k)))

(define (fibc x c)
  (if (zero? x)
      (c 0)
      (if (zero? (pred x))
          (c 1)
          (addc (call-with-current-continuation
                 (lambda (c) (fibc (pred x) c)))
                (call-with-current-continuation
                 (lambda (c) (fibc (pred (pred x)) c)))
                c))))

;; lip driver
(define (fib n) (fibc n (lambda (x) x)))

(timeit (lambda (_) (fib 20)) 1)

;; EOF
