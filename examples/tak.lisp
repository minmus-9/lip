;; from https://github.com/ecraven/r7rs-benchmarks/blob/master/src/tak.scm
;;; TAK -- A vanilla version of the TAKeuchi function.

(define (tak x y z)
  (if (< y x)
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y))
      z))

(timeit (lambda (_) (print (tak 18 12 6))) 1)

;; from https://github.com/ecraven/r7rs-benchmarks/blob/master/src/cpstak.scm
;;; CPSTAK -- A continuation-passing version of the TAK benchmark.
;;; A good test of first class procedures and tail recursion.

(define (cpstak x y z)
  (define (tak x y z k)
    (if (< y x)
        (tak (- x 1)
             y
             z
             (lambda (v1)
               (tak (- y 1)
                    z
                    x
                    (lambda (v2)
                      (tak (- z 1)
                           x
                           y
                           (lambda (v3)
                             (tak v1 v2 v3 k)))))))
        (k z)))

  (tak x y z (lambda (a) a)))

(timeit (lambda (_) (print (cpstak 18 12 6))) 1)

;; from https://www.dreamsongs.com/Files/Timrep.pdf

(define = equal?)
(define (1- x) (- x 1))
(define null null?)

(define (listn n)
    (if (= 0 n) () (cons n (listn (1- n)))))

(define 18l (listn 18))
(define 12l (listn 12))
(define  6l (listn 6))

(define (mas x y z)
    (if (shorterp y x)
        (mas
            (mas (cdr x) y z)
            (mas (cdr y) z x)
            (mas (cdr z) x y))
        z))

(define (ltak x y z)
    (car (mas x y z)))

;; 5.7 sec
(define (shorterp x y)
    (if
        y
        (if
            (null? x)
            #t
            (shorterp (cdr x) (cdr y)))
        ()))

;; 5.9 sec
(define (shorterp x y)
    (and y (or (null? x) (shorterp (cdr x) (cdr y)))))

(define 2l (listn 2))
(timeit (lambda (_) (print (ltak 12l 6l 2l))) 1)
(timeit (lambda (_) (print (ltak 18l 12l 6l))) 1)


;; from https://www.dreamsongs.com/Files/Timrep.pdf

;;; Here are the definitions of TAKF as provided by GJC.
;;; #-NIL means except in NIL, #+NIL means for NIL.
(define (takf x y z) (takfsub takfsub x y z))

(define (takfsub f x y z)
    (if (< y x)
        (f f (f f (1- x) y z)
             (f f (1- y) z x)
             (f f (1- z) x y))
        z))

;; 36.8 sec
(timeit (lambda (_) (print (takf 18 12 6))) 1)


;; from https://www.dreamsongs.com/Files/Timrep.pdf

(define (stak x y z) (stak-aux))

(special (stak-aux)
    (if (< y x)
        (let ((x (let ((x (1- x))
                       (y y)
                       (z z))
                       (stak-aux)))
              (y (let ((x (1- y))
                       (y z)
                       (z x))
                       (stak-aux)))
              (z (let ((x (1- z))
                       (y x)
                       (z y))
                       (stak-aux))))
            (stak-aux))
        z))

;; 34.4 sec
(timeit (lambda (_) (print (stak 18 12 6))) 1)


;; EOF
