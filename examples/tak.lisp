;; from https://github.com/ecraven/r7rs-benchmarks/blob/master/src/tak.scm
;;; TAK -- A vanilla version of the TAKeuchi function.

(define (tak x y z)
  (if (not (< y x))
      z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y))))

;; from https://github.com/ecraven/r7rs-benchmarks/blob/master/src/cpstak.scm
;;; CPSTAK -- A continuation-passing version of the TAK benchmark.
;;; A good test of first class procedures and tail recursion.

(define (cpstak x y z)
  (define (tak x y z k)
    (if (not (< y x))
        (k z)
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
                             (tak v1 v2 v3 k)))))))))

  (tak x y z (lambda (a) a)))

(timeit (lambda (_) (print (tak 12 6 0))) 1)
(timeit (lambda (_) (print (cpstak 12 6 0))) 1)

;; EOF
