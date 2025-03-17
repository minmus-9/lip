;; from http://www.ulisp.com/show?1EO1

(define (tak x y z)
  (if (not (< y x))
      z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y))))

;; 55.6s on arduino 2560 to return 7
(timeit (lambda (_) (print (tak 18 12 6))) 1)



(define (fib n)
    (if (< n 3)
        1
        (+ (fib (- n 1)) (fib (- n 2)))))

;; 30.5sec on arduino 2560 to return 28657
(timeit (lambda (_) (print (fib 23))) 1)



(define (q n)
  (if (<= n 2) 1
    (+
     (q (- n (q (- n 1))))
     (q (- n (q (- n 2)))))))

;; 58.2 on arduino 2560 to return 12
(timeit (lambda (_) (print (q 21))) 1)



(define (1- x) (- x 1))

(define (q2 x y)
  (if (or (< x 1) (< y 1)) 1
    (+ (q2 (- x (q2 (1- x) y)) y)
       (q2 x (- y (q2 x (1- y)))))))

;; 113sec on arduino 2560 to return 31
(timeit (lambda (_) (print (q2 7 8))) 1)


(define (zerop x) (equal? x 0))
(define mod %)

(define (factor n)
  (cond
   ((zerop (mod n 2)) 2)
   ((zerop (mod n 3)) 3)
   (#t (let loop ((d 5) (i 2))
        (if
            (> (* d d) n)
            n
            (if
                (zerop (mod n d))
                d
                (loop (+ d i) (- 6 i))))))))

;; 3.9sec on arduino 2560 to return 2142142141
(timeit (lambda (_) (print (factor 2142142141))) 1)
;; 4sec on arduino 2560 to return 46327
(timeit (lambda (_) (print (factor 2146654199))) 1)



(define = equal?)

(define (factorize n)
  (let ((f (factor n)))
    (if (= n f) (list n) (cons f (factorize (/ n f))))))

;; no timing given to produce (3 17 43 333667)
(timeit (lambda (_) (print (factorize 731731731))) 1)
