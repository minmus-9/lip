;; from https://github.com/ecraven/r7rs-benchmarks/blob/master/src/sum.scm
;;; SUM -- Compute sum of integers from 0 to 10000

(define (run n)
  (let loop ((i n) (sum 0))
    (if (< i 1)
        sum
        (loop (- i 1) (+ i sum)))))

;; the rest of these are mine

(define (run2 n)
 ((lambda (loop) (loop loop n 0))
  (lambda (loop i sum)
    (if (< i 1)
        sum
        (loop loop (- i 1) (+ i sum))))))

(define (run3 n)
  (define (loop i sum)
    (if (< i 1)
        sum
        (loop (- i 1) (+ i sum))))
  (loop n 0))

(define (run4 n)
 ((lambda ()
  (define (loop i sum)
    (if (< i 1)
        sum
        (loop (- i 1) (+ i sum))))
  (loop n 0))))

(define (run5 n)
  (let* loop ((i n) (sum 0))
    (if (< i 1)
        sum
        (loop (- i 1) (+ i sum)))))

(define (run6 n)
  (letrec loop ((i n) (sum 0))
    (if (< i 1)
        sum
        (loop (- i 1) (+ i sum)))))

(define reps 10000)
(timeit (lambda (_) (print (run  reps))) 1)
(timeit (lambda (_) (print (run2 reps))) 1)
(timeit (lambda (_) (print (run3 reps))) 1)
(timeit (lambda (_) (print (run4 reps))) 1)
(timeit (lambda (_) (print (run5 reps))) 1)
(timeit (lambda (_) (print (run6 reps))) 1)

;; EOF
