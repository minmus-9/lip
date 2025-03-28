;;;; from https://github.com/ecraven/r7rs-benchmarks/blob/master/src/ack.scm

;;; ACK -- One of the Kernighan and Van Wyk benchmarks.

(define (ack m n)
  (cond ((= m 0) (+ n 1))
        ((= n 0) (ack (- m 1) 1))
        (else (ack (- m 1) (ack m (- n 1))))))

(timeit (lambda (_) (print (ack 3 5))) 1)
