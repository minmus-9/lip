;;;; from https://github.com/ecraven/r7rs-benchmarks/blob/master/src/primes.scm

;;; PRIMES -- Compute primes less than n, written by Eric Mohr.

(define remainder mod)

(define  (interval-list m n)
  (if (> m n)
      '()
      (cons m (interval-list (+ 1 m) n))))

(define (sieve l)
  (letrec ((remove-multiples
            (lambda (n l)
              (if (null? l)
                  '()
                  (if (= (remainder (car l) n) 0)
                      (remove-multiples n (cdr l))
                      (cons (car l)
                            (remove-multiples n (cdr l))))))))
    (if (null? l)
        '()
        (cons (car l)
              (sieve (remove-multiples (car l) (cdr l)))))))

(define (primes<= n)
  (sieve (interval-list 2 n)))

(print (primes<= 100))
(timeit (lambda (_) (primes<= 1000)) 1)

;;;; EOF
