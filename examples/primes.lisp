;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; prime sieve - can't remember where i found this

(define (primes)
  (define pl '(2))
  (define t pl)
  (define (q x)
    (define n (cons x ()))
    (set-cdr! t n)
    (set! t n))
  (define i 1)
  (define ok #t)
  (define l pl)
  (define (inner)
    (define p (car l))
      (set! l (cdr l))
      (if (null? l)
          ()
          (if (equal? 0 (mod i p))
              (begin (set! ok ()) ())
              (if (< i (* p p)) () #t))))
  (define (outer)
    (set! i (+ i 2))
    (set! ok #t)
    (set! l pl)
    (while inner)
    (if ok (begin (q i) pl) ()))
 (define (driver)
    (while (lambda () (not (outer))))
    (last pl))
  driver
)

(define g (primes))
(define (h _) (print (g)))
(for h 1 20 1)

;;;; EOF
