;; from sicp
;;

(define (memo-proc proc)
  (let ((already-run? ())
        (result ()))
    (lambda ()
      (if (not already-run?)
          (begin
            (set! result (proc))
            (set! already-run? #t)
            result)
          result))))

(define the-empty-stream ())
(define stream-null? null?)

(define (stream-ref s n)
   (if (equal? n 0)
       (stream-car s)
       (stream-ref (stream-cdr s) (sub n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin
        (proc (stream-car s))
        (stream-for-each proc (stream-cdr s)))))

(special delay (lambda (x)
    (eval `(memo-proc (lambda () ,x)) 1)))

(define (force x) (x))

(special cons-stream (lambda (x y) (eval `(cons ,x (delay ,y)) 1)))

(define (stream-car x) (car x))
(define (stream-cdr x) (force (cdr x)))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
          (cons-stream (stream-car stream)
                       (stream-filter pred (stream-cdr stream))))
        (#t (stream-filter pred (stream-cdr stream)))))


(define (fibgen a b) (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))

(define (stream-counter start)
  (cons-stream start
               (stream-counter (+ start 1))))

(define (sieve stream)
  (define (divisible? x y) (equal? (mod x y) 0))
  (cons-stream
      (stream-car stream)
      (sieve
        (stream-filter
          (lambda (x)
            (not (divisible? x (stream-car stream))))
          (stream-cdr stream)))))

(define primes (sieve (stream-counter 2)))
;(stream-ref primes 50)  ;; almost 6.5 sec to compute!
;(stream-for-each print primes)


(define (stream-enumerate-interval low high)
  (if (< high low)
      the-empty-stream
      (cons-stream
          low
          (stream-enumerate-interval (+ low 1) high))))

;(stream-for-each print (stream-enumerate-interval 1 10))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
        (apply proc (map stream-car argstreams))
        (apply stream-map (cons proc (map stream-cdr argstreams))))))

(define (stream-sink f s)
  (if (stream-null? s)
      ()
      (begin
        (f (stream-car s))
        (stream-sink f (stream-cdr s)))))

(define (stream-add s1 s2) (stream-map + s1 s2))

(define ones (cons-stream 1 ones))

(define integers
    (cons-stream 1 (stream-add ones integers)))

;(stream-sink print integers)
;(stream-for-each print integers)




(define fibs (cons-stream 0 (cons-stream 1 (stream-add (stream-cdr fibs) fibs))))

(no-op (iter-func
    (lambda (stream) (print (stream-car stream)) (stream-cdr stream))
    fibs
    10))

;(stream-sink print fibs)
