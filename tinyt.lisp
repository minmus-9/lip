;;;; tinyt.lisp -- tests for tiny.py

(define x 1)
(if (not (equal? x 1)) (error "fail 1"))

(define (f x) x)
(if (not (equal? (f 2) 2)) (error "fail 2"))

(define (f x . y) 1 x)
(if (not (equal? (f 3) 3)) (error "fail 3"))

(define f (lambda (x y) x))
(if (not (equal? (f 4 5) 4)) (error "fail 4"))

(define f (lambda (x) 2 x))
(if (not (equal? (f 4) 4)) (error "fail 5"))

(if (not (eq? (quote x) (string>symbol "x"))) (error "fail 6"))
(if (not (eq? 'x (string>symbol "x"))) (error "fail 7"))

(set! x 11)
(if (not (equal? x 11)) (error "fail 8"))

(trap 1 (lambda (msg exp) (error "fail 9")))
(trap (a b) (lambda (msg exp) (if (not (equal? msg "a")) (error "fail 10"))))

(set! f (lambda (x) x))
(if (not (equal? (apply f '(1)) 1)) (error "fail 11"))

(if (not (equal? (atom>string 'a) "a")) (error "fail 12"))

(if (not (equal? '(1 2) (cons 1 (cons 2 ())))) (error "fail 13"))
(if (not (equal? (car '(1 2)) 1)) (error "fail 14"))
(if (not (equal? (cadr '(1 2)) 2)) (error "fail 15"))

(if (not (equal? (/ 5 2) 2)) (error "fail 16"))
(if (not (equal? (/ 5 2.) 2.5)) (error "fail 16"))

(set! x 1)
(if (not (equal? ((lambda (x) (eval 'x 0)) 2) 2)) (error "fail 17"))
(if (not (equal? ((lambda (x) (eval 'x 1)) 2) 1)) (error "fail 18"))
(if (not (equal? (eval "x") 1)) (error "fail 19"))

(if (< 4 5) () (error "fail 20"))
(if (< 5 4) (error "fail 21"))
(if (not (equal? (* 3 7) 21)) (error "fail 22"))
(if (not (equal? (nand 1 3) -2)) (error "fail 23"))

(set! x (cons 1 2))
(set-car! x 3)
(if (not (equal? (cons 3 2) x)) (error "fail 24"))
(set-cdr! x 4)
(if (not (equal? (cons 3 4) x)) (error "fail 35"))

(if (not (equal? (string>number "1") 1)) (error "fail 25"))
(if (not (equal? (string>number "1.5") 1.5)) (error "fail 26"))
(if (not (eq? (string>symbol "z") 'z)) (error "fail 27"))

(if (not (true? #t)) (error "fail 28"))
(if (not (pair? (list 1))) (error "fail 29"))
(if (pair? 1) (error "fail 30"))

(if (not (symbol? 'x)) (error "fail 31"))
(if (not (integer? 1)) (error "fail 32"))
(if (not (float? 1.5)) (error "fail 33"))
(if (not (string? "a")) (error "fail 34"))
;; 35 is above
(if (not (lambda? +)) (error "fail 36"))
(if (not (primitive? -)) (error "fail 37"))
(if (opaque? ()) (error "fail 38"))

(if (not (equal? (+ 2 3) 5)) (error "fail 39"))
(if (not (equal? (abs 1) 1)) (error "fail 40"))
(if (not (equal? (abs -1) 1)) (error "fail 41"))

(define (f x)
  (cond ((equal? x 1) -1)
        ((< x 3) 1)
        (else 7)))
(if (equal? (f 1) -1) () (error "fail 42"))
(if (equal? (f 2) 1) () (error "fail 43"))
(if (equal? (f 9) 7) () (error "fail 44"))

(if (not (equal? '(1 2) (list 1 2))) (error "fail 45"))

(if (or () ()) (error "fail 46"))
(if (equal? (or () 17) 17) () (error "fail 47"))

(if (and () ()) (error "fail 48"))
(if (equal? (and 1 2) 2) () (error "fail 49"))

(if (equal? (~ 0) -1) () (error "fail 50"))
(if (equal? (& 1 3) 1) () (error "fail 51"))
(if (equal? (| 1 2) 3) () (error "fail 52"))
(if (equal? (^ 1 3) 2) () (error "fail 53"))

(if (equal? (mod 5 2) 1) () (error "fail 54"))

(if (equal? (lshift 5 2) 20) () (error "fail 55"))
(if (equal? (rshift 20 3) 2) () (error "fail 56"))

(if (>= 5 3) () (error "fail 57"))
(if (>= 3 5) (error "fail 58"))
(if (> 5 3) () (error "fail 59"))
(if (> 3 5) (error "fail 60"))
(if (<= 5 3) (error "fail 61"))
(if (<= 3 5) () (error "fail 62"))

(if (not (equal? (reverse '(1 2)) '(2 1))) (error "fail 63"))

(if (not (equal? (length '(1 2 3)) 3)) (error "fail 64"))
(if (not (equal? '(1 2 3)
                 (map1 (lambda (x) (+ x 1)) '(0 1 2))))
    (error "fail 65"))
(if (not (equal? '(1 2 3)
                 (map (lambda (x) (+ x 1)) '(0 1 2))))
    (error "fail 66"))

(if (not (equal? '(1 2 3 4) (join '(1 2) '(3 4)))) (error "fail 67"))

(set! x 3)
(let ((x 1) (y x))
  (if (equal? x 1) () (error "fail 68"))
  (if (equal? y 3) () (error "fail 69")))

(let* ((x 1) (y x))
  (if (equal? x 1) () (error "fail 68"))
  (if (equal? y 1) () (error "fail 69")))

(letrec ((x 1) (y x))
  (if (equal? x 1) () (error "fail 68"))
  (if (equal? y 1) () (error "fail 69")))

(set! x 0)
(while (lambda ()
         (set! x (+ x 1))
         (< x 3)))
(if (equal? x 3) () (error "fail 70"))

(set! x ())
(for (lambda (i)
       (set! x (cons i x)))
     0
     5
     2)
(if (equal? x '(4 2 0)) () (error "fail 71"))

(set! x ())
(foreach (lambda (i) (set! x (cons i x))) '(1 2 3))
(if (equal? x '(3 2 1)) () (error "fail 72"))

(no-op (a b c))

(display "pass\n")

;;;; EOF
