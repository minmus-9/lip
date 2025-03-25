;;;; from sicp

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
          (if (same-variable? exp var)
              1
              0))
        ((sum? exp)
          (make-sum (deriv (addend exp) var)
                    (deriv (augend exp) var)))
        ((product? exp)
          (make-sum (make-product (multiplier exp)
                                  (deriv (multiplicand exp) var))
                    (make-product (deriv (multiplier exp) var)
                                  (multiplicand exp))))
        (#t (error "unknown expression type: DERIV" exp))))

(define (number? x)
  (or (eq? (type x) 'integer)
      (eq? (type x) 'float)))

(define (symbol? x)
  (eq? (type x) 'symbol))

(define (variable? x) (symbol? x))

(define (same-variable? x y)
  (and (variable? x) (variable? y) (eq? x y)))

(define (zero? x)
  (and (number? x) (equal? x 0)))

(define (one? x)
  (and (number? x) (equal? x 1)))

(define (make-sum x y)
  (cond ((zero? x) y)
        ((zero? y) x)
        ((and (number? x) (number? y)) (+ x y))
        (#t (list '+ x y))))

(define (make-product x y)
  (cond ((or (zero? x) (zero? y)) 0)
        ((one? x) y)
        ((one? y) x)
        ((and (number? x) (number? y)) (* x y))
        (#t (list '* x y))))

(define (sum? x)
  (and (pair? x)
       (eq? (car x) '+)))

(define (addend x) (cadr x))
(define (augend x) (caddr x))

(define (product? x)
  (and (pair? x)
       (eq? (car x) '*)))

(define (multiplier x) (cadr x))
(define (multiplicand x) (caddr x))

(deriv '(* (* x y) (+ x 3)) 'x)
(timeit (lambda (_) (deriv '(* (* x y) (+ x 3)) 'x)) 1000)

;;;; EOF
