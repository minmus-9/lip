;; from https://www.dreamsongs.com/Files/Timrep.pdf

(define (atom x) (or (atom? x) (eq? (type x) 'integer)))
(define eq eq?)
(define t #t)
(define = equal?)
(define (1- x) (- x 1))
(define (mapcar f L)
    (if (null? L) () (cons (f (car L)) (mapcar f (cdr L)))))
(define (cddr l) (cdr (cdr l)))

(special (defvar var & value)
    (if
        (null? value)
        (set! value ())
        (if
            (null? (cdr value))
            (set! value (car value))
            (error "too many args, expected one or two")))
    (eval `(define ,var ,value) 1))

(special (defun sym args & body)
    (if
        (null? (cdr body))
        (set! body (car body))
        (set! body (cons 'begin body)))
    (eval `(define ,(cons sym args) ,body) 1))

(defun create-n (n)
    (let loop ((n n) (a ()))
        (if
            (= n 0)
            a
            (loop (1- n) (cons () a)))))

(defvar ll (create-n 200))

(defun iterative-div2 (l)
    (let loop ((l l) (a ()))
        (if
            (null? l)
            a
            (loop (cddr l) (cons () a)))))

(defun recursive-div2 (l)
    (if
        (null? l)
        ()
        (cons (car l) (recursive-div2 (cddr l)))))

(timeit (lambda (_) (iterative-div2 ll)) 1000)
(timeit (lambda (_) (recursive-div2 ll)) 1000)
