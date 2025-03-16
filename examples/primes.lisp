;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prime sieve
;;
;; lip - lisp in python
;;       https://github.com/minmus-9/lip
;; Copyright (C) 2025  Mark Hays (github:minmus-9)
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(define (primes)
    (define pl '(2))
    (define t pl)
    (define (q x)
        (define n (cons x ()))
        (set-cdr! t n)
        (set! t n)
    )
    (define i 1)
    (define ok #t)
    (define l pl)
    (define (inner)
        (define p (car l))
        (set! l (cdr l))
        (if
            (null? l)
            ()
            (if 
                (equal? 0 (% i p))
                (begin (set! ok ()) ())
                (if (< i (* p p)) () #t)
            )
        )
    )
    (define (outer)
        (set! i (+ i 2))
        (set! ok #t)
        (set! l pl)
        (while inner)
        (if ok (begin (q i) pl) ())
    )
    (define (driver)
        (while (lambda () (not (outer))))
        (last pl)
    )
    driver
)

(define g (primes))
(define (h _) (print (g)))
(for h 1 40 1)
