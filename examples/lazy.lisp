;; from sicp
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

(
    (lambda (n)
        (
            (lambda (fact) (fact fact n))
                (lambda (ft k) (if (equal? k 1) 1 (* k (ft ft (- k 1)))))
        )
    )
10)




(define (cons x y) (lambda (m) (m x y)))

(special (cons __special_cons_x__ __special_cons_y__)
    (eval `(lambda (__special_cons_m__) (__special_cons_m__ ,__special_cons_x__ ,__special_cons_y__)) 1)
)

(define (car z) (z (lambda (p q) p)))
(define (cdr z) (z (lambda (p q) q)))

(define (list-ref items n)
    (if (equal? n 0)
        (car items)
        (list-ref (cdr items) (- n 1))
    )
)

(define (map proc items)
    (if (null? items)
        ()
        (cons (proc (car items)) (map proc (cdr items)))
    )
)

(define (scale-list items factor) (map (lambda (x) (* x factor)) items))

(define (add-lists list1 list2)
    (cond
        ((null? list1) list2)
        ((null? list2) list1)
        (#t (cons
            (+ (car list1) (car list2))
            (add-lists (cdr list1) (cdr list2))))
    )
)

(define ones (cons 1 ones))
(define integers (cons 1 (add-lists ones integers)))
(print (car integers))
(print (cdr integers))
(print (cadr integers))
(print (cdr (cdr integers)))

;; this takes 74 seconds!!!
;(list-ref integers 17)

(list-ref integers 10)
