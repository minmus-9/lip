;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; calculate pi
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

(define (pi1 _)
    (define z 1)
    (define k 3)
    (define s -1.0)
    (define (f c & _) (if (< k 25000) (c c) ()));print (* z 4))))
    (f  (call/cc (lambda (cc) cc))
        (set! z (+ z (/ s k)))
        (set! k (+ k 2))
        (set! s (- s))
    )
)
(timeit pi1 1)

(define (pi2 & _)
    (define k 2)
    (define a 4)
    (define b 1)
    (define a1 12)
    (define b1 4)
    (define d ())
    (define d1 ())
    (define (next)
        (define p (* k k))
        (define q (+ (* k 2) 1))
        (set! k (+ k 1))
        (define t1 (+ (* p a) (* q a1)))
        (define t2 (+ (* p b) (* q b1)))
        (set! a a1)
        (set! b b1)
        (set! a1 t1)
        (set! b1 t2)
        (set! d (/ a b))
        (set! d1 (/ a1 b1))
        (while inner)
        (if
            (< k 20)
            #t
            ()
        )
    )
    (define (inner)
        (if
            (equal? d d1)
            (begin
                ;(print d)
                (set! a  (* 10 (% a b)))
                (set! a1 (* 10 (% a1 b1)))
                (set! d  (/ a b))
                (set! d1 (/ a1 b1))
                #t
            )
            ()
        )
    )
    (while next)
)
(timeit pi2 100)
