;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fast factorial -- i ran across something like this somewhere on the
;;                   net... was an example of not having to use (define)
;;                   to create a recursive function iirc. the wikipedia
;;                   page for lambda calculus shows this technique as
;;                   well
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

(define (! n)
    ((lambda (f) (f f 1 n))
        (lambda (f p k)
            (if
                (< k 2)
                p
                (f f (* p k) (- k 1))))))

(define n 10000)

(timeit (lambda (_) (! n)) 10)

;; EOF
