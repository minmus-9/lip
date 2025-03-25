;; lisp.lisp - runtime for lisp.py
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; {{{ basics

;; to accompany quasiquote
(define (unquote x) (error "cannot unquote here"))
(define (unquote-splicing x) (error "cannot unquote-splicing here"))

;; ditto
(define (caar l) (car (car l)))
(define (caaar l) (car (car (car l))))
(define (caaaar l) (caar (caar l)))

(define (cddr l) (cdr (cdr l)))
(define (cdddr l) (cdr (cdr (cdr l))))
(define (cddddr l) (cddr (cddr l)))

(define (cadr l) (car (cdr l)))
(define (caddr l) (car (cddr l)))
(define (cadddr l) (car (cdddr l)))
(define (caddddr l) (car (cddddr l)))

;; }}}
;; {{{ bitwise ops

;; bitwise ops from nand
(define (~ x)   (nand x x))
(define (& x y) (~ (nand x y)))
(define (| x y) (nand (~ x) (~ y)))
(define (^ x y) (& (nand x y) (| x y)))

;; }}}
;; {{{ arithmetic

(define (+ x y) (- x (- y)))

;; oh, mod
(define (mod n d) (- n (* d (/ n d))))

;; absolute value
(define (abs x)
  (if (< x 0)
      (- x)
      x))

;; copysign
(define (copysign x y)
  (if (< y 0)
      (- (abs x))
      (abs x)))

;; unsigned shifts
(define (lshift x n)
  (if (equal? n 0)
      x
      (lshift (+ x x) (- n 1))))

(define (rshift x n)
  (if (equal? n 0)
      x
      (rshift (/ x 2) (- n 1))))

;; }}}
;; {{{ not

(define (not x) (if x () #t))

;; }}}
;; {{{ comparison predicates

(define (>= x y) (if (< x y) () #t))
(define (>  x y) (< y x))
(define (<= x y) (if (< y x) () #t))

;; }}}
;; {{{ assert

(special (assert __special_assert_sexpr__)
    ;; extra indentation for (special) body...
    (if (eval __special_assert_sexpr__ 1)
        ()
        (error (obj>string __special_assert_sexpr__))))

;; }}}
;; {{{ reverse

(define (reverse l)
  (define (rev x y)
    (if (null? x)
        y
        (rev (cdr x) (cons (car x) y))))
  (rev l ()))

;; }}}
;; {{{ length

(define (length lst)
  (define (iter l i)
    (if (null? l)
        i
        (iter (cdr l) (- i -1))))
  (iter lst 0))

;; }}}
;; {{{ fold, transpose, map
;; sicp p.158-165 with interface tweaks
(define (fold-left f x sequence)
  (if (null? sequence)
      x
      (fold-left f (f (car sequence) x) (cdr sequence))))

(define reduce fold-left)  ;; python nomenclature

(define (fold-right f initial sequence)
  (fold-left f initial (reverse sequence)))

(define accumulate fold-right)  ;; sicp nomenclature

;(fold-left  cons () (list 1 4 9))  ;; (9 4 1)    (cons 9 (cons 4 (cons 1 ())))
;(fold-right cons () (list 1 4 9))  ;; (1 4 9)    (cons 1 (cons 4 (cons 9 ())))

;; not elegant like sicp -- but faster in this lisp
(define (map1 f lst)
  (define ret ())  ;; head of queue and return value
  (define tail ())  ;; tail of queue
  (define (map1$ lst)
    (if (null? lst)
        ret
        (begin
          ;; link in the new value
          (set-cdr! tail (cons (f (car lst)) ()))
          (set! tail (cdr tail))
          ;; rinse, repeat
          (map1$ (cdr lst)))))
    (if (null? lst)
        ()
        (begin
          ;; enqueue the first item here to avoid main loop test
          (set! ret (cons (f (car lst)) ()))
          (set! tail ret)
          (map1$ (cdr lst)))))

;; map 2 funcs over a lst of 2-lists (x y); for example, a list of (let) vdefs
;; returns
;;      (cons (map1 car lst) (map1 cadr lst))
;; but faster
(define (map-2-list fcar fcdr lst)
  (define rcar ())
  (define tcar ())
  (define rcdr ())
  (define tcdr ())
  (define (map2$ lst)
    (if (null? lst)
        (cons rcar rcdr)
        (begin
          (define xy (car lst))
          ;; link in the new values
          (set-cdr! tcar (cons (fcar xy) ()))
          (set! tcar (cdr tcar))
          (set-cdr! tcdr (cons (fcdr xy) ()))
          (set! tcdr (cdr tcdr))
          ;; rinse, repeat
          (map2$ (cdr lst)))))
    (if (null? lst)
        ()
        (begin
          ;; enqueue the first item here to avoid main loop test
          (define xy (car lst))
          (set! rcar (cons (fcar xy) ()))
          (set! tcar rcar)
          (set! rcdr (cons (fcdr xy) ()))
          (set! tcdr rcdr)
          (map2$ (cdr lst)))))

(define (accumulate-n f initial sequences)
  (define r ())
  (define c (call/cc))
  (if (null? (car sequences))
      (reverse r)
      (begin
        (set! r (cons (accumulate f initial (map1 car sequences)) r))
        (set! sequences (map1 cdr sequences))
        (c c))))

(define (ftranspose f lists)
  (define ret ())  ;; head of queue and return value
  (define tail ())  ;; tail of queue
  (define (t1 lists)
    (if (null? (car lists))
        ret
        (begin
          ;; link in the new value
          (set-cdr! tail (cons (f (map1 car lists)) ()))
          (set! tail (cdr tail))
          ;; rinse, repeat
          (t1 (map1 cdr lists)))))
    (if (null? lists)
        ()
        (if
            (null? (car lists))
            ()
            (begin
              ;; enqueue the first item here to avoid main loop test
              (set! ret (cons (f (map1 car lists)) ()))
              (set! tail ret)
              (t1 (map1 cdr lists))))))

(define (transpose lists)
    (ftranspose (lambda (x) x) lists))

(define (map f . lists)
    (ftranspose (lambda (tuple) (apply f tuple)) lists))

;; }}}
;; {{{ queue

(define (queue)
  (define (unpack0 args)
    (if args (error "too many args") ()))
  (define (unpack1 args)
    (if (null? args)
        (error "not enough args")
        (if (null? (cdr args))
            (car args)
            (error "too many args"))))
  (define head ())
  (define tail ())
  (define node ())
  (define (enqueue x)
    (set! node (cons x ()))
    (if (null? head)
        (set! head node)
        (set-cdr! tail node))
    (set! tail node))
  (define (e lst)
    (if (null? lst)
        ()
        (begin
          (set-cdr! tail (cons (car lst) ()))
          (set! tail (cdr tail))
          (e (cdr lst)))))
  (define (extend lst)
    (if (null? lst)
        ()
        (begin
          (enqueue (car lst))
          (e (cdr lst)))))
  (define (dequeue)
    (define n head)
    (set! head (cdr n))
    (if (null? head)
        (set! tail ())
        ())
    (car n))
  (define (append x)
    (if (null? head)
        (extend x)
        (if (pair? x)
            (set-cdr! tail x)
            (if (null? x) () (error "can only append list")))))
  (define (dispatch m . args)
    (cond ((eq? m 'extend) (extend (unpack1 args)))
          ((eq? m 'enqueue) (enqueue (unpack1 args)))
          ((eq? m 'dequeue) (unpack0 args) (dequeue))
          ((eq? m 'get) (unpack0 args) head)
          ((eq? m 'depth) (unpack0 args) (length head))
          ((eq? m 'append) (append (unpack1 args)))
          ((eq? m 'last) (car tail))))
  dispatch)

;; }}}
;; {{{ join

(define (join x . lists)
  (define q (queue))
  (define (j x lists)
    (if (null? lists)
        (begin
          (q 'append x)
          (q 'get))
        (begin
          (q 'extend x)
          (j (car lists) (cdr lists)))))
  (if (null? lists) x (j x lists))
)

;; }}}
;; {{{ let

(special (let __special_let_vdefs__ . __special_let_body__)
    (eval (let$ __special_let_vdefs__ __special_let_body__) 1))

(define (let$ vdefs body)
  (if (eq? (type vdefs) 'symbol)
      (let$3 vdefs (car body) (cdr body))
      (let$2 vdefs body)))

(define (let$2 vdefs body)
  (define xy (map-2-list car cadr vdefs))
  `((lambda (,@(car xy)) ,@body) ,@(cdr xy)))

(define (let$3 sym vdefs body)
  (define xy (map-2-list car cadr vdefs))
  `(begin
     (define (,sym ,@(car xy)) ,@body)
     (,sym ,@(cdr xy))))

;; }}}
;; {{{ let*

(special (let* __special_lets_vdefs__ . __special_lets_body__)
    (eval (let*$ __special_lets_vdefs__ __special_lets_body__) 1))

(define (let*$ vdefs body)
  (if (eq? (type vdefs) 'symbol)
      (let*$3 vdefs (car body) (cdr body))
      (let*$2 vdefs body)))

(define (let*$2 vdefs body)
  (if (null? vdefs)
      (if (null? (cdr body)) (car body) (cons 'begin body))
      (begin
        (define kv (car vdefs))
        `((lambda (,(car kv)) ,(let*$2 (cdr vdefs) body)) ,(cadr kv)))))

(define (let*$3 sym vdefs body)
  (define (inner vdefs)
    (if (null? vdefs)
        (if (null? (cdr body)) (car body) (cons 'begin body))
        `((lambda (,(caar vdefs)) ,(inner (cdr vdefs))) ,(caar vdefs))))
    `((lambda ()
       (define (,sym ,@(map1 car vdefs)) ,(inner vdefs))
       (,sym ,@(map1 cadr vdefs)))))

;; }}}
;; {{{ letrec
;; i saw this (define x ()) ... (set! x value) on stackoverflow somewhere

(special (letrec __special_letrec_vdefs__ . __special_letrec_body__)
    (eval (letrec$ __special_letrec_vdefs__ __special_letrec_body__) 1))

(define (letrec$ vdefs body)
  (if (eq? (type vdefs) 'symbol)
      (letrec$3 vdefs (car body) (cdr body))
      (letrec$2 vdefs body)))

(define (letrec$2 vdefs body)
  `((lambda ()
      ,@(map1 (lambda (x) `(define ,(car x) ())) vdefs)
      ,@(map1 (lambda (x) `(set! ,(car x) ,(cadr x))) vdefs)
      ,@body)))

(define (letrec$3 sym vdefs body)
  `((lambda ()
      ,@(map1 (lambda (x) `(define ,(car x) ())) vdefs)
      ,@(map1 (lambda (x) `(set! ,(car x) ,(cadr x))) vdefs)
      (define (,sym ,@(map1 car vdefs)) ,@body)
      (,sym ,@(map1 cadr vdefs)))))

;; }}}
;; {{{ associative table

(define (table compare)
  (define items ())
  (define (table$find items key compare)
    (if (null? items)
        ()
        (if (compare (car (car items)) key)
            (table$find (cdr items) key compare)
            (car items))))
  (define (table$delete items key compare)
      (define prev ())
      (define (helper assoc)
        (if (null? assoc)
            items
            (if (compare (car (car assoc)) key)
                (begin (set! prev assoc) (helper (cdr assoc)))
                (if (null? prev)
                    (cdr assoc)
                    (begin (set-cdr! prev (cdr assoc)) items)))))
    (helper items))
  (define (dispatch m . args)
    (cond ((eq? m 'get) (table$find items (car args) compare))
          ((eq? m 'set)
            (define key (car args))
            (define value (cadr args))
            (define node (table$find items key compare))
            (if (null? node)
                (set! items (cons (cons key value) items))
                (set-cdr! node value)))
          ((eq? m 'del) (set! items (table$delete items (car args) compare)))
          ((eq? m 'known) (table$find items (car args) compare))
          ((eq? m 'setdefault)
            (define key (car args))
            (define value (cadr args))
            (define node (table$find items key compare))
              (if (null? node)
                  (begin
                    (set! items (cons (cons key value) items))
                    value)
                  (cdr node)))
          ((eq? m 'empty?) (null? items))
          ((eq? m 'iter)
            (let ((lst items))
              (lambda ()
                (if (null? lst)
                    ()
                    (begin
                      (define ret (car lst))
                      (set! lst (cdr lst))
                      ret)))))
          ((eq? m 'len) (length items))
          ((eq? m 'raw) items)
          (#t (error "unknown method"))))
  dispatch)

;; }}}
;; {{{ last

(define (last lst)
  (if (null? lst)
      (error "(last) needs a nonempty list")
      (let loop
            ((first (car lst))
             (rest (cdr lst)))
        (if (null? rest)
            first
            (loop (car rest) (cdr rest))))))

;; }}}
;; {{{ loop, loop-with-break, for, while, foreach

;; call f in a loop forever
(define (loop f) (f) (loop f))

(define (loop-with-break f)
  (define (break) (c ()))
  (define c (call/cc))
  (if c
      (begin
        (define c2 (call/cc))
        (f break)
        (c2 c2))
      ()))

(define (while f) (if (f) (while f) ()))

;; call f a given number of times as (f counter)
(define (for f start stop step)
  (define op <)
  (define (for$ start)
    (if (op start stop)
        (begin
          (f start)
          (for$ (- start (- step))))))
    (cond
        ((equal? start stop) ())
        ((< 0 step)
          (if (< stop start)
              (error "bad step")))
        ((< step 0)
          (if (< start stop)
              (error "bad step")
              (set! op >)))
        (#t (error "step must be nonzero")))
  (for$ start))

(define (for f start stop step)
  (define op <)
  (define (for$ start)
    (if (op start stop)
          (begin
            (f start)
            (for$ (- start (- step))))))
    (cond
        ((< 0 step)
          (if (< stop start)
              (error "bad step")))
        ((< step 0)
          (if (< start stop)
              (error "bad step")
              (set! op >)))
        (#t (error "step must be nonzero")))
  (for$ start))

(define (foreach f lst)
  (define (loop f _ lst)
    (if (null? lst)
        ()
        (loop f (f (car lst)) (cdr lst))))
  (loop f () lst))

;; }}}
;; {{{ iterate (compose with itself) a function

(define (iter-func f x0 n)
  (if (< n 1) x0 (iter-func f (f x0) (- n 1)))
)

;; }}}
;; {{{ benchmarking

(define (timeit f n)
  (define (loop i)
    (if (< i n) (begin (f i) (loop (- i -1))) ()))
  (define t0 (time 'time))
  (loop 0)
  (define t1 (time 'time))
  (define dt (- t1 t0))
  (if (< dt 1e-7) (set! dt 1e-7) ())
  (if (< n 1) (set! n 1) ())
  (list n dt (* 1e6 (/ dt n)) (/ n dt)))

;; }}}
;; {{{ gcd

(define (gcd x y)
  (define (gcd$ x y)
    (if (equal? y 0)
        x
        (gcd$ y (mod x y))))
  (cond ((lt? x y) (gcd y x))
        ((equal? x 0) 1)
        (#t (gcd$ x y))))

;; }}}
;; {{{ misc

(special (no-op . args) ())  ;; replace no-op with begin to execute args

;; }}}
;; {{{ scheme-ish

(define = equal?)
(define else #t)
(define mapcar map1)

;; }}}

;; EOF
