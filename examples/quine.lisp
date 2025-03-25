;;;; quines from various places

;;; from https://okmij.org/ftp/meta-programming/Quines.html

(define (ff) ((lambda (f) `(,f ',f)) '(lambda (f) `(,f ',f))))

(print ff)
(print (eval ff))

(define (ff1)
  ((lambda (g) `(,g (,`quasiquote ,g))) 
  `(lambda (g) `(,g (,`quasiquote ,g)))))

(print ff1)
(print (eval ff1))




;;; from https://literateprograms.org/quine__lisp_.html
(print)
(define (f)
  ((lambda (x) (list x (list 'quote x))) '(lambda (x) (list x (list 'quote x)))))
(print f)
(print (eval f))




;;; from https://www.nyx.net/~gthompso/self_lisp.txt
(print)
(define (f)
  (let ((a '(list 'let (list (list 'a (list 'quote a))) a))) (list 'let (list (list 'a (list 'quote a))) a)))

(print f)
(print (eval f))

;;

(print)
(define (f)
  (let ((a '(list 'let (list (list 'a (list 'quote a))) a)))
  `(let ((a (quote ,a))) ,a)))

(print f)
(print (eval f))

;;

(print)
(define (f)
  ((lambda (x) `(,x ',x)) '(lambda (x) `(,x ',x))))

(print f)
(print (eval f))

;;

(print)
(define (f)
  ((lambda (x) (list `',x)) '(lambda (x) (list x `',x))))

(print f)
(print (eval f))

;;

(print)
(define (f)
  ((lambda (list) (list list `',list)) '(lambda (list) (list list `',list))))

(print f)
(print (eval f))

;;; note from source page: Author: John McCarthy(creator of the language) and Carolyn Talcott

(print)
(define (f)
  ((lambda (x)
    (list x (list (quote quote) x)))
  (quote
   (lambda (x)
     (list x (list (quote quote) x))))))

(print f)
(print (eval f))

;;

(print)
(define (f)
((lambda ()
    ((lambda (Y)
        (list
           (list
              (quote lambda)
              ()
              (list
                 Y
                 (list
                    (quote quote)
                    Y)))))
      (quote
         (lambda (Y)
            (list
               (list
                  (quote lambda)
                  ()
                  (list
                     Y
                     (list
                        (quote quote)
                         Y))))))))))


(print f)
(print (eval f))

;;

(print)
(define (f)
  ((lambda (lambda) `(,lambda ',lambda)) '(lambda (lambda) `(,lambda ',lambda))))

(print f)
(print (eval f))

;;;; EOF
