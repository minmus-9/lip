;;;; from https://www.dreamsongs.com/Files/Timrep.pdf

(define (atom x) (or (atom? x) (eq? (type x) 'integer)))
(define eq eq?)
(define t #t)

(special (defun sym args . body)
 (eval `(define ,(cons sym args) ,@body) 1))

(defun deriv-aux (a) (list '/ (deriv a) a))

(defun deriv (a)
  (cond ((atom a)
          (if (eq a 'x) 1 0))
        ((eq (car a) '+)
          (cons '+ (mapcar deriv (cdr a))))
        ((eq (car a) '-)
          (cons '- (mapcar deriv (cdr a))))
        ((eq (car a) '*)
          (list '* a (cons '+ (mapcar deriv-aux (cdr a)))))
        ((eq (car a) '/)
          (list '-
              (list '/ (deriv (cadr a)) (caddr a))
              (list '/
                  (cadr a)
                  (list '* (caddr a) (caddr a) (deriv (caddr a))))))
        (t 'error)))

                           (print '(+ (* 3 x x) (* a x x) (* b x) 5))
                    (print (deriv '(+ (* 3 x x) (* a x x) (* b x) 5)))
(timeit (lambda (_) (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))) 5000)

;;;; EOF
