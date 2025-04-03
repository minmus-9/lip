;;;; sicp chapter 5 register machine

(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (foreach
      (lambda (register-name)
        (machine 'allocate-register register-name))
      register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) (set! contents value)))
            (else (error "unknown message"))))
    dispatch))

(define (get-contents register) (register 'get))
(define (set-contents! register value)
  ((register 'set) value))

(define (make-stack)
  (let ((s ()))
    (define (push x) (set! s (cons x s)))
    (define (pop)
      (if (null? s)
          (error "stack empty")
          (let ((top (car s)))
            (set! s (cdr s))
            top)))
    (define (initialize) (set! s ()))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            (else (error "unknnown message"))))
    dispatch))

(define (pop stack) (stack 'pop))
(define (push stack value) ((stack 'push) value))



;;;; EOF
