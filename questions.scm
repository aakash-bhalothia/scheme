(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cddr x) (cdr (cdr x)))
(define (cadar x) (car (cdr (car x))))

; Some utility functions that you may find useful to implement.
(define (map proc items)
    (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (cons-all first rests)
  (cond
    ((null? rests) nil)
    (else (cons (cons first (car rests)) (cons-all first (cdr rests))))
    )
)

(define (zip pairs)
  (cond 
    ((null? pairs) (list nil nil))
    (else 
      (cons (map (lambda (x) (car x)) pairs) (cons (map (lambda (x) (car (cdr x))) pairs) nil)))))

;; Problem 18
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN Question 18
  (define (helper x s)
  (cond
    ((null? s) nil)
    (else (cons (list x (car s)) (helper (+ x 1) (cdr s))))
    )
  )(helper 0 s))
  ; END Question 18

;; Problem 19
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN Question 19
  (define (helper x newtotal)
  (cond
    ((null? x) nil)
    ((> (car x) newtotal) (helper (cdr x) newtotal))
    ((< (car x) newtotal) (append (cons-all (car x) (helper x (- newtotal (car x)))) (helper (cdr x) newtotal)))
    (else (cons (list (car x)) (helper (cdr x) newtotal)))
    )) (helper denoms total))
  ; END Question 19

;; Problem 20
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (analyze expr)
  (cond ((atom? expr)
         expr
         )
        ((quoted? expr)
         expr
         )
        ((or (lambda? expr)
             (define? expr))
         (let ((form   (car expr))
               (params (cadr expr))
               (body   (cddr expr)))
           (cons form (cons params (analyze body)))
           ))
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))
         (define formals (car (zip values)))
         (define args (car (cdr (zip values))))
         (cons (cons 'lambda (cons formals (analyze body))) (analyze args))
           ))
        (else
         (map analyze expr)
          )
         )))

;; Problem 21 (optional)
;; Draw the hax image using turtle graphics.
(define (hax d k)
  ; BEGIN Question 21
  'REPLACE-THIS-LINE
  )
  ; END Question 21

