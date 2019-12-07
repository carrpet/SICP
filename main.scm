;;; small testing framework

(define (reportmsg msg)
  (display msg)
	(newline))


(define (reporterr msg)
  (display "ERROR: ")
	(display msg)
	(newline))


(define (assert msg b)
  ( if ( not b) (reporterr msg)))

(define (assert-lists-eq a b)
  (if (not (equal? a b)) (reporterr "list contents not equal")))

(define (asserteq msg a b)
  ( assert msg ( > 0.0001 (abs ( - a b)))))

;;; Section 2.3.2 Symbolic Differentiation

;;; Representation of symbolic Differentiation
;;; defined in the book


(define (sum? x) (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s)) ;;Addend of the sum e.
(define (augend s) (caddr s));;Augend of the sum e.
(define (make-sum a1 a2) (list '+ a1 a2)) 
(define (product? x) (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p)) ;;Multiplier of the product e.
(define (multiplicand p) (caddr p))
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
          (+ a1 a2))
        (else (list '+ a1 a2))))


(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (deriv exp var)
  (cond ((number? exp) 0)
    ((variable? exp) (if (same-variable? exp var) 1 0))
    ((sum? exp) (make-sum (deriv (addend exp) var)
                          (deriv (augend exp) var)))
    ((product? exp)
      (make-sum
        (make-product (multiplier exp)
                      (deriv (multiplicand exp) var))
        (make-product (deriv (multiplier exp) var)
                      (multiplicand exp))))
    ((exponentiation? exp)
      (make-product (exponent exp)
                    (make-product
                      (make-exponentiation 
                        (base exp) 
                        (make-difference (exponent exp)
                                          1))
                      (deriv (base exp) var))))
    (else
      (error "unknown expression type: DERIV" exp))))

;;; Exercise 2.56 - Implement the differentiation rules for exponentiation

(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))

(define (base exp) (cadr exp))
(define (exponent exp) (caddr exp))
(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((and (number? b) (number? e)) (expr b e))
        (else (list '** b e))))

;; needed for the exponentiation clause in deriv
(define (make-difference a1 a2)
  (cond ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
          (- a1 a2))
        (else (list '- a1 a2))))

;;; tests for exponentiation
(assert-lists-eq (deriv '(** x 2) 'x) '(* 2 x))

