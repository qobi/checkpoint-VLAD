#!r6rs

(library
 (vlad)
 (export vlad vlad-I)
 (import (rnrs) (rnrs r5rs (6)) (ikarus))

 (define *d* 3)

 (define *t* 5)

 (define *mid* #f)

 ;; Must be such that dividing it by two and rounding down does not become <=4
 ;; because that would checkpoint during a preamble.
 (define *base-case-duration* 10)

 (define infinity (/ 1.0 0.0))

 (define *e* 0)

 (define <_e <)

 (define-record-type dual-number (fields epsilon primal tangent))

 (define-record-type
  tape
  (fields epsilon primal factors tapes (mutable fanout) (mutable cotangent)))

 (define (new-tape epsilon primal factors tapes)
  (make-tape epsilon primal factors tapes 0 0))

 (define (tapify x) (new-tape *e* x '() '()))

 (define (lift-real->real f df/dx)
  (letrec ((self (lambda (x)
		  (cond ((dual-number? x)
			 (make-dual-number (dual-number-epsilon x)
					   (self (dual-number-primal x))
					   (d* (df/dx (dual-number-primal x))
					       (dual-number-tangent x))))
			((tape? x)
			 (new-tape (tape-epsilon x)
				   (self (tape-primal x))
				   (list (df/dx (tape-primal x)))
				   (list x)))
			(else (f x))))))
   self))

 (define (lift-real*real->real f df/dx1 df/dx2)
  (letrec ((self
	    (lambda (x1 x2)
	     (cond
	      ((dual-number? x1)
	       (cond
		((dual-number? x2)
		 (cond
		  ((<_e (dual-number-epsilon x1)
			(dual-number-epsilon x2))
		   (make-dual-number (dual-number-epsilon x2)
				     (self x1 (dual-number-primal x2))
				     (d* (df/dx2 x1 (dual-number-primal x2))
					 (dual-number-tangent x2))))
		  ((<_e (dual-number-epsilon x2)
			(dual-number-epsilon x1))
		   (make-dual-number (dual-number-epsilon x1)
				     (self (dual-number-primal x1) x2)
				     (d* (df/dx1 (dual-number-primal x1) x2)
					 (dual-number-tangent x1))))
		  (else
		   (make-dual-number
		    (dual-number-epsilon x1)
		    (self (dual-number-primal x1) (dual-number-primal x2))
		    (d+ (d* (df/dx1 (dual-number-primal x1)
				    (dual-number-primal x2))
			    (dual-number-tangent x1))
			(d* (df/dx2 (dual-number-primal x1)
				    (dual-number-primal x2))
			    (dual-number-tangent x2)))))))
		((tape? x2)
		 (if (<_e (dual-number-epsilon x1) (tape-epsilon x2))
		     (new-tape (tape-epsilon x2)
			       (self x1 (tape-primal x2))
			       (list (df/dx2 x1 (tape-primal x2)))
			       (list x2))
		     (make-dual-number (dual-number-epsilon x1)
				       (self (dual-number-primal x1) x2)
				       (d* (df/dx1 (dual-number-primal x1) x2)
					   (dual-number-tangent x1)))))
		(else (make-dual-number (dual-number-epsilon x1)
					(self (dual-number-primal x1) x2)
					(d* (df/dx1 (dual-number-primal x1) x2)
					    (dual-number-tangent x1))))))
	      ((tape? x1)
	       (cond
		((dual-number? x2)
		 (if (<_e (tape-epsilon x1) (dual-number-epsilon x2))
		     (make-dual-number (dual-number-epsilon x2)
				       (self x1 (dual-number-primal x2))
				       (d* (df/dx2 x1 (dual-number-primal x2))
					   (dual-number-tangent x2)))
		     (new-tape (tape-epsilon x1)
			       (self (tape-primal x1) x2)
			       (list (df/dx1 (tape-primal x1) x2))
			       (list x1))))
		((tape? x2)
		 (cond
		  ((<_e (tape-epsilon x1) (tape-epsilon x2))
		   (new-tape (tape-epsilon x2)
			     (self x1 (tape-primal x2))
			     (list (df/dx2 x1 (tape-primal x2)))
			     (list x2)))
		  ((<_e (tape-epsilon x2) (tape-epsilon x1))
		   (new-tape (tape-epsilon x1)
			     (self (tape-primal x1) x2)
			     (list (df/dx1 (tape-primal x1) x2))
			     (list x1)))
		  (else
		   (new-tape (tape-epsilon x1)
			     (self (tape-primal x1) (tape-primal x2))
			     (list (df/dx1 (tape-primal x1) (tape-primal x2))
				   (df/dx2 (tape-primal x1) (tape-primal x2)))
			     (list x1 x2)))))
		(else (new-tape (tape-epsilon x1)
				(self (tape-primal x1) x2)
				(list (df/dx1 (tape-primal x1) x2))
				(list x1)))))
	      (else
	       (cond ((dual-number? x2)
		      (make-dual-number (dual-number-epsilon x2)
					(self x1 (dual-number-primal x2))
					(d* (df/dx2 x1 (dual-number-primal x2))
					    (dual-number-tangent x2))))
		     ((tape? x2)
		      (new-tape (tape-epsilon x2)
				(self x1 (tape-primal x2))
				(list (df/dx2 x1 (tape-primal x2)))
				(list x2)))
		     (else (f x1 x2))))))))
   self))

 (define (fold f l)
  (let loop ((l (cdr l)) (c (car l)))
   (if (null? l) c (loop (cdr l) (f c (car l))))))

 (define (lift-real^n->real f df/dx1 df/dx2)
  (lambda xs
   (if (null? xs) (f) (fold (lift-real*real->real f df/dx1 df/dx2) xs))))

 (define (lift-real^n+1->real f df/dx df/dx1 df/dx2)
  (lambda xs
   (cond ((null? xs) (f))
	 ((null? (cdr xs)) ((lift-real->real f df/dx) (car xs)))
	 (else (fold (lift-real*real->real f df/dx1 df/dx2) xs)))))

 (define (primal* x)
  (cond ((dual-number? x) (primal* (dual-number-primal x)))
	((tape? x) (primal* (tape-primal x)))
	(else x)))

 (define (lift-real^n->boolean f) (lambda xs (apply f (map primal* xs))))

 (define d+ (lift-real^n->real + (lambda (x1 x2) 1) (lambda (x1 x2) 1)))

 (define d- (lift-real^n+1->real
	     - (lambda (x) -1) (lambda (x1 x2) 1) (lambda (x1 x2) -1)))

 (define d* (lift-real^n->real * (lambda (x1 x2) x2) (lambda (x1 x2) x1)))

 (define d/ (lift-real^n+1->real
	     /
	     (lambda (x) (d- (d/ (d* x x))))
	     (lambda (x1 x2) (d/ x2))
	     (lambda (x1 x2) (d- (d/ x1 (d* x2 x2))))))

 (define dsqrt (lift-real->real sqrt (lambda (x) (d/ (d* 2 (dsqrt x))))))

 (define dexp (lift-real->real exp (lambda (x) (dexp x))))

 (define dlog (lift-real->real log (lambda (x) (d/ x))))

 (define dsin (lift-real->real sin (lambda (x) (dcos x))))

 (define dcos (lift-real->real cos (lambda (x) (d- (dsin x)))))

 (define (datan . xs)
  (cond ((null? xs) (apply atan xs))
	((null? (cdr xs)) (datan (car xs) 1))
	((null? (cdr (cdr xs)))
	 ((lift-real*real->real
	   atan
	   (lambda (x1 x2) (d/ x2 (d+ (d* x1 x1) (d* x2 x2))))
	   (lambda (x1 x2) (d/ (d- x1) (d+ (d* x1 x1) (d* x2 x2)))))
	  (car xs)
	  (cadr xs)))
	(else (apply atan xs))))

 (define d= (lift-real^n->boolean =))

 (define d< (lift-real^n->boolean <))

 (define d> (lift-real^n->boolean >))

 (define d<= (lift-real^n->boolean <=))

 (define d>= (lift-real^n->boolean >=))

 (define dzero? (lift-real^n->boolean zero?))

 (define dpositive? (lift-real^n->boolean positive?))

 (define dnegative? (lift-real^n->boolean negative?))

 (define dreal? (lift-real^n->boolean real?))

 (define (determine-fanout! tape)
  (tape-fanout-set! tape (+ (tape-fanout tape) 1))
  (when (= (tape-fanout tape) 1)
   (for-each determine-fanout! (tape-tapes tape))))

 (define (initialize-cotangent! tape)
  (tape-cotangent-set! tape 0)
  (tape-fanout-set! tape (- (tape-fanout tape) 1))
  (when (zero? (tape-fanout tape))
   (for-each initialize-cotangent! (tape-tapes tape))))

 (define (reverse-phase! cotangent tape)
  (tape-cotangent-set! tape (d+ (tape-cotangent tape) cotangent))
  (tape-fanout-set! tape (- (tape-fanout tape) 1))
  (when (zero? (tape-fanout tape))
   (let ((cotangent (tape-cotangent tape)))
    (for-each
     (lambda (factor tape) (reverse-phase! (d* cotangent factor) tape))
     (tape-factors tape)
     (tape-tapes tape)))))

;;;\needswork: We don't provide (co)tangent vector mode.
;;;\needswork: We don't provide the ability to choose y-cotangent after
;;;            computing y.
;;;\needswork: We don't provide the ability to perform the (co)tangent
;;;            computation multiple times after a single primal computation.

;;; This evaluator has all of the same vagaries as the Stalingrad evaluator:
;;;   constant expressions instead of constant conversion
;;;   if-procedure (it no longer exposes and shadows this)
;;;   parameters
;;;   cons is syntax
;;;   +, -, *, /, atan, =, <, >, <=, and >= are binary
;;;   generalized define
;;;   can't define non-lambdas

 ;; paper: c
 (define-record-type il:constant-expression (fields value))

 ;; paper: x
 (define-record-type il:variable-access-expression (fields variable))

 ;; paper: x e
 (define-record-type il:lambda-expression (fields parameter expression))

 ;; paper: e
 (define-record-type il:application (fields expression1 expression2))

 ;; paper: e
 (define-record-type il:unary-expression (fields procedure expression))

 (define-record-type il:binary-expression
  ;; paper: e
  (fields procedure expression1 expression2))

 (define-record-type il:ternary-expression
  ;; paper: e
  (fields procedure expression1 expression2 expression3))

 (define-record-type il:letrec-expression
  (fields variables expressions expression))

 (define-record-type il:checkpoint-expression
  ;; paper: e
  (fields expression1 expression2 expression3))

 ;; paper: e
 (define-record-type il:resume-expression (fields expression))

 (define-record-type il:binding (fields variable value))

 (define-record-type il:pair (fields cache car cdr))

 (define-record-type il:nonrecursive-closure
  (fields cache expression environment))

 (define-record-type il:recursive-closure
  (fields cache variables expressions index environment))

 (define-record-type il:continuation (fields cache id procedure values))

 (define-record-type il:checkpoint (fields cache continuation closure))

 (define (il:cons value1 value2) (make-il:pair #f value1 value2))

 (define (il:car value) (il:pair-car value))

 (define (il:cdr value) (il:pair-cdr value))

 (define (il:make-nonrecursive-closure expression environment)
  (make-il:nonrecursive-closure #f expression environment))

 (define (il:make-recursive-closure variables expressions index environment)
  (make-il:recursive-closure #f variables expressions index environment))

 (define (il:closure? thing)
  (or (il:nonrecursive-closure? thing) (il:recursive-closure? thing)))

 (define (il:make-continuation id procedure . values)
  (make-il:continuation #f id procedure values))

 (define (il:make-checkpoint continuation closure)
  (make-il:checkpoint #f continuation closure))

;;; j*, *j, and checkpoint-*j are implemented as ternary expressions with
;;; il:j*, il:*j, and il:checkpoint-*j as the procedure.

 (define (il:externalize-expression expression)
  (cond
   ((il:constant-expression? expression)
    (il:constant-expression-value expression))
   ((il:variable-access-expression? expression)
    (il:variable-access-expression-variable expression))
   ((il:lambda-expression? expression)
    `(lambda (,(il:externalize-expression
		(il:lambda-expression-parameter expression)))
      ,(il:externalize-expression
	(il:lambda-expression-expression expression))))
   ((il:application? expression)
    `(,(il:externalize-expression (il:application-expression1 expression))
      ,(il:externalize-expression (il:application-expression2 expression))))
   ((il:unary-expression? expression)
    `(,(il:unary-expression-procedure expression)
      ,(il:externalize-expression (il:unary-expression-expression expression))))
   ((il:binary-expression? expression)
    (if (eq? (il:binary-expression-procedure expression) il:cons)
	`(cons ,(il:externalize-expression
		 (il:binary-expression-expression1 expression))
	       ,(il:externalize-expression
		 (il:binary-expression-expression2 expression)))
	`(,(il:binary-expression-procedure expression)
	  ,(il:externalize-expression
	    (il:binary-expression-expression1 expression))
	  ,(il:externalize-expression
	    (il:binary-expression-expression2 expression)))))
   ((il:ternary-expression? expression)
    `(,(il:ternary-expression-procedure expression)
      ,(il:externalize-expression
	(il:ternary-expression-expression1 expression))
      ,(il:externalize-expression
	(il:ternary-expression-expression2 expression))
      ,(il:externalize-expression
	(il:ternary-expression-expression3 expression))))
   ((il:letrec-expression? expression)
    `(letrec ,(map (lambda (variable expression)
		    `(,variable ,(il:externalize-expression expression)))
		   (il:letrec-expression-variables expression)
		   (il:letrec-expression-expressions expression))
      ,(il:externalize-expression
	(il:letrec-expression-expression expression))))
   ((il:checkpoint-expression? expression)
    `(checkpoint ,(il:externalize-expression
		   (il:checkpoint-expression-expression1 expression))
		 ,(il:externalize-expression
		   (il:checkpoint-expression-expression2 expression))
		 ,(il:externalize-expression
		   (il:checkpoint-expression-expression3 expression))))
   ((il:resume-expression? expression)
    `(resume
      ,(il:externalize-expression
	(il:resume-expression-expression expression))))
   (else (internal-error))))

 (define (il:externalize x)
  (cond
   ((eq? x #t) #t)
   ((eq? x #f) #f)
   ((null? x) '())
   ((dreal? x) x)
   ((il:pair? x) (cons (il:externalize (il:car x)) (il:externalize (il:cdr x))))
   ((il:binding? x)
    `(binding ,(il:binding-variable x)
	      ,(il:externalize (il:binding-value x))))
   ((il:nonrecursive-closure? x)
    `(nonrecursive-closure
      ,(il:externalize-expression (il:nonrecursive-closure-expression x))
      ,(map il:externalize (il:nonrecursive-closure-environment x))))
   ((il:recursive-closure? x)
    `(recursive-closure
      ,(il:recursive-closure-variables x)
      ,(map il:externalize-expression (il:recursive-closure-expressions x))
      ,(il:recursive-closure-index x)
      ,(map il:externalize (il:recursive-closure-environment x))))
   ((il:continuation? x)
    `(continuation ,(il:continuation-id x)
		   ;;\needswork: This no longer works because
		   ;; il:continuation-values is a Scheme pair not an il:pair.
		   ,(il:externalize (il:continuation-values x))))
   ((il:checkpoint? x)
    `(checkpoint ,(il:externalize (il:checkpoint-continuation x))
		 ,(il:externalize (il:checkpoint-closure x))))
   (else (internal-error))))

 (define (il:lookup-binding variable environment)
  (cond ((null? environment) (internal-error))
	((eq? variable (il:binding-variable (first environment)))
	 (first environment))
	(else (il:lookup-binding variable (rest environment)))))

 (define (il:lookup variable environment)
  (il:binding-value (il:lookup-binding variable environment)))

 (define (il:free-variables expression)
  (cond
   ((il:constant-expression? expression) '())
   ((il:variable-access-expression? expression)
    (list (il:variable-access-expression-variable expression)))
   ((il:lambda-expression? expression)
    (set-differenceq
     (il:free-variables (il:lambda-expression-expression expression))
     (il:free-variables (il:lambda-expression-parameter expression))))
   ((il:application? expression)
    (unionq (il:free-variables (il:application-expression1 expression))
	    (il:free-variables (il:application-expression2 expression))))
   ((il:unary-expression? expression)
    (il:free-variables (il:unary-expression-expression expression)))
   ((il:binary-expression? expression)
    (unionq (il:free-variables (il:binary-expression-expression1 expression))
	    (il:free-variables (il:binary-expression-expression2 expression))))
   ((il:ternary-expression? expression)
    (unionq
     (il:free-variables (il:ternary-expression-expression1 expression))
     (unionq
      (il:free-variables (il:ternary-expression-expression2 expression))
      (il:free-variables (il:ternary-expression-expression3 expression)))))
   ((il:letrec-expression? expression)
    (set-differenceq
     (unionq (map-reduce unionq
			 '()
			 il:free-variables
			 (il:letrec-expression-expressions expression))
	     (il:free-variables (il:letrec-expression-expression expression)))
     (il:letrec-expression-variables expression)))
   ((il:checkpoint-expression? expression)
    (unionq
     (il:free-variables (il:checkpoint-expression-expression1 expression))
     (unionq
      (il:free-variables (il:checkpoint-expression-expression2 expression))
      (il:free-variables (il:checkpoint-expression-expression3 expression)))))
   ((il:resume-expression? expression)
    (il:free-variables (il:resume-expression-expression expression)))
   (else (internal-error))))

 (define (il:restrict environment expression)
  (map (lambda (variable) (il:lookup-binding variable environment))
       (il:free-variables expression)))

 (define (il:expression=? expression1 expression2)
  (or (and (il:constant-expression? expression1)
	   (il:constant-expression? expression2)
	   (equal? (il:constant-expression-value expression1)
		   (il:constant-expression-value expression2)))
      (and (il:variable-access-expression? expression2)
	   (il:variable-access-expression? expression2)
	   (eq? (il:variable-access-expression-variable expression1)
		(il:variable-access-expression-variable expression2)))
      (and (il:lambda-expression? expression1)
	   (il:lambda-expression? expression2)
	   (il:expression=? (il:lambda-expression-parameter expression1)
			    (il:lambda-expression-parameter expression2))
	   (il:expression=? (il:lambda-expression-expression expression1)
			    (il:lambda-expression-expression expression2)))
      (and (il:application? expression1)
	   (il:application? expression2)
	   (il:expression=? (il:application-expression1 expression1)
			    (il:application-expression1 expression2))
	   (il:expression=? (il:application-expression2 expression1)
			    (il:application-expression2 expression2)))
      (and (il:unary-expression? expression1)
	   (il:unary-expression? expression2)
	   (eq? (il:unary-expression-procedure expression1)
		(il:unary-expression-procedure expression2))
	   (il:expression=? (il:unary-expression-expression expression1)
			    (il:unary-expression-expression expression2)))
      (and (il:binary-expression? expression1)
	   (il:binary-expression? expression2)
	   (eq? (il:binary-expression-procedure expression1)
		(il:binary-expression-procedure expression2))
	   (il:expression=? (il:binary-expression-expression1 expression1)
			    (il:binary-expression-expression1 expression2))
	   (il:expression=? (il:binary-expression-expression2 expression1)
			    (il:binary-expression-expression2 expression2)))
      (and (il:ternary-expression? expression1)
	   (il:ternary-expression? expression2)
	   (eq? (il:ternary-expression-procedure expression1)
		(il:ternary-expression-procedure expression2))
	   (il:expression=? (il:ternary-expression-expression1 expression1)
			    (il:ternary-expression-expression1 expression2))
	   (il:expression=? (il:ternary-expression-expression2 expression1)
			    (il:ternary-expression-expression2 expression2))
	   (il:expression=? (il:ternary-expression-expression3 expression1)
			    (il:ternary-expression-expression3 expression2)))
      (and (il:letrec-expression? expression1)
	   (il:letrec-expression? expression2)
	   (= (length (il:letrec-expression-variables expression1))
	      (length (il:letrec-expression-variables expression2)))
	   (every eq?
		  (il:letrec-expression-variables expression1)
		  (il:letrec-expression-variables expression2))
	   (= (length (il:letrec-expression-expressions expression1))
	      (length (il:letrec-expression-expressions expression2)))
	   (every il:expression=?
		  (il:letrec-expression-expressions expression1)
		  (il:letrec-expression-expressions expression2))
	   (il:expression=? (il:letrec-expression-expression expression1)
			    (il:letrec-expression-expression expression2)))
      (and (il:checkpoint-expression? expression1)
	   (il:checkpoint-expression? expression2)
	   (il:expression=? (il:checkpoint-expression-expression1 expression1)
			    (il:checkpoint-expression-expression1 expression2))
	   (il:expression=? (il:checkpoint-expression-expression2 expression1)
			    (il:checkpoint-expression-expression2 expression2))
	   (il:expression=? (il:checkpoint-expression-expression3 expression1)
			    (il:checkpoint-expression-expression3 expression2)))
      (and (il:resume-expression? expression1)
	   (il:resume-expression? expression2)
	   (il:expression=? (il:resume-expression-expression expression1)
			    (il:resume-expression-expression expression2)))))

 (define (il:walk1 f x)
  ;;\needswork: Not safe for space.
  (cond ((eq? x #t) #t)
	((eq? x #f) #f)
	((null? x) '())
	((dreal? x) (f x))
	((il:pair? x) (il:cons (il:walk1 f (il:car x)) (il:walk1 f (il:cdr x))))
	((il:nonrecursive-closure? x)
	 (il:make-nonrecursive-closure
	  (il:nonrecursive-closure-expression x)
	  (map (lambda (b)
		(make-il:binding (il:binding-variable b)
				 (il:walk1 f (il:binding-value b))))
	       (il:nonrecursive-closure-environment x))))
	((il:recursive-closure? x)
	 (il:make-recursive-closure
	  (il:recursive-closure-variables x)
	  (il:recursive-closure-expressions x)
	  (il:recursive-closure-index x)
	  (map (lambda (b)
		(make-il:binding (il:binding-variable b)
				 (il:walk1 f (il:binding-value b))))
	       (il:recursive-closure-environment x))))
	((il:continuation? x)
	 (apply il:make-continuation
		(il:continuation-id x)
		(il:continuation-procedure x)
		(map (lambda (x) (il:walk1 f x)) (il:continuation-values x))))
	((il:checkpoint? x)
	 (il:make-checkpoint (il:walk1 f (il:checkpoint-continuation x))
			     (il:walk1 f (il:checkpoint-closure x))))
	(else (internal-error))))

 (define (il:walk2 f x x-prime)
  ;;\needswork: Not safe for space.
  (cond
   ((and (eq? x #t) (eq? x-prime #t)) #t)
   ((and (eq? x #f) (eq? x-prime #f)) #f)
   ((and (null? x) (null? x-prime)) '())
   ((and (dreal? x) (dreal? x-prime)) (f x x-prime))
   ((and (il:pair? x) (il:pair? x-prime))
    (il:cons (il:walk2 f (il:car x) (il:car x-prime))
	     (il:walk2 f (il:cdr x) (il:cdr x-prime))))
   ((and (il:nonrecursive-closure? x)
	 (il:nonrecursive-closure? x-prime)
	 (= (length (il:nonrecursive-closure-environment x))
	    (length (il:nonrecursive-closure-environment x-prime)))
	 (every (lambda (b b-prime)
		 (eq? (il:binding-variable b) (il:binding-variable b-prime)))
		(il:nonrecursive-closure-environment x)
		(il:nonrecursive-closure-environment x-prime))
	 (il:expression=? (il:nonrecursive-closure-expression x)
			  (il:nonrecursive-closure-expression x-prime)))
    (il:make-nonrecursive-closure
     (il:nonrecursive-closure-expression x)
     (map (lambda (b b-prime)
	   (make-il:binding
	    (il:binding-variable b)
	    (il:walk2 f (il:binding-value b) (il:binding-value b-prime))))
	  (il:nonrecursive-closure-environment x)
	  (il:nonrecursive-closure-environment x-prime))))
   ((and (il:recursive-closure? x)
	 (il:recursive-closure? x-prime)
	 (= (length (il:recursive-closure-environment x))
	    (length (il:recursive-closure-environment x-prime)))
	 (every (lambda (b b-prime)
		 (eq? (il:binding-variable b) (il:binding-variable b-prime)))
		(il:recursive-closure-environment x)
		(il:recursive-closure-environment x-prime))
	 (= (length (il:recursive-closure-variables x))
	    (length (il:recursive-closure-variables x-prime)))
	 (every eq?
		(il:recursive-closure-variables x)
		(il:recursive-closure-variables x-prime))
	 (= (length (il:recursive-closure-expressions x))
	    (length (il:recursive-closure-expressions x-prime)))
	 (every il:expression=?
		(il:recursive-closure-expressions x)
		(il:recursive-closure-expressions x-prime))
	 (= (il:recursive-closure-index x)
	    (il:recursive-closure-index x-prime)))
    (il:make-recursive-closure
     (il:recursive-closure-variables x)
     (il:recursive-closure-expressions x)
     (il:recursive-closure-index x)
     (map (lambda (b b-prime)
	   (make-il:binding
	    (il:binding-variable b)
	    (il:walk2 f (il:binding-value b) (il:binding-value b-prime))))
	  (il:recursive-closure-environment x)
	  (il:recursive-closure-environment x-prime))))
   ((and (il:continuation? x)
	 (il:continuation? x-prime)
	 (= (length (il:continuation-values x))
	    (length (il:continuation-values x-prime)))
	 ;; Can't check eq? on il:continuation-procedure because c and c` are
	 ;; generated from different calls to il:apply/il:eval.
	 ;; We don't allow mismatched dummies here but do in walk2!.
	 (= (il:continuation-id x) (il:continuation-id x-prime)))
    (apply il:make-continuation
	   (il:continuation-id x)
	   ;; Use the one from x, not x-prime.
	   (il:continuation-procedure x)
	   (map (lambda (x x-prime) (il:walk2 f x x-prime))
		(il:continuation-values x)
		(il:continuation-values x-prime))))
   ((and (il:checkpoint? x) (il:checkpoint? x-prime))
    (il:make-checkpoint (il:walk2 f
				  (il:checkpoint-continuation x)
				  (il:checkpoint-continuation x-prime))
			(il:walk2 f
				  (il:checkpoint-closure x)
				  (il:checkpoint-closure x-prime))))
   (else (run-time-error "Values don't conform: ~s ~s"
			 (il:externalize x)
			 (il:externalize x-prime)))))

 (define (il:walk1! f x)
  (cond ((eq? x #t) #f)
	((eq? x #f) #f)
	((null? x) #f)
	((dreal? x) (f x))
	((il:pair? x) (il:walk1! f (il:car x)) (il:walk1! f (il:cdr x)))
	((il:nonrecursive-closure? x)
	 (for-each (lambda (b) (il:walk1! f (il:binding-value b)))
		   (il:nonrecursive-closure-environment x)))
	((il:recursive-closure? x)
	 (for-each (lambda (b) (il:walk1! f (il:binding-value b)))
		   (il:recursive-closure-environment x)))
	((il:continuation? x)
	 (for-each (lambda (x) (il:walk1! f x)) (il:continuation-values x)))
	((il:checkpoint? x)
	 (il:walk1! f (il:checkpoint-continuation x))
	 (il:walk1! f (il:checkpoint-closure x)))
	(else (internal-error))))

 (define (il:walk2! f x x-prime)
  (cond
   ((and (eq? x #t) (eq? x-prime #t)) #f)
   ((and (eq? x #f) (eq? x-prime #f)) #f)
   ((and (null? x) (null? x-prime)) #f)
   ((and (dreal? x) (dreal? x-prime)) (f x x-prime))
   ((and (il:pair? x) (il:pair? x-prime))
    (il:walk2! f (il:car x) (il:car x-prime))
    (il:walk2! f (il:cdr x) (il:cdr x-prime)))
   ((and (il:nonrecursive-closure? x)
	 (il:nonrecursive-closure? x-prime)
	 (= (length (il:nonrecursive-closure-environment x))
	    (length (il:nonrecursive-closure-environment x-prime)))
	 (every (lambda (b b-prime)
		 (eq? (il:binding-variable b) (il:binding-variable b-prime)))
		(il:nonrecursive-closure-environment x)
		(il:nonrecursive-closure-environment x-prime))
	 (il:expression=? (il:nonrecursive-closure-expression x)
			  (il:nonrecursive-closure-expression x-prime)))
    (for-each (lambda (b b-prime)
	       (il:walk2! f (il:binding-value b) (il:binding-value b-prime)))
	      (il:nonrecursive-closure-environment x)
	      (il:nonrecursive-closure-environment x-prime)))
   ((and (il:recursive-closure? x)
	 (il:recursive-closure? x-prime)
	 (= (length (il:recursive-closure-environment x))
	    (length (il:recursive-closure-environment x-prime)))
	 (every (lambda (b b-prime)
		 (eq? (il:binding-variable b) (il:binding-variable b-prime)))
		(il:recursive-closure-environment x)
		(il:recursive-closure-environment x-prime))
	 (= (length (il:recursive-closure-variables x))
	    (length (il:recursive-closure-variables x-prime)))
	 (every eq?
		(il:recursive-closure-variables x)
		(il:recursive-closure-variables x-prime))
	 (= (length (il:recursive-closure-expressions x))
	    (length (il:recursive-closure-expressions x-prime)))
	 (every il:expression=?
		(il:recursive-closure-expressions x)
		(il:recursive-closure-expressions x-prime))
	 (= (il:recursive-closure-index x)
	    (il:recursive-closure-index x-prime)))
    (for-each (lambda (b b-prime)
	       (il:walk2! f (il:binding-value b) (il:binding-value b-prime)))
	      (il:recursive-closure-environment x)
	      (il:recursive-closure-environment x-prime)))
   ((and (il:continuation? x)
	 (il:continuation? x-prime)
	 (= (length (il:continuation-values x))
	    (length (il:continuation-values x-prime)))
	 ;; Can't check eq? on il:continuation-procedure because c and c` are
	 ;; generated from different calls to il:apply/il:eval.
	 (or (= (il:continuation-id x) (il:continuation-id x-prime))
	     ;; We allow mismatched dummies here but not in walk2.
	     (and (= (il:continuation-id x) 13)
		  (= (il:continuation-id x-prime) 15))))
    (for-each (lambda (x x-prime) (il:walk2! f x x-prime))
	      (il:continuation-values x)
	      (il:continuation-values x-prime)))
   ((and (il:checkpoint? x) (il:checkpoint? x-prime))
    (il:walk2! f
	       (il:checkpoint-continuation x)
	       (il:checkpoint-continuation x-prime))
    (il:walk2! f (il:checkpoint-closure x) (il:checkpoint-closure x-prime)))
   (else (run-time-error "Values don't conform: ~s ~s"
			 (il:externalize x)
			 (il:externalize x-prime)))))

 (define (il:call-continuation continuation value count limit)
  (apply (il:continuation-procedure continuation)
	 value
	 count
	 limit
	 (il:continuation-values continuation)))

 (define (il:destructure parameter value)
  ;; removed lambda and letrec parameters
  (cond ((il:constant-expression? parameter)
	 ;; This equal? is OK because a constant expression can't contain a
	 ;; checkpoint or continuation.
	 (unless (equal? (il:constant-expression-value parameter) value)
	  (run-time-error "Argument is not an equivalent value for ~s: ~s"
			  (il:constant-expression-value parameter)
			  value))
	 '())
	((il:variable-access-expression? parameter)
	 (list (make-il:binding
		(il:variable-access-expression-variable parameter) value)))
	((il:binary-expression? parameter)
	 (append (il:destructure
		  (il:binary-expression-expression1 parameter) (il:car value))
		 (il:destructure
		  (il:binary-expression-expression2 parameter) (il:cdr value))))
	(else (internal-error))))

 (define (il:apply continuation value1 value2 count limit)
  ;; paper: k v n l, different order
  (cond ((il:nonrecursive-closure? value1)
	 (il:eval continuation
		  (il:lambda-expression-expression
		   (il:nonrecursive-closure-expression value1))
		  (append (il:destructure
			   (il:lambda-expression-parameter
			    (il:nonrecursive-closure-expression value1))
			   value2)
			  (il:nonrecursive-closure-environment value1))
		  count
		  limit))
	((il:recursive-closure? value1)
	 (il:eval
	  continuation
	  (il:lambda-expression-expression
	   (list-ref (il:recursive-closure-expressions value1)
		     (il:recursive-closure-index value1)))
	  (append (il:destructure
		   (il:lambda-expression-parameter
		    (list-ref (il:recursive-closure-expressions value1)
			      (il:recursive-closure-index value1)))
		   value2)
		  (map-indexed (lambda (variable index)
				(make-il:binding
				 variable
				 (il:make-recursive-closure
				  (il:recursive-closure-variables value1)
				  (il:recursive-closure-expressions value1)
				  index
				  (il:recursive-closure-environment value1))))
			       (il:recursive-closure-variables value1))
		  (il:recursive-closure-environment value1))
	  count
	  limit))
	(else (run-time-error "Not a closure: ~s" value1))))

 (define (assert-that-is-checkpoint value)
  ;; debugging
  (unless (il:checkpoint? value)
   (run-time-error "Not a checkpoint record: ~s" value))
  value)

 (define (il:environment->values environment)
  (if (null? environment)
      '()
      (il:cons (il:binding-value (first environment))
	       (il:environment->values (rest environment)))))

 (define (il:values->environment variables values)
  (if (null? variables)
      (if (null? values)
	  '()
	  (internal-error))
      (if (null? values)
	  (internal-error)
	  (cons (make-il:binding (first variables) (il:car values))
		(il:values->environment (rest variables) (il:cdr values))))))

 (define (il:eval continuation expression environment count limit)
  ;; paper: e rho n l, different order
  (let ((environment (il:restrict environment expression)))
   (cond
    ((il:limit-check count limit)
     (il:make-checkpoint
      continuation
      (il:make-nonrecursive-closure
       (make-il:lambda-expression (make-il:constant-expression '()) expression)
       environment)))
    ((il:constant-expression? expression)
     (il:call-continuation continuation
			   (il:constant-expression-value expression)
			   (il:increment count)
			   limit))
    ((il:variable-access-expression? expression)
     (il:call-continuation
      continuation
      (il:lookup (il:variable-access-expression-variable expression)
		 environment)
      (il:increment count)
      limit))
    ((il:lambda-expression? expression)
     (il:call-continuation continuation
			   (il:make-nonrecursive-closure expression environment)
			   (il:increment count)
			   limit))
    ((il:application? expression)
     (il:eval (il:make-continuation
	       0
	       (lambda (value1 count limit continuation values)
		(il:eval (il:make-continuation
			  1
			  (lambda (value2 count limit continuation value1)
			   (il:apply continuation value1 value2 count limit))
			  continuation
			  value1)
			 (il:application-expression2 expression)
			 (il:values->environment
			  (il:free-variables expression) values)
			 count
			 limit))
	       continuation
	       (il:environment->values environment))
	      (il:application-expression1 expression)
	      environment
	      (il:increment count)
	      limit))
    ((il:unary-expression? expression)
     (il:eval (il:make-continuation
	       2
	       (lambda (value count limit continuation)
		(il:call-continuation
		 continuation
		 ((il:unary-expression-procedure expression) value)
		 count
		 limit))
	       continuation)
	      (il:unary-expression-expression expression)
	      environment
	      (il:increment count)
	      limit))
    ((il:binary-expression? expression)
     (il:eval (il:make-continuation
	       3
	       (lambda (value1 count limit continuation values)
		(il:eval (il:make-continuation
			  4
			  (lambda (value2 count limit continuation value1)
			   (il:call-continuation
			    continuation
			    ((il:binary-expression-procedure expression)
			     value1 value2)
			    count
			    limit))
			  continuation
			  value1)
			 (il:binary-expression-expression2 expression)
			 (il:values->environment
			  (il:free-variables expression) values)
			 count
			 limit))
	       continuation
	       (il:environment->values environment))
	      (il:binary-expression-expression1 expression)
	      environment
	      (il:increment count)
	      limit))
    ((il:ternary-expression? expression)
     (il:eval
      (il:make-continuation
       5
       (lambda (value1 count limit continuation values)
	(il:eval
	 (il:make-continuation
	  6
	  (lambda (value2 count limit continuation values value1)
	   (il:eval (il:make-continuation
		     7
		     (lambda (value3 count limit continuation value1 value2)
		      (il:call-continuation
		       continuation
		       ((il:ternary-expression-procedure expression)
			value1 value2 value3)
		       count
		       limit))
		     continuation
		     value1
		     value2)
		    (il:ternary-expression-expression3 expression)
		    (il:values->environment
		     (il:free-variables expression) values)
		    count
		    limit))
	  continuation
	  values
	  value1)
	 (il:ternary-expression-expression2 expression)
	 (il:values->environment (il:free-variables expression) values)
	 count
	 limit))
       continuation
       (il:environment->values environment))
      (il:ternary-expression-expression1 expression)
      environment
      (il:increment count)
      limit))
    ((il:letrec-expression? expression)
     (il:eval continuation
	      (il:letrec-expression-expression expression)
	      (append
	       (map-indexed (lambda (variable index)
			     (make-il:binding
			      variable
			      (il:make-recursive-closure
			       (il:letrec-expression-variables expression)
			       (il:letrec-expression-expressions expression)
			       index
			       environment)))
			    (il:letrec-expression-variables expression))
	       environment)
	      (il:increment count)
	      limit))
    ((il:checkpoint-expression? expression)
     (il:eval
      (il:make-continuation
       8
       (lambda (value1 count limit continuation values)
	(il:eval
	 (il:make-continuation
	  9
	  (lambda (value2 count limit continuation values value1)
	   (il:eval
	    (il:make-continuation
	     10
	     (lambda (value3 count limit continuation value1 value2)
	      (let ((value3 (primal* value3)))
	       (unless (= count 4)
		(run-time-error "checkpoint input count is not 4: ~s" count))
	       (unless (or (= limit infinity) (> value3 limit))
		(run-time-error
		 "limit not= infinity and value3<=limit: value3=~s limit=~s"
		 value3 limit))
	       (when (= (il:continuation-id continuation) 14) (internal-error))
	       (if (= limit infinity)
		   ;; We used to assert-that-is-checkpoint on the result.
		   ;; But this assertion foils the tail call. And it also
		   ;; breaks treeverse.
		   (il:apply continuation value1 value2 0 value3)
		   (let ((checkpoint
			  (assert-that-is-checkpoint
			   (il:apply continuation value1 value2 0 limit))))
		    (il:make-checkpoint (il:checkpoint-continuation checkpoint)
					(make-closure-for-checkpoint
					 (il:checkpoint-closure checkpoint)
					 (- value3 limit)))))))
	     continuation
	     value1
	     value2)
	    (il:checkpoint-expression-expression3 expression)
	    (il:values->environment (il:free-variables expression) values)
	    count
	    limit))
	  continuation
	  values
	  value1)
	 (il:checkpoint-expression-expression2 expression)
	 (il:values->environment (il:free-variables expression) values)
	 count
	 limit))
       continuation
       (il:environment->values environment))
      (il:checkpoint-expression-expression1 expression)
      environment
      (il:increment count)
      limit))
    ((il:resume-expression? expression)
     (il:eval (il:make-continuation
	       11
	       (lambda (value count limit continuation)
		(unless (= count 2)
		 (run-time-error "resume input count is not 2: ~s" count))
		(il:apply (il:checkpoint-continuation value)
			  (il:checkpoint-closure value)
			  '()
			  0
			  limit))
	       continuation)
	      (il:resume-expression-expression expression)
	      environment
	      (il:increment count)
	      limit))
    (else (internal-error)))))

 (define (forward-mode map-independent map-dependent f x x-tangent)
  (set! *e* (+ *e* 1))
  (let* ((y-forward
	  (f (map-independent (lambda (x x-tangent)
			       (make-dual-number *e* x x-tangent))
			      x
			      x-tangent)))
	 (y (map-dependent
	     (lambda (y-forward)
	      (if (and (dual-number? y-forward)
		       (not (<_e (dual-number-epsilon y-forward) *e*)))
		  (dual-number-primal y-forward)
		  y-forward))
	     y-forward))
	 (y-tangent
	  (map-dependent
	   (lambda (y-forward)
	    (if (and (dual-number? y-forward)
		     (not (<_e (dual-number-epsilon y-forward) *e*)))
		(dual-number-tangent y-forward)
		0))
	   y-forward)))
   (set! *e* (- *e* 1))
   (il:cons y y-tangent)))

 (define (reverse-mode map-independent
		       map-dependent
		       for-each-dependent1!
		       for-each-dependent2!
		       f
		       x
		       y-cotangent)
  (set! *e* (+ *e* 1))
  (let* ((x-reverse (map-independent tapify x))
	 (y-reverse (f x-reverse)))
   (for-each-dependent1!
    (lambda (y-reverse)
     (when (and (tape? y-reverse) (not (<_e (tape-epsilon y-reverse) *e*)))
      (determine-fanout! y-reverse)))
    y-reverse)
   (for-each-dependent1!
    (lambda (y-reverse)
     (when (and (tape? y-reverse) (not (<_e (tape-epsilon y-reverse) *e*)))
      (initialize-cotangent! y-reverse)))
    y-reverse)
   (for-each-dependent1!
    (lambda (y-reverse)
     (when (and (tape? y-reverse) (not (<_e (tape-epsilon y-reverse) *e*)))
      (determine-fanout! y-reverse)))
    y-reverse)
   (for-each-dependent2!
    (lambda (y-reverse y-cotangent)
     (when (and (tape? y-reverse) (not (<_e (tape-epsilon y-reverse) *e*)))
      (reverse-phase! y-cotangent y-reverse)))
    y-reverse
    y-cotangent)
   (let ((x-cotangent (map-independent tape-cotangent x-reverse))
	 (y (map-dependent (lambda (y-reverse)
			    (if (and (tape? y-reverse)
				     (not (<_e (tape-epsilon y-reverse) *e*)))
				(tape-primal y-reverse)
				y-reverse))
			   y-reverse)))
    (set! *e* (- *e* 1))
    (il:cons y x-cotangent))))

 (define *closure-that-resumes*
  (il:make-nonrecursive-closure
   (let ((x (gensym)))
    (make-il:lambda-expression
     (make-il:variable-access-expression x)
     (make-il:resume-expression (make-il:variable-access-expression x))))
   '()))

 (define *lambda-expression-for-checkpoint*
  (let ((x (gensym)))
   (make-il:lambda-expression
    (make-il:variable-access-expression x)
    (make-il:checkpoint-expression
     (make-il:variable-access-expression 'value1)
     (make-il:variable-access-expression x)
     (make-il:variable-access-expression 'limit)))))

 (define (make-closure-for-checkpoint value1 limit)
  ;; paper: f l
  (il:make-nonrecursive-closure
   *lambda-expression-for-checkpoint*
   (list (make-il:binding 'value1 value1) (make-il:binding 'limit limit))))

 (define (il:j* value1 value2 value3)
  ;; paper: v
  (forward-mode
   il:walk2
   il:walk1
   (lambda (x)
    (il:apply (il:make-continuation 12 (lambda (value count limit) value))
	      value1 x 0 infinity))
   value2
   value3))

 (define (il:*j value1 value2 value3)
  ;; paper: v
  (reverse-mode
   il:walk1
   il:walk1
   il:walk1!
   il:walk2!
   (lambda (x)
    (il:apply (il:make-continuation 13 (lambda (value count limit) value))
	      value1 x 0 infinity))
   value2
   value3))

 (define (primops value1 value2)
  ;; paper: f x
  (let ((result
	 (il:apply (il:make-continuation 14 (lambda (value count limit) count))
		   value1 value2 0 infinity)))
   ;; debugging
   (when (il:checkpoint? result) (internal-error))
   result))

 (define (checkpoint value1 value2 value3)
  ;; paper: f x l
  (assert-that-is-checkpoint
   (il:apply (il:make-continuation 15 (lambda (value count limit) value))
	     value1 value2 0 value3)))

 (define (il:checkpoint-*j value1 value2 value3)
  ;; paper: f x y-grave
  (let loop ((value1 value1)
	     (value2 value2)
	     (value3 value3)
	     ;; 1. n=primops(f,x)
	     (l (primops value1 value2)))
   (if (<= l *base-case-duration*)
       ;; 0. (y,x`)=*j(f,x,y`)
       (il:*j value1 value2 value3)
       ;; 2. u=checkpoint(f,x,n)
       (let* ((u (checkpoint value1 value2 (quotient l 2)))
	      ;; 3. (y,u`)=checkpoint-*j(\u.resume(u),u,y`)
	      (y-u-cotangent
	       (loop *closure-that-resumes* u value3 (- l (quotient l 2))))
	      ;; 4. (u,x`)=checkpoint-*j(\x.checkpoint(f,x,n),x,u`)
	      (u-x-cotangent
	       (loop (make-closure-for-checkpoint value1 (quotient l 2))
		     value2
		     (il:cdr y-u-cotangent)
		     (quotient l 2))))
	(il:cons (il:car y-u-cotangent) (il:cdr u-x-cotangent))))))

 (define (il:binary-checkpoint-*j f x y-grave)
  (define (binary f x y-grave delta tau phi)
   (if (or (<= phi *base-case-duration*) (= delta 0) (= tau 0))
       ;; 0. (y,x`)=*j(f,x,y`)
       (il:*j f x y-grave)
       ;; 2. u=checkpoint(f,x,n)
       (let* ((kappa (*mid* delta tau 0 phi))
	      (u (checkpoint f x kappa))
	      ;; 3. (y,u`)=checkpoint-*j(\u.resume(u),u,y`)
	      (y-u-cotangent
	       (binary *closure-that-resumes*
		       u
		       y-grave
		       (- delta 1)
		       tau
		       (- phi kappa)))
	      ;; 4. (u,x`)=checkpoint-*j(\x.checkpoint(f,x,n),x,u`)
	      (u-x-cotangent
	       (binary (make-closure-for-checkpoint f kappa)
		       x
		       (il:cdr y-u-cotangent)
		       delta
		       (- tau 1)
		       kappa)))
	(il:cons (il:car y-u-cotangent) (il:cdr u-x-cotangent)))))
  ;; 1. n=primops(f,x)
  (binary f x y-grave *d* *t* (primops f x)))

 (define (il:treeverse-checkpoint-*j f x y-grave)
  (define (treeverse f x y-grave delta tau beta sigma phi)
   (if (> sigma beta)
       (let ((u (checkpoint f x (- sigma beta))))
	(first *closure-that-resumes* u y-grave (- delta 1) tau beta sigma phi))
       (first f x y-grave delta tau beta sigma phi)))
  (define (first f x y-grave delta tau beta sigma phi)
   (if (and (> (- phi sigma) *base-case-duration*)
	    (not (= delta 0))
	    (not (= tau 0)))
       (let* ((kappa (*mid* delta tau sigma phi))
	      (y-u-cotangent (treeverse f x y-grave delta tau sigma kappa phi)))
	(rest f x (il:cdr y-u-cotangent) (il:car y-u-cotangent) delta (- tau 1)
	      beta sigma kappa))
       (il:*j (make-closure-for-checkpoint f (- phi sigma)) x y-grave)))
  (define (rest f x y-grave y delta tau beta sigma phi)
   (if (and (> (- phi sigma) *base-case-duration*)
	    (not (= delta 0))
	    (not (= tau 0)))
       (let* ((kappa (*mid* delta tau sigma phi))
	      (y-u-cotangent (treeverse f x y-grave delta tau sigma kappa phi)))
	(rest f x (il:cdr y-u-cotangent) y delta (- tau 1)
	      beta sigma kappa))
       (let ((y-u-cotangent
	      (il:*j (make-closure-for-checkpoint f (- phi sigma)) x y-grave)))
	(il:cons y (il:cdr y-u-cotangent)))))
  (treeverse f x y-grave *d* *t* 0 0 (primops f x)))

 (define (bisection-mid delta tau sigma phi) (ceiling (/ (+ sigma phi) 2)))

 (define (binomial-mid delta tau sigma phi)
  (ceiling (/ (+ (* delta sigma) (* tau phi)) (+ delta tau))))

 (define (choose n r)
  (let loop ((i n) (c 1))
   (if (= i (- n r))
       (let loop ((i r) (c c)) (if (= i 0) c (loop (- i 1) (/ c i))))
       (loop (- i 1) (* i c)))))

 (define (eta d t) (choose (+ d t) t))

 ;; The base-case-duration parameter in the following is a lower bound. This
 ;; is to prevent checkpointing in the preamble. Due to nondivisibility, some
 ;; leaves will have more than base-case-duration steps. The total number of
 ;; extra steps is no more than the number of leaves, (eta d t).

 (define (fixed-d d base-case-duration l)
  (let loop ((t 0))
   (if (> (eta d t) (quotient l base-case-duration)) (- t 1) (loop (+ t 1)))))

 (define (fixed-t t base-case-duration l)
  (let loop ((d 0))
   (if (> (eta d t) (quotient l base-case-duration)) (- d 1) (loop (+ d 1)))))

 (define (fixed-base-case-duration base-case-duration l)
  (let loop ((d-t 0))
   (if (> (eta d-t d-t) (quotient l base-case-duration))
       (- d-t 1)
       (loop (+ d-t 1)))))

 (define first car)

 (define second cadr)

 (define third caddr)

 (define fourth cadddr)

 (define rest cdr)

 (define (every p l . &rest)
  (let loop ((l l) (&rest &rest))
   (or (null? l)
       (and (apply p (first l) (map first &rest))
	    (loop (rest l) (map rest &rest))))))

 (define (last x) (if (null? (rest x)) (first x) (last (rest x))))

 (define (but-last x) (reverse (rest (reverse x))))

 (define (duplicatesq? xs)
  (and (not (null? xs))
       (or (memq (first xs) (rest xs)) (duplicatesq? (rest xs)))))

 (define (find-if p l)
  (let loop ((l l))
   (cond ((null? l) #f)
	 ((p (first l)) (first l))
	 (else (loop (rest l))))))

 (define (unionq x y)
  (let loop ((l x) (c '()))
   (cond ((null? l) (append (reverse c) y))
	 ((memq (first l) y) (loop (rest l) c))
	 (else (loop (rest l) (cons (first l) c))))))

 (define (set-differenceq x y)
  (let loop ((l x) (c '()))
   (cond ((null? l) (reverse c))
	 ((memq (first l) y) (loop (rest l) c))
	 (else (loop (rest l) (cons (first l) c))))))

 (define (map-reduce g i f l . ls)
  (if (null? l)
      i
      (apply map-reduce
	     g
	     (g i (apply f (car l) (map car ls)))
	     f
	     (cdr l)
	     (map cdr ls))))

 (define (map-indexed f l)
  (let loop ((i 0) (l l) (c '()))
   (if (null? l)
       (reverse c)
       (loop (+ i 1) (rest l) (cons (f (first l) i) c)))))

 (define (internal-error . message) (error #f "Internal error" message))

 (define (compile-time-error message . arguments)
  (error #f (apply format message arguments)))

 (define (run-time-error message . arguments)
  (error #f (apply format message arguments)))

 (define (has-extension? pathname)
  (let loop ((l (reverse (string->list pathname))))
   (and (not (null? l))
	(not (char=? (first l) #\/))
	(or (char=? (first l) #\.) (loop (rest l))))))

 (define (default-extension pathname extension)
  (if (has-extension? pathname)
      pathname
      (string-append pathname "." extension)))

 (define *include-path* '())

 (define (search-include-path-without-extension pathname)
  (cond ((file-exists? pathname) pathname)
	((and (>= (string-length pathname) 1)
	      (char=? (string-ref pathname 0) #\/))
	 (compile-time-error "Cannot find: ~a" pathname))
	(else (let loop ((include-path *include-path*))
	       (cond ((null? include-path)
		      (compile-time-error "Cannot find: ~a" pathname))
		     ((file-exists?
		       (string-append (first include-path) "/" pathname))
		      (string-append (first include-path) "/" pathname))
		     (else (loop (rest include-path))))))))

 (define (search-include-path pathname)
  (search-include-path-without-extension (default-extension pathname "vlad")))

 (define (read-source pathname)
  (let ((pathname (default-extension pathname "vlad")))
   (unless (file-exists? pathname)
    (compile-time-error "Cannot find: ~a" pathname))
   (call-with-input-file pathname
    (lambda (input-port)
     (let loop ((es '()))
      (let ((e (read input-port)))
       (cond
	((eof-object? e) (reverse es))
	((and (list? e)
	      (= (length e) 2)
	      (eq? (first e) 'include)
	      (string? (second e)))
	 (loop
	  (append (reverse (read-source (search-include-path (second e)))) es)))
	(else (loop (cons e es))))))))))

 (define (concrete-variable? x)
  ;; removed alpha anf backpropagator tangent forward cotangent reverse
  (and (symbol? x)
       (not (memq x '(quote
		      lambda
		      letrec
		      let
		      let*
		      if
		      cons
		      cons*
		      list
		      cond
		      else
		      and
		      or)))))

 (define (definens? e)
  (or (concrete-variable? e)
      (and (list? e) (not (null? e)) (definens? (first e)))))

 (define (definition? d)
  (and (list? d)
       (>= (length d) 3)
       (eq? (first d) 'define)
       (definens? (second d))
       (or (not (concrete-variable? (second d))) (= (length d) 3))))

 (define (definens-name e)
  (if (concrete-variable? e) e (definens-name (first e))))

 (define (definens-expression e es)
  (if (concrete-variable? e)
      (first es)
      (definens-expression (first e) (list `(lambda ,(rest e) ,@es)))))

 (define (expand-definitions ds e)
  (for-each
   (lambda (d)
    (unless (definition? d) (compile-time-error "Invalid definition: ~s" d)))
   ds)
  (if (null? ds)
      e
      `(letrec ,(map (lambda (d)
		      `(,(definens-name (second d))
			,(definens-expression (second d) (rest (rest d)))))
		     ds)
	,e)))

 (define (value? v)
  (or (null? v)
      (boolean? v)
      (real? v)
      (and (pair? v) (value? (car v)) (value? (cdr v)))))

 (define (il:value v)
  (cond ((null? v) v)
	((boolean? v) v)
	((real? v) v)
	((pair? v) (il:cons (il:value (car v)) (il:value (cdr v))))
	(else (internal-error))))

 (define (syntax-check-parameter! p)
  (cond
   ((boolean? p) (syntax-check-parameter! `',p))
   ((real? p) (syntax-check-parameter! `',p))
   ((concrete-variable? p)
    (unless (concrete-variable? p)
     (compile-time-error "Invalid parameter: ~s" p))
    #f)
   ((and (list? p) (not (null? p)))
    (case (first p)
     ((quote) (unless (and (= (length p) 2) (value? (second p)))
	       (compile-time-error "Invalid parameter: ~s" p))
      #f)
     ((cons)
      (unless (= (length p) 3) (compile-time-error "Invalid parameter: ~s" p))
      (syntax-check-parameter! (second p))
      (syntax-check-parameter! (third p)))
     ((cons*) (syntax-check-parameter! (macro-expand p)))
     ((list) (syntax-check-parameter! (macro-expand p)))
     (else (compile-time-error "Invalid parameter: ~s" p))))
   (else (compile-time-error "Invalid parameter: ~s" p))))

 (define (valid-body? es)
  (and (not (null? es)) (every definition? (but-last es))))

 (define (macro-expand-body es) (expand-definitions (but-last es) (last es)))

 (define (macro-expand e)
  (if (and (list? e) (not (null? e)))
      (case (first e)
       ((quote) e)
       ((lambda)
	(unless (and (>= (length e) 3)
		     (list? (second e))
		     (valid-body? (rest (rest e))))
	 (compile-time-error "Invalid expression: ~s" e))
	(case (length (second e))
	 ((0) `(lambda ((cons* ,@(second e)))
		,(macro-expand-body (rest (rest e)))))
	 ((1) `(lambda ,(second e) ,(macro-expand-body (rest (rest e)))))
	 (else `(lambda ((cons* ,@(second e)))
		 ,(macro-expand-body (rest (rest e)))))))
       ((letrec) e)
       ((let) (cond ((and (>= (length e) 3)
			  (list? (second e))
			  (every (lambda (b) (and (list? b) (= (length b) 2)))
				 (second e)))
		     `((lambda ,(map first (second e)) ,@(rest (rest e)))
		       ,@(map second (second e))))
		    ((and (>= (length e) 4)
			  (concrete-variable? (second e))
			  (list? (third e))
			  (every (lambda (b) (and (list? b) (= (length b) 2)))
				 (third e)))
		     `(letrec ((,(second e)
				(lambda ,(map first (third e))
				 ,@(rest (rest (rest e))))))
		       (,(second e) ,@(map second (third e)))))
		    (else (compile-time-error "Invalid expression: ~s" e))))
       ((let*)
	(unless (and (>= (length e) 3)
		     (list? (second e))
		     (every (lambda (b) (and (list? b) (= (length b) 2)))
			    (second e))
		     (valid-body? (rest (rest e))))
	 (compile-time-error "Invalid expression: ~s" e))
	(case (length (second e))
	 ((0) (macro-expand-body (rest (rest e))))
	 ((1) `(let ,(second e) ,@(rest (rest e))))
	 (else `(let (,(first (second e)))
		 (let* ,(rest (second e)) ,@(rest (rest e)))))))
       ((if) e)
       ((cons) e)
       ((cons*) (case (length (rest e))
		 ((0) ''())
		 ((1) (second e))
		 (else `(cons ,(second e) (cons* ,@(rest (rest e)))))))
       ((list)
	(if (null? (rest e)) ''() `(cons ,(second e) (list ,@(rest (rest e))))))
       ;; We don't allow (cond ... (e) ...) or (cond ... (e1 => e2) ...).
       ((cond)
	(unless (and (>= (length e) 2)
		     (every (lambda (b) (and (list? b) (= (length b) 2)))
			    (rest e))
		     (eq? (first (last e)) 'else))
	 (compile-time-error "Invalid expression: ~s" e))
	(if (null? (rest (rest e)))
	    (second (second e))
	    `(if ,(first (second e))
		 ,(second (second e))
		 (cond ,@(rest (rest e))))))
       ((and) (case (length (rest e))
	       ((0) #t)
	       ((1) (second e))
	       (else `(if ,(second e) (and ,@(rest (rest e))) #f))))
       ((or) (case (length (rest e))
	      ((0) #f)
	      ((1) (second e))
	      (else
	       (let ((x (gensym)))
		`(let ((,x ,(second e))) (if ,x ,x (or ,@(rest (rest e)))))))))
       (else (case (length (rest e))
	      ((0) `(,(first e) (cons* ,@(rest e))))
	      ((1) e)
	      (else `(,(first e) (cons* ,@(rest e)))))))
      e))

 (define (parameter-variables p)
  ;; removed lambda and letrec parameters
  (cond ((il:constant-expression? p) '())
	((il:variable-access-expression? p)
	 (list (il:variable-access-expression-variable p)))
	((il:binary-expression? p)
	 (unionq (il:free-variables (il:binary-expression-expression1 p))
		 (il:free-variables (il:binary-expression-expression2 p))))
	(else (internal-error))))

 (define (syntax-check-expression! e)
  (let loop ((e e) (xs (map il:binding-variable *top-level-environment*)))
   (cond
    ((boolean? e) (loop `',e xs))
    ((real? e) (loop `',e xs))
    ((concrete-variable? e)
     (unless (memq e xs) (compile-time-error "Unbound variable: ~s" e))
     #f)
    ((and (list? e) (not (null? e)))
     (case (first e)
      ((quote)
       (unless (and (= (length e) 2) (value? (second e)))
	(compile-time-error "Invalid expression: ~s" e))
       #f)
      ((lambda)
       (unless (and (>= (length e) 3)
		    (list? (second e))
		    (valid-body? (rest (rest e))))
	(compile-time-error "Invalid expression: ~s" e))
       (case (length (second e))
	((0) (loop (macro-expand e) xs))
	((1)
	 (syntax-check-parameter! (first (second e)))
	 (let ((xs0 (parameter-variables
		     (internalize-expression (first (second e))))))
	  (when (duplicatesq? xs0)
	   (compile-time-error "Duplicate variables: ~s" e))
	  (loop (macro-expand-body (rest (rest e))) (append xs0 xs))))
	(else (loop (macro-expand e) xs))))
      ((letrec)
       (unless (and (>= (length e) 3)
		    (list? (second e))
		    (every
		     (lambda (b)
		      (and (list? b)
			   (= (length b) 2) (concrete-variable? (first b))))
		     (second e)))
	(compile-time-error "Invalid expression: ~s" e))
       (let ((xs0 (map first (second e))))
	(when (duplicatesq? xs0)
	 (compile-time-error "Duplicate variables: ~s" e))
	(for-each
	 (lambda (b)
	  (let ((e1 (macro-expand (second b))))
	   (unless (and (list? e1) (= (length e1) 3) (eq? (first e1) 'lambda))
	    (compile-time-error "Invalid expression: ~s" e))
	   (loop e1 (append xs0 xs))))
	 (second e))
	(loop (macro-expand-body (rest (rest e))) (append xs0 xs))))
      ((let) (loop (macro-expand e) xs))
      ((let*) (loop (macro-expand e) xs))
      ((if)
       (unless (= (length e) 4) (compile-time-error "Invalid expression: ~s" e))
       (loop (second e) xs)
       (loop (third e) xs)
       (loop (fourth e) xs))
      ((cons)
       (unless (= (length e) 3) (compile-time-error "Invalid expression: ~s" e))
       (loop (second e) xs)
       (loop (third e) xs))
      ((cons*) (loop (macro-expand e) xs))
      ((list) (loop (macro-expand e) xs))
      ((cond) (loop (macro-expand e) xs))
      ((and) (loop (macro-expand e) xs))
      ((or) (loop (macro-expand e) xs))
      (else (case (length (rest e))
	     ((0) (loop (macro-expand e) xs))
	     ((1) (loop (first e) xs) (loop (second e) xs))
	     (else (loop (macro-expand e) xs))))))
    (else (compile-time-error "Invalid expression: ~s" e)))))

 (define (il:increment count) (+ count 1))

 (define (il:limit-check count limit) (= count limit))

 (define (il:if-procedure value1 value2 value3) (if value1 value2 value3))

 (define (il:read-real value) (read))

 (define (il:real value) value)

 (define (il:write-real value) (write (primal* value)) (newline) value)

 (define (il:expt y x) (expt (primal* y) (primal* x)))

 (define (il:floor x) (floor (primal* x)))

 (define (il:modulo y x) (modulo (primal* y) (primal* x)))

 (define (il:expression? thing)
  (or (il:constant-expression? thing)
      (il:variable-access-expression? thing)
      (il:lambda-expression? thing)
      (il:application? thing)
      (il:unary-expression? thing)
      (il:binary-expression? thing)
      (il:ternary-expression? thing)
      (il:letrec-expression? thing)
      (il:checkpoint-expression? thing)
      (il:resume-expression? thing)))

 (define (il:make-if expression1 expression2 expression3)
  (unless (and (il:expression? expression1)
	       (il:expression? expression2)
	       (il:expression? expression3))
   (internal-error))
  (make-il:application
   (make-il:ternary-expression
    il:if-procedure
    expression1
    (make-il:lambda-expression (make-il:constant-expression '()) expression2)
    (make-il:lambda-expression (make-il:constant-expression '()) expression3))
   (make-il:constant-expression '())))

 (define (il:make-cons e1 e2) (make-il:binary-expression il:cons e1 e2))

 (define (il:make-let p e1 e2)
  (make-il:application (make-il:lambda-expression p e2) e1))

 (define (internalize-expression e)
  (cond
   ((boolean? e) (internalize-expression `',e))
   ((real? e) (internalize-expression `',e))
   ((concrete-variable? e) (make-il:variable-access-expression e))
   ((and (list? e) (not (null? e)))
    (case (first e)
     ((quote) (make-il:constant-expression (il:value (second e))))
     ((lambda)
      (case (length (second e))
       ((0) (internalize-expression (macro-expand e)))
       ((1) (make-il:lambda-expression
	     (internalize-expression (first (second e)))
	     (internalize-expression (macro-expand-body (rest (rest e))))))
       (else (internalize-expression (macro-expand e)))))
     ((letrec)
      (make-il:letrec-expression
       (map first (second e))
       (map (lambda (b) (internalize-expression (macro-expand (second b))))
	    (second e))
       (internalize-expression (macro-expand-body (rest (rest e))))))
     ((let) (internalize-expression (macro-expand e)))
     ((let*) (internalize-expression (macro-expand e)))
     ((if) (il:make-if (internalize-expression (macro-expand (second e)))
		       (internalize-expression (macro-expand (third e)))
		       (internalize-expression (macro-expand (fourth e)))))
     ((cons) (il:make-cons (internalize-expression (macro-expand (second e)))
			   (internalize-expression (macro-expand (third e)))))
     ((cons*) (internalize-expression (macro-expand e)))
     ((list) (internalize-expression (macro-expand e)))
     ((cond) (internalize-expression (macro-expand e)))
     ((and) (internalize-expression (macro-expand e)))
     ((or) (internalize-expression (macro-expand e)))
     (else (case (length (rest e))
	    ((0) (internalize-expression (macro-expand e)))
	    ((1) (make-il:application
		  (internalize-expression (macro-expand (first e)))
		  (internalize-expression (macro-expand (second e)))))
	    (else (internalize-expression (macro-expand e)))))))
   (else (internal-error))))

 (define (concrete->abstract e)
  (let ((e (internalize-expression e)))
   (list e
	 (map (lambda (x)
	       (find-if (lambda (b) (eq? x (il:binding-variable b)))
			*top-level-environment*))
	      (il:free-variables e)))))

 (define (make-unary-primitive variable f)
  ;;\needswork: type check
  (make-il:binding
   variable
   (il:make-nonrecursive-closure
    (make-il:lambda-expression
     (make-il:variable-access-expression 'x)
     (make-il:unary-expression f (make-il:variable-access-expression 'x)))
    '())))

 (define (make-binary-primitive variable f)
  ;;\needswork: type check
  (make-il:binding
   variable
   (il:make-nonrecursive-closure
    (make-il:lambda-expression
     (il:make-cons (make-il:variable-access-expression 'x1)
		   (make-il:variable-access-expression 'x2))
     (make-il:binary-expression f
				(make-il:variable-access-expression 'x1)
				(make-il:variable-access-expression 'x2)))
    '())))

 (define (make-ternary-primitive variable f)
  ;;\needswork: type check
  (make-il:binding
   variable
   (il:make-nonrecursive-closure
    (make-il:lambda-expression
     (il:make-cons (make-il:variable-access-expression 'x1)
		   (il:make-cons (make-il:variable-access-expression 'x2)
				 (make-il:variable-access-expression 'x3)))
     (make-il:ternary-expression f
				 (make-il:variable-access-expression 'x1)
				 (make-il:variable-access-expression 'x2)
				 (make-il:variable-access-expression 'x3)))
    '())))

 (define *top-level-environment*
  (list (make-binary-primitive '+ d+)
	(make-binary-primitive '- d-)
	(make-binary-primitive '* d*)
	(make-binary-primitive '/ d/)
	(make-unary-primitive 'sqrt dsqrt)
	(make-unary-primitive 'exp dexp)
	(make-unary-primitive 'log dlog)
	(make-unary-primitive 'sin dsin)
	(make-unary-primitive 'cos dcos)
	(make-binary-primitive 'atan datan)
	(make-binary-primitive '= d=)
	(make-binary-primitive '< d<)
	(make-binary-primitive '> d>)
	(make-binary-primitive '<= d<=)
	(make-binary-primitive '>= d>=)
	(make-unary-primitive 'zero? dzero?)
	(make-unary-primitive 'positive? dpositive?)
	(make-unary-primitive 'negative? dnegative?)
	(make-unary-primitive 'null? null?)
	(make-unary-primitive 'boolean? boolean?)
	(make-unary-primitive 'real? dreal?)
	(make-unary-primitive 'pair? il:pair?)
	(make-unary-primitive 'procedure? il:closure?)
	(make-unary-primitive 'read-real il:read-real)
	(make-unary-primitive 'real il:real)
	(make-unary-primitive 'write-real il:write-real)
	;; The next three are not in VLAD and added for ad2016. You can't take
	;; derivatives of these.
	(make-binary-primitive 'expt il:expt)
	(make-unary-primitive 'floor il:floor)
	(make-binary-primitive 'modulo il:modulo)
	(make-ternary-primitive 'j* il:j*)
	(make-ternary-primitive '*j il:*j)
	(make-ternary-primitive 'checkpoint-*j il:checkpoint-*j)))

 (define (vlad base-case-duration pathname)
  (set! *mid* binomial-mid)
  (set! *base-case-duration* base-case-duration)
  (let loop ((es (read-source pathname)) (ds '()))
   (unless (null? es)
    (if (definition? (first es))
	(loop (rest es) (cons (first es) ds))
	(let ((e (expand-definitions (reverse ds) (first es))))
	 (syntax-check-expression! e)
	 (let* ((result (concrete->abstract e))
		(e (first result))
		(bs (second result)))
	  (il:eval (il:make-continuation
		    16
		    (lambda (value count limit)
		     (write (il:externalize value))
		     (newline)
		     (loop (rest es) ds)))
		   e
		   bs
		   0
		   infinity)))))
   (exit)))

 (define (vlad-I include-directory base-case-duration pathname)
  (set! *include-path* (list include-directory))
  (set! *mid* binomial-mid)
  (set! *base-case-duration* base-case-duration)
  (let loop ((es (read-source pathname)) (ds '()))
   (unless (null? es)
    (if (definition? (first es))
	(loop (rest es) (cons (first es) ds))
	(let ((e (expand-definitions (reverse ds) (first es))))
	 (syntax-check-expression! e)
	 (let* ((result (concrete->abstract e))
		(e (first result))
		(bs (second result)))
	  (il:eval (il:make-continuation
		    17
		    (lambda (value count limit)
		     (write (il:externalize value))
		     (newline)
		     (loop (rest es) ds)))
		   e
		   bs
		   0
		   infinity)))))
   (exit))))
