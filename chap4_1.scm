;;============================4.1  The Metacircular Evaluator================;;
;;Exercise 4.1
;;the left-to-right version
(define (list-of-values exps env)
  (if(no-operands? exps)
     '()
     (let ((temp (eval (first-operand exps) env)))
       (cons temp (list-of-values (rest-operands exps) env)))))

;;the right-to-left version
(define (list-of-values exps env)
  (if(no-operands? exps)
     '()
     (let ((rest (list-of-values (rest-operands exps) env)))
       (cons (eval (first-operand exps) env)
	     rest))))


;;Exercise 4.2
;;a:  assignment is a special form, if the clause for procedure applications appears before the clause for assignments, the expression (define x 3) will treated as an application procedure, "define" is operator "x" and "3" will be operands.

;;b: so we have to rewrite the application? and so on
; exp (call operator operands ...)
(define (application? exp)
  (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

;the we can chang the eval procedure.
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
	((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp) 
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

;;Exercise 4.3
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
	((quoted? exp) (text-of-quotation exp))
	(else
	 (if(get 'eval (car exp))
	    ((get 'eval (car exp)) exp env)
	    (error "Unknown expression type -- EVAL" exp)))))

;;Exercise 4.4
(define (or? exp)
  (tagged-list? exp 'or))
(define (and? exp)
  (tagged-list? exp 'and))
(define (logical-operands exp)
  (cdr exp))


;;(and) ==> #t
;;(and) ==> #t
(define (eval-and exp env)
  (cond
   ((no-operands? (operands exp)) #t)
    ((not (true? (eval (first-operand (operands exp)) env))) #f)
    (else
     (eval-and (cons (car exp) (rest-operands (operands exp))) env)))) 
;;(or) ==> #f
(define (eval-or exp env)
  (cond ((no-operands? (operands exp)) #f)
	 ((true? (eval (first-operand (operands exp)) env)) #t)
	 (else
	  (eval-or (cons (car exp) (rest-operands (operands exp))) env))))

;;derived expression
(define (and->if exp)
  (expand-and (logical-operands exp)))

(define (expand-and operands)
  (if (null? operands)
      (quote true) ;;(and) ==> #t
      (let ((first (car operands))
	    (rest (cdr operands)))
	(make-if first
		 (expand-and rest)
		 (quote false)))))

(define (or->if exp)
  (expand-or (logical-operands exp)))

(define (expand-or operands)
  (if (null? operands)
      (quote false)  ;;(or) ==> #f
      (let ((first (car operands))
	    (rest (cdr operands)))
	(make-if first
		 (quote true)
		 (expand-or rest)))))

;;Exercise 4.5
(define (test)
  (cond ((assoc 'a '((a 1) (b 2))) => cadr)
	(else false)))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))

(define (cond-recipient-action? actions)
  (eq? (car actions) '=>))

;It is not convinient to use derived expressions.
;so I write a eval-cond

;;Exercise 4.6

(define (test x)
  (if(> x 0)
     (let ((temp 3)
           (y 5))
       (+ x y temp))
     (- x 1)))

(define (test x)
  (cond ((> x 0) (+ x 1))
        ((= x 0) 0)
        (else 
         (- x 1))))

;;Exercise 4.11
;;the enviroment is a list of frames,so there is no change
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

;;each frame is a list of pair (variable value),so it is:
(define (make-var-val var val)
  (cons var val))
(define (var-part var-val)
  (car var-val))
(define (val-part var-val)
  (cdr var-val))

(define (make-frame variables values)
  (if(= (length variables) (length values))
     (map cons variables values)
     (if(< (length variables) (length values))
	(error "Too many arguments supplied" variables values)
	(error "Too few arguments supplied" variables values))))


;; (define (add-binding-to-frame! var val frame)
;;   (define (loop a-frame)
;;     (cond ((null? (cdr a-frame))
;; 	   (set-cdr! a-frame (list (make-var-val var val))));;This is important
;; 	  (else
;; 	   (loop (cdr a-frame)))))
;;   (loop frame))

;;new variale binded to the last position,though it is not for lookup-variable-value,
;;because lookup find from the beginning of a frame...

(define (add-binding-to-frame! var val frame)
  (let ((var-val (make-var-val var val)))
    (set-cdr! frame (cons var-val (cdr frame)))))
;;this version will add the new variable to 2th position.


(define (extend-environment vars vals base-env)
  (cons (make-frame vars vals) base-env))

(define (lookup-variable-value var env)
  (define (loop environment)
    (define (scan frame)
      (cond((null? frame) (loop (enclosing-environment environment)))
	   ((eq? (var-part (car frame)) var)
	    (val-part (car frame)))
	   (else (scan (cdr frame)))))
    (if(eq? environment the-empty-environment)
	   (error "Unbound variable" var)
	   (scan (car environment))))
  (loop env))


(define (set-variable-value! var val env)
  (define (loop environment)
    (define (scan frame)
      (cond ((null? frame) (loop (enclosing-environment environment)))
	    ((eq? (var-part (car frame)) var)
	     (set-cdr! (car frame) val))
	    (else (scan (cdr frame)))))
    (if (eq? environment the-empty-environment)
	(error "Unbound variable" var)
	(scan (car environment))))
  (loop env))

(define (define-variable! var val env)
  (let ((the-frame (first-frame env)))
    (define (scan frame)
      (cond ((null? frame)
	     (add-binding-to-frame! var val the-frame))
	    ((eq? (var-part (car frame)) var)
	     (set-cdr! (car frame) val))
	    (else (scan (cdr frame)))))
    (scan the-frame)))

;;Exercise 4.12
(define (abstruct-procedure env)
  (lambda (p)
    (define (env-loop env)
      (define (scan vars vals)
	(cond ((null? vars)
	       (env-loop (enclosing-environment env)))
	      ((eq? var (car vars))
	       (p vars vals))
	       (else
		(scan (cdr vars) (cdr vals)))))
      (if (eq? env eht-empty-environment)
	  (error "Unbound variable" var)
	  (let ((frame (frist-frame env)))
	    (scan (frame-variables frame)
		  (frame-values frame)))))
    (env-loop env)))

(define (lookup-variable-value var env)
  ((abstruct-procedure env)
   (lambda(vars vals)
     (car vals))))

(define (set-variable-value! var val env)
  ((abstruct-procedure env)
   (lambda (vars vals)
     (set-car! vals val))))

;;Exercise 4.13

;;Exercise 4.14

;;Exercise 4.15
;This is Turing's celebrated Halting Theorem
;

(define (try p)
  (if (halts? p p)
      (run-forever)
      'halted))
;(try try)
;if procedure try is halted. Then (try try) run-forever,if try is not to halt,then (try try) is halted,so all the possible outcome violate the intended behavior of halt?


;;=============4.2  Variations on a Scheme -- Lazy Evaluation============;;
;;Exercise 4.25
(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

(define (factorial n)
  (unless (= n 1)
          (* n (factorial (- n 1)))
          1))

;(factorial 4)
;In appilcative-order Scheme this will get into an infinite loop ,because unless as a procedure will always eval each argument
;In normal-order language this will work.

;;Exercise 4.26

;;Exercise 4.27

;;count ==> 2
;;w ==> 10
;;count ==>2

;;Exercise 4.28

;;Exercise 4.29
;; memoizing version: (square (id 10)) ==> 100   count ==> 1
;; non-memoizing version: (square (id 10)) ==> 100 count ==>2
;;Because in memoizing version ((square x) (* x x)) eval the first x,the second evaluation will just use the value-of-memorize-body, so through the process call id function just one time;
;;in non-memoizing version,square will call id function two time,so count will be 2.

(define (test n)
  (cond ((= n 1) 1)
	((= n 0) 0)
	(else
	 (+ (test (- n 1)) (test (- n 2))))))

;for the function above,memoizing version will save a lot of compute time.

;;Exercise 4.30
;a) :因为该序列中的各元素之间没有关联，用原来的eval-sequence将产生正确的结果。
;b) :(p1 1) ==> (1 2) (p2 1) ==> 1
;after change eval-sequence: (p1 1) ==> (1 2) (p2 1) ==> (1 2)
;在惰性求值序列的时候，因为序列中有元素没有求实际值，所以当以一个序列中的元素作为后续过程的参数时，将产生不同结果，例子中：
;（p (set! x (cons x '(2)))) ,e为（set! x (cons x '(2))) 但在p中e作为delay-obj推迟了求值，返回x，即为1
;若修改后，p 中的e会先求值，（set！ x  (cons x '(2)))将执行，所以x为(1 2)

;c) :修改后的eval-sequence用在元素之间没有关联的序列当然也能产生正确的结果，eval-sequence除了最后一项，其他的都是是在eval的时候产生真实值。

;;Exercise 4.35
(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ 1 low) high)))

;;Exercise 4.36
(define (one-pythagorean-triple)
  (let ((i (an-integer-starting-from 1)))
    (let ((j (an-integer-starting-from 1)))
      (let ((k (an-integer-between 1 (+ i j))))
	(require (= (+ (* i i) (* j j)) (* k k)))
	(list i j k)))))


;;Exercise 4.38
;Omit the requirement that Smith and Fletcher do not live on adjacent floors. 5 solutions

;;Exercise 4.39
