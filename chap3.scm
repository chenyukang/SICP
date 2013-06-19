
;;Exercise 3.28
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
	   (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
		   (lambda ()
		     (set-signal! output new-value))))
    (add-action! a1 or-action-procedure)
    (add-action! a2 or-action-procedure)
    'ok))

;;Exercise 3.29
;; A or B = (not (and (not A) (not B)))
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
	   (logical-not (logical-and
			 (logical-not (get-signal a1))
			 (logical-not (get-signal a2))))))
      (after-delay or-gate-delay
		   (lambda ()
		     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)
;;Exercise 3.30
;;half-adder delay :
;;half-adder-delay= Max{or-delay+inverter-dely,and-delay}+or-delay
;;full-adder-delay= 2*half-adder-delay+or-delay
;;= 3*or-delay+2*Max{or-delay+inverter-delay,and-delay}
;;n bit ripple-carry adder :
;;ripple-carry adder-delay = N*full-adder-delay


;;Exercise 3.31
;;If the new prcedure changes a wire's value ,it stimulate other related procedure to run.

;;Exercise 3.32
;;离散事件模拟，前一个事件的结果可能导致新的事件的发生，用队列存储事件顺序才能正确模拟。


;;Exercise 3.33
; Using primitive multiplier, adder, and constant constraints

(define a (make-connector))
(define b (make-connector))
(define c (make-connector))

(define (average-converter a b c)
  (let ((sum (make-connector))
	(factor (make-connector)))
    (adder a b sum)
    (multiplier factor sum c)
    (constant 1/2 factor)
    'ok))

;(average-converter a b c)
;(probe "Input-a temp" a)
;(probe "Input-b temp" b)
;(probe "Average-c temp" c)

;;Exercise 3.34
;;This version procedure can compute sqaure of a if given the value of a,but can't compute the value of a if given the value of it's value of sqaure, so it it's not a compute network.


;;Exercise 3.35
(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0 -- SQUARER" (get-value b))
            (set-value! a (sqrt (get-value b)) me)) 
        (if (has-value? a)
            (set-value! b (* (get-value a) (get-value a)) me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- SQUARER" request))))
  (connect a me)
  (connect b me)
  me)

;;Test
;(squarer a b)
;(probe "Input-a temp" a)
;(probe "Output-b temp" b)

;;Exercise 3.36
;skip

;;Exercise 3.37
;;similar,we can define subber and diver  as primitive constraint,

(define (subber a1 a2 sub)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (- (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sub))
           (set-value! a2
                       (- (get-value a1) (get-value sub))
                       me))
          ((and (has-value? a2) (has-value? sub))
           (set-value! a1
                       (+ (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sub me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)  
           (process-new-value))
          ((eq? request 'I-lost-my-value) 
           (process-forget-value))
          (else 
           (error "Unknown request -- SUBBER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sub me)
  me)

;so these definition will give an expression-oriented style.
(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))
(define (cv value)
  (let ((z (make-connector)))
    (constant value z)
    z))

(define (c- x y)
  (let ((z (make-connector)))
    (subber x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (diver x y z)
    z))

;;============3.4  Concurrency: Time Is of the Essence================;;

;;Exercise 3.38
;a.  There are four  possible value. 45,35,40,50

;b.  skip

;;Exercise 3.39
;100: 	P1 accesses x (twice), then P2 sets x to 11, then P1 sets x.
;110: 	P2 changes x from 10 to 11 between the two times that P1 accesses the value of x during the evaluation of (* x x).

;;Exercise 3.40
;10^4,10^6,10^5
;10^6


;;Exercise 3.41
;Yes,I agree,Supposse there is a joint accout which have 100$.If Peter is withdrawing 90$ from a account,but haven't finished,Paul think there enough money,so withdraw 20 from account,but after Peter finished there money,the account haven't enough money for Paul,but the program have go to the line: (set! (balance (- balance amount))) which is not correct.

;;Exercise 3.42
;not safe


;;Exercise 3.43



;;====================3.5 Streams ===========================;;
;;Exercise 3.50
;;the list version is:
(define (my-map proc . args)
  (if(null? (car args))
     '()
     (cons 
      (apply proc (map car args))
      (apply my-map 
             (cons proc (map cdr args))))))
;so, the stream version is :
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (stream-cons
       (apply proc (map stream-car  argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

;;Exercise 3.51
;(define x (stream-map show (stream-enumerate-interval 0 10)))
;print 0
;      1
;      2
;      3
;     ....

;(stream-ref x 5) ==>5
;(stream-ref x 7) ==>7

;;Exercise 3.52

;sum=210

;(stream-ref y 7)  ==>136
;(display-stream z) ==>
;; 10
;; 15
;; 45
;; 55
;; 105
;; 120
;; 190
;; 210done

;There is no difference if we change delay's implemention. memo-proc can just save
;some uncessary computation.

;Part of the power of stream processing is that it lets us ignore the order in which events actually happen in our programs. Unfortunately, this is precisely what we cannot afford to do in the presence of assignment, which forces us to be concerned with time and change.







