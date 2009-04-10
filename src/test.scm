
(define a 20)
a

(define b 10)
b

(+ a b)

;; lambda abstraction
((lambda (x y) (+ x y)) a b)

;; Even Function
(define even (lambda (a) (if (= (modulo a 2) 0) #t #f))) 

;; factorial
(define fact (lambda (n) (if (< n 2)
			     1
			     (* n (fact (- n 1))))))

;; fibonacci
(define fib (lambda (n) (if (< n 2)
			    n
			    (+ (fib (- n 1))
			       (fib (- n 2))))))

;; Ackerman Function
(define ack (lambda (m n) 
	      (if (= m 0)
		  (+ n 1)
		  (if (> m 0)
		      (if (= n 0)
			  (ack (- m 1) 1)
			  (ack (- m 1)
			       (ack m (- n 1))))))))
		






(define ack1 (lambda (m n)
	       (if (= m 0)
		   (+ n 1)
		   (if (= m 1)
		       (+ n 2)
		       (if (= m 2)
			   (+ 3 (* n 2))
			   (if (= m 3)
			       (+ 5 (* 8 (- (expt 2 n) 1)))
			       (if (= n 0)
				   (ack1 (- m 1) 1)
				   (if (> m 3)
				       (ack1 (- m 1) (ack1 m (- n 1)))))))))))
