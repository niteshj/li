
;; Even Function
(define even (lambda (a) (if (= (modulo a 2) 0) #t #f))) 

;; Ackerman Function
(define ack (lambda (m n) 
	      (if (= m 0)
		  (+ n 1)
		  (if (> m 0)
		      (if (= n 0)
			  (ack (- m 1) 1)
			  (ack (- m 1)
			       (ack m (- n 1))))))))
		
