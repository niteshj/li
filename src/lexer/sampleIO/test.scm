(define (add-if-all-numbers lst)
  (call/cc
    (lambda (exit)
      (let loop ((lst lst) (sum 0))
        (if (null? lst) sum
          (if (not (number? (car lst))) (exit #f)
            (loop (cdr lst) (+ sum (car lst)))))))))
