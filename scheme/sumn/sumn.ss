(use extras)

(define reduce
  (lambda (f xs)
    (cond 
      ((null? (cdr xs)) (car xs))
      ((f (car xs) (reduce f (cdr xs)))))))

(define range
  (lambda (n)
    (cond
      ((zero? n) (quote ()))
      (#t (cons n (range (- n 1)))))))

(print (reduce + (range (string->number (read-line)))))
