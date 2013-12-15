(use extras)

(define read-line-with-message
  (lambda [message]
    (begin (print message) (read-line))))

(define reduce
  (lambda [f xs]
    (cond
      [(null? xs)        xs]
      [(null? (cdr xs))  (car xs)]
      [#t                (f (car xs) (reduce f (cdr xs)))])))

(define range-from
  (lambda [i n]
    (cond
      [(> i n)      (quote ())]
      [#t           (cons i (range-from (+ 1 i) n))])))

(define partial
  (lambda [f . oargs]
    (lambda iargs
      (apply f (append oargs iargs)))))

(define range (partial range-from 1))

(cond
  [(equal? (read-line-with-message "Do you want factorial?") "yes") (print (reduce * (range (string->number (read-line-with-message "fact: ")))))]
  [#t (print (reduce + (range (string->number (read-line-with-message "sum: ")))))])
