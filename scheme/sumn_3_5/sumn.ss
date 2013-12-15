(use extras)

(define filter
  (lambda [f xs]
    (cond
      [(null? xs)  xs]
      [(f (car xs)) (cons (car xs) (filter f (cdr xs)))]
      [#t (filter f (cdr xs))])))

(define reduce
  (lambda [f xs]
    (cond 
      [(null? xs) xs]
      [(null? (cdr xs)) (car xs)]
      [(f (car xs) (reduce f (cdr xs)))]
    )
  )
)

(define partial
  (lambda [f . iargs]
    (lambda oargs
      (apply f (append iargs oargs)))
  )
)

(define rangefrom
  (lambda [i n]
    (cond
      [(> i n)  (quote ())]
      [#t       (cons i (rangefrom (+ i 1) n))]
    )
  )
)

(define range (partial rangefrom 1))

(print (reduce + (filter (lambda (x) (or (= (remainder x 3) 0) (= (remainder x 5) 0))) (range (string->number (read-line))))))
