(use extras)
(use srfi-69)

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

(define reverse-f
  (lambda [f . args]
    (partial f (reverse args))))

(define cartesian-product-single
  (lambda [x xs]
    (cond
      [(null? xs) xs]
      [#t (cons (list x (car xs)) (cartesian-product-single x (cdr xs)))])))

(define cartesian-product
  (lambda [xs ys]
    (map 
      (lambda [x] (cartesian-product-single x ys))
      xs)))

(define divisible?
  (lambda [x y]
    (= (remainder x y) 0)))

(define memoize
  (lambda [f]
    (let ((values (make-hash-table)))
      (lambda args
        (if (hash-table-exists? values args) 
          (hash-table-ref values args)
          (let ((result (apply f args)))
            (hash-table-set! values args result)
            result))))))

(define prime-test
  (lambda [n x]
    (cond
      [(= x 2)           #t]
      [(> n (sqrt x))    #t]
      [(divisible? x n)  #f]
      [#t                (prime-test (+ n 2) x)])))

(define prime? (memoize (partial prime-test 2)))

(define forever-prime
  (lambda [x]
    (if
      (prime? x)
      (begin
        (print x)
        (forever-prime (+ x 1)))
      (forever-prime (+ x 1)))))

(forever-prime 1)
