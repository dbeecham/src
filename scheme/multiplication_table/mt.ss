(use extras)

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
    (map (lambda [x] (cartesian-product-single x ys)) xs)))
