(use extras)

(define sum
  (lambda [i n f]
    (cond
      [(= i n)   0]
      [#t        (+ (f i) (sum (+ i 1) n f))])))

(define pi
  (lambda [x]
    (/ (expt -1 (+ x 1))
       (- (* 2 x) 1))))

(print (* 4 (sum 1 1e6 pi)))
