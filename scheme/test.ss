(define *
    (lambda (m n)
        (cond
            ((eq? n 1) m)
            (else (+ m (* m (- n 1)))))))

(define atom?
    (lambda (x)
        (and (not (pair? x)) (not (null? x)))))

(define member?
    (lambda (a lat)
        (cond
            ((null? lat) #f)
            (else (or (eq? a (car lat))
                (member? a (cdr lat)))))))

(define tup+
    (lambda (tup1 tup2)
        (cond
            ((and (null? tup1) (null? tup2)) (quote ()))
            (else (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))

(define no-nums
    (lambda (lat)
        (cond
            ((null? lat) (quote ()))
            ((number? (car lat)) (no-nums (cdr lat)))
            (else (cons (car lat) (no-nums (cdr lat)))))))

(define pick
    (lambda (n lat)
        (cond
            ((null? lat) (quote ()))
            ((eq? n 1) (car lat))
            (else (pick (- n 1) (cdr lat))))))

(define <
    (lambda (x y)
        (cond
            ((zero? y) #f)
            ((zero? x) #t)
            (else (< (- x 1) (- y 1))))))

(define length
    (lambda (lat)
        (cond
            ((null? lat) 0)
            (else (+ 1 (length (cdr lat)))))))

(define >
    (lambda (x y)
        (cond
            ((zero? x) #f)
            ((zero? y) #t)
            (else (> (- x 1) (- y 1))))))

(define addtup
    (lambda (tup)
        (cond
            ((null? tup) 0)
            (else (+ (car tup) (addtup (cdr tup)))))))


(define rember
    (lambda (a lat)
        (cond
            ((null? lat) (quote()))
            ((eq? a (car lat)) (cdr lat))
            (else (cons (car lat) (rember a (cdr lat)))))))

(define multirember
    (lambda (a lat)
        (cond
            ((null? lat) (quote ()))
            ((eq? a (car lat)) (multirember a (cdr lat)))
            (else (cons (car lat) (multirember a (cdr lat)))))))

(define firsts
    (lambda (lat)
        (cond
            ((null? lat) (quote()))
            (else (cons (car (car lat)) (firsts (cdr lat)))))))

(define insertR
    (lambda (new old lat)
        (cond
            ((null? lat) (quote ()))
            ((eq? old (car lat)) (cons (car lat) (cons new (cdr lat))))
            (else (cons (car lat) (insertR new old (cdr lat)))))))

(define multiinsertR
    (lambda (new old lat)
        (cond
            ((null? lat) (quote ()))
            ((eq? old (car lat)) (cons (car lat) (cons new (multiinsertR new old (cdr lat)))))
            (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(define substr
    (lambda (new old lat)
        (cond
            ((null? lat) (quote ()))
            ((eq? (car lat) old) (cons new (cdr lat)))
            (else (cons (car lat) (substr new old (cdr lat)))))))

(define substr2
    (lambda (new old1 old2 lat)
        (cond
            ((null? lat) (quote ()))
            ((eq? old1 (car lat)) (cons new (cdr lat)))
            ((eq? old2 (car lat)) (cons new (cdr lat)))
            (else (cons (car lat) (substr2 new old1 old2 (cdr lat)))))))

(define multisubstr
    (lambda (new old lat)
        (cond
            ((null? lat) (quote ()))
            ((eq? old (car lat)) (cons new (multisubstr new old (cdr lat))))
            (else (cons (car lat) (multisubstr new old (cdr lat)))))))
