; cpr = Contents of the Prefix part of Register number
; ctr = Contents of the Tag part of Register number


(atom? (quote atom)) ; #t
(atom? 'atom) ; #t
(atom? '1492) ; #t
(atom? '*abc$) ; #t
(atom? '(3)) ; #f
(atom? 3) ; #t
(atom? '()) ; #f


; Contents of the Address part of Register number
(car '(1 2 3)) ; 1
(car '((1) (2) (3))) ; (1)
(car 3) ; no answer
(car '()) ; no answer
(null? '()) ; #t

; Law of Car
; The primitive car is defined
; only for non-empty lists.

; Contents of the Decrement part of Register number
(cdr '(1 2 3)) ; (2 3)
(cdr '((1 2 3) 4 5 6) ; (4 5 6)
(cdr '(1)) ; ()
(cdr '()) ; no answer
(null? '()) ; #t

; Law of Cdr
; The primitive cdr is defined only for
; non-empty lists. The cdr of any non-empty
; list is always another list.

(cons 'peanut '(butter and jelly)) ; (peanut butter and jelly)
(cons '(peanut) '(butter and jelly)) ; ((peanut) butter and jelly)
(cons '(peanut butter and jelly) '()) ;((peanut butter and jelly))
(cons 'a '()) ; (a)
(cons '(a) 'b) ; no answer; second argument must be a list

; The Law of Cons
; The primitive cons takes two arguments.
; The second argument to cons must be a list. The result
; is a list.


(null? '()) ; #t
(null? '(a)) ; #f
(null? 'atom) ; No answer

; The Law of null?
; The primitive null? is defined only for lists.

(eq? 'alice 'bob) ; #f
(eq? 'alice 'alice) ; #t
(eq? 3 3) ; No answer. Numeric atom.
(eq? '() '()) ; No answer. Lists.

; The Law of eq?
; The primitive eq? takes two arguments.
; Each must be a non-numeric atom.

(lat? '(Jack Sprat could eat no chicken fat)) ; #t, each S-expression is an atom
(lat? '((Jack) Sprat could eat no chicken fat)) ; #f, (car) is a list.
(lat? '(Jack (Sprat could) eat no chicken fat)) ; #f, one of the s-exp is a list.
(lat? '()) ; #t, does not contain a list

; lat = List of AToms


; Example definition of lat?
(define lat?                                ; (define <name> <value>)
  (lambda (l)                               ; (lambda <arguments> <value>)
    (cond                                   ; (cond (<if> <then>) ...)
      ((null? l) #t)                        ; if (null? l) then #t
      ((atom? (car l)) (lat? (cdr l)))      ; if (atom? (car l)) then try next s-expr
      (#t #f))))                            ; if true then false (else)

(or? (null? '()) (atom? '(a b c))) ; #t, (null? '()) is #t
(or? (null? '(a)) (null? '())) ; #t, (null? '()) is #t
(or? (null? '(atom)) (null? '(atom))) ; #f, neither is true

(define member?
  (lambda (needle haystack)
    (cond
      ((null? haystack) #f)
      ((eq? needle (car haystack)) #t)
      (#t (member? needle (cdr haystack))))))

; or

(define member?
  (lambda (needle haystack)
    (cond
      ((null? haystack) #f)
      (else
        (or
          (eq? needle (car haystack))
          (member? needle (cdr haystack)))))))

; ^ ugly as shit


; The First Commandment
; When recurring on a list of atoms, lat, ask two questions about it:
; (null? lat) and else
; When recurring on on a number, n, ask two questions about it:
; (zero? n) and else


(rember needle haystack) ; remove member needle from haystack
(rember 'mint '(lamb chops and mint jelly)) ; (lamb chops and jelly)
(rember 'toast '(bacon lettuce and tomato)) ; (bacon lettuce and tomato)
(rember 'cup '(coffee cup tea cup and hick cup)) ; (coffee tea cup and hick cup)

; rember removes the first appearance of needle from haystack
; what to do?
; first commandment: ask null?
; is (eq? needle (car haystack))? then return (cdr haystack)!
; else cons needle onto (rember (cdr haystack))

(define rember
  (lambda (needle haystack)
    (cond
      ((null? haystack) haystack)
      ((eq? needle (car haystack)) (cdr haystack))
      (#t (cons needle (rember needle (cdr haystack)))))))


; The Second Commandment
; Use cons to build lists.

; The Third Commandment
; When building a list, describe the first typical element,
; and then cons it onto the natural recursion


(insert.right value at l) ; inserts 'value' right of position 'at' in list 'l'
(insert.right 'topping 'fudge '(ice cream with fudge for dessert))
    ; (ice cream with fudge topping for dessert)

(define insert.right
  (lambda (value at l)
    (cond
      ((null? l) l)
      ((eq? at (car l)) (cons (car l) (cons value (cdr l))))
      (#t (cons (car l) (insert value at (cdr l)))))))

(insert.left value at lat) ; insert 'value' left of position of 'at' in list 'lat'
(insert.left 'topping 'fudge '(ice cream with fudge for dessert))
    ; (ice cream with topping fudge for dessert)

(define insert.left


(subst value replacement lat) ; substitutes value for replacement in lat
(subst 'topping 'fudge '(ice cream with fudge for dessert))
    ; (ice cream with topping for dessert

(define subst
  (lambda (value replacement lat)
    (cond
      ((null? lat) lat)
      ((eq? (car lat) value) (cons replacement (cdr lat)))
      (#t (cons (car lat) (subst value replacement (cdr lat)))))))


(multirember needle haystack) ; remove every occurance of needle in haystack

(define multirember
  (lambda (needle haystack)
    (cond
      ((null? haystack) haystack)
      ((eq? needle (car haystack)) (multirember needle (cdr haystack)))
      (#t (cons (car haystack) (multirember needle (cdr haystack)))))))


(multiinsert.right value at lat) ; inserts value right of every occurance of 'at' in list 'lat
(multiinsert.right 'v 't '(a b t c d t)) ; (a b t v c d t v)

(define multiinsert.right
  (lambda (value at lat)
    (cond
      ((null? lat) lat)
      ((eq? at (car lat)) (cons at (cons value (multiinsert.right value at (cdr lat)))))
      (#t (cons (car lat) (multiinsert.right value at (cdr lat)))))))

; The Fourth Commandment
; Always change at least one argument while recurring. It
; must be changed to be closer to termination. The changing
; argument must be tested in the termination condition:
; when using cdr, test termination with null? and
; when using sub1, test termination with zero?

(number? 3) ; #t
(add1 1) ; 2
(sub1 2) ; 1
(zero? 0) ; #t
(zero? -0) ; #t
(zero? 1) ; #f
(+ 1 2) ; 3
(- 2 1) ; 1

(define +
  (lambda (x y)
    (cond
      ((zero? y) x)
      (#t (+ (add1 x) (sub1 y)))))) ; does not work for negative y's

(define -
  (lambda (x y)
    (cond
      ((zero? y) x)
      (#t (- (sub1 x) (sub1 y)))))) ; does not work for negative y's


(tup? '(2 3 4 5 6)) ; a tuple is a list of numbers, this is #t
(tup? '(2 3 a b 4)) ; #f
(tup? '()) ; #t


(define tup?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((number? (car l))


(define tup?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((number? (car l)) (tup? (cdr l)))
      (#t #f))))

(define addtup
  (lambda (l)
    (cond
      ((null? l) 0)
      (#t (+ (car l) (addtup (cdr l)))))))

; The Fifth Commandment
; When building a value with +, always use 0 for the value of the terminating
; line, for adding 0 does not change the value of an addition.
; When building a value with *, always use 1 for the value of the terminating
; line, for multiplying by 1 does not change the value of a multiplication.
; When building a value with cons, always consider () for the value of the
; terminating line.

(tup+ '(1 2 3 4 5) '(2 3 4 5 6)) ; (3 5 7 9 11)

(define tup+
  (lambda (tup0 tup1)
    (cond
      ((null? tup0) tup1)
      ((null? tup1) tup0)
      (#t (cons (+ (car tup0) (car tup1)) (tup+ (cdr tup0) (cdr tup1)))))))

(define >
  (lambda (x y)
    (cond
      ((zero? x) #f)
      ((zero? y) #t)
      (#t (> (sub1 x) (sub1 y))))))

(define length
  (lambda (l)
    (cond
      ((null? l) 0)
      (#t (add1 (length (cdr l)))))))

; (pick n lat)
(define pick
  (lambda (n lat)
    (cond
      ((null?  lat) #f)
      ((eq? n 1) (car lat))
      (#t (pick (sub1 n) (cdr lat))))))

(define no-nums
  (lambda (lat)
    (cond
      [(null? lat)           lat]
      [(number? (car lat))   (no-nums (cdr lat))]
      [#t                    (cons (car lat) (no-nums (cdr lat)))])))

(define rempick
  (lambda (n lat)
    (cond
      [{null? lat}  lat]
      [(= n 1)      (cdr lat)]
      [#t           (cons (car lat) (rempick (sub1 n) (cdr lat)))])))

(define rember*
  (lambda (a l)
    (cond
      [(null? l)               l]
      [(eq? a (car l))         (rember* a (cdr l))]
      [(not (atom? (car l)))   (cons (rember* a (car l)) (rember* a (cdr l)))]
      [#t                      (cons (car l) (rember* a (cdr l)))])))
