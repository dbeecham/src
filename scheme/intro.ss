(atom? (quote atom)) ; #t
(atom? 'atom) ; #t
(atom? '1492) ; #t
(atom? '*abc$) ; #t
(atom? '(3)) ; #f
(atom? 3) ; #t
(atom? '()) ; #f

(car '(1 2 3)) ; 1
(car '((1) (2) (3))) ; (1)
(car 3) ; no answer
(car '()) ; no answer
(null? '()) ; #t

; Law of Car
; The primitive car is defined
; only for non-empty lists.

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
; Always ask null? as the first question in expressing any function


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


(multiinsert value at l)
