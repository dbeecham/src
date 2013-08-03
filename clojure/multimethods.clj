; multimethods are declared using the defmulti macro
; (defmulti name dispatch-fn & options)
; defmethod defines a concrete implementation for a previously defined multimethod
; (defmethod multifn dispatch-value & fn-tail)

(defn fee-amount [percentage user]
	(->> user
		(:salary)
		(* 0.01 percentage)
		(float)))

; single dispatch (over :referrer)
(defmulti affiliate-fee :referrer)

(defmethod affiliate-fee :mint.com [user]
	(fee-amount 0.03 user))

(defmethod affiliate-fee :google.com [user]
	(fee-amount 0.01 user))

(defmethod affiliate-fee :default [user]
	(fee-amount 0.02 user))

(def user1 {:login "rob" :referrer :mint.com :salary 100000})
(def user2 {:login "kyle" :referrer :google.com :salary 90000})
(def user3 {:login "celeste" :referrer :yahoo.com :salary 70000})

(affiliate-fee user1) ; 30
(affiliate-fee user2) ; 9
(affiliate-fee user3) ; 14