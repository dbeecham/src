(def user1 {:login "rob" :referrer :mint.com :salary 100000})
(def user2 {:login "kyle" :referrer :google.com :salary 90000})
(def user3 {:login "celeste" :referrer :yahoo.com :salary 70000})

; build a hierarchy
(derive ::bronze ::basic)
(derive ::silver ::basic)
(derive ::gold ::premier)
(derive ::platinum ::premier)

(defn fee-amount [percentage user]
	(->> user
		(:salary)
		(* 0.01 percentage)
		(float)))

(defn profit-rating 
	"Returns a random profit rating."
	[user]
	(let [ratings [::bronze ::silver ::gold ::platinum]]
		(nth ratings (rand-int (count ratings)))))

(defn fee-category [user]
	[(:referrer user) (profit-rating user)])


; multiple (double) dispatch (over fee-category, which is [referrer profit-rating])
(defmulti affiliate-fee-for-hierarchy fee-category)
(defmethod affiliate-fee-for-hierarchy [:mint.com ::bronze] [user]
	(fee-amount 0.03 user))
(defmethod affiliate-fee-for-hierarchy [:mint.com ::silver] [user]
	(fee-amount 0.04 user))
(defmethod affiliate-fee-for-hierarchy [:mint.com ::premier] [user]
	(fee-amount 0.05 user))
(defmethod affiliate-fee-for-hierarchy [:google.com ::premier] [user]
	(fee-amount 0.03 user))
(defmethod affiliate-fee-for-hierarchy :default [user]
	(fee-amount 0.02 user))