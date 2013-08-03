(def user1 {:login "rob" :referrer :mint.com :salary 100000})
(def user2 {:login "kyle" :referrer :google.com :salary 90000})
(def user3 {:login "celeste" :referrer :yahoo.com :salary 70000})

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
(defmulti profit-based-affiliate-fee fee-category)
(defmethod profit-based-affiliate-fee [:mint.com ::bronze] [user]
	(fee-amount 0.03 user))
(defmethod profit-based-affiliate-fee [:mint.com ::silver] [user]
	(fee-amount 0.04 user))
(defmethod profit-based-affiliate-fee [:mint.com ::gold] [user]
	(fee-amount 0.05 user))
(defmethod profit-based-affiliate-fee [:mint.com ::platinum] [user]
	(fee-amount 0.05 user))
(defmethod profit-based-affiliate-fee [:google.com ::gold] [user]
	(fee-amount 0.03 user))
(defmethod profit-based-affiliate-fee [:google.com ::platinum] [user]
	(fee-amount 0.03 user))
(defmethod profit-based-affiliate-fee :default [user]
	(fee-amount 0.02 user))