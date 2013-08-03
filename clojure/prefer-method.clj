(derive ::programmer ::employee)
(derive ::programmer ::geek)

(def user {:name "alice" :type ::programmer})

(defmulti foo :type)

; :type user would put alice in both ::geek and ::employee method, prefer-method solves this...
(prefer-method foo ::geek ::employee)

(defmethod foo ::employee [user]
	(println (user :name) "is employee"))

(defmethod foo ::geek [user]
	(println (user :name) "is a geek!"))

(defmethod foo :default [user]
	(println (user :name) "is default."))