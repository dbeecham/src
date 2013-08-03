(def all-users (ref {})) ; (ref initial-value & options)
(deref all-users) ; {}
@all-users ; {}, reader macro for deref

(ref-set all-users {}) ; error, no transaction running

(dosync
	(ref-set all-users {})) ; ok

(dosync
	(alter all-users assoc :name "John"))

(dosync
	(commute all-users assoc :value 3))