(defn greet
	([]
		(greet "Hello" "World"))
	([name]
		(greet "Hello" name))
	([greeting name]
		(str greeting name)))

(println (greet))
(println (greet "you"))
(println (greet "hi" "you"))println