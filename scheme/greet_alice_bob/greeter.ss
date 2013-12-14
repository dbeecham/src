(use extras)

(let ((name (read-line)))
  (cond ((or (equal? name "Alice") (equal? name "Bob")) (print "Hello " name "!\n"))
        (#t (print "No hello for you!"))))
