(use extras)

(define partial
  (lambda [f . oargs]
    (lambda iargs
      (apply f (append oargs iargs)))))

(define guess-with-guesses
  (lambda [guesses x]
    (let ((guess (string->number (read-line))))
      (cond
        [(= guesses x) (print "Congratulations! You made it in " guesses " guesses!")]
        [(> guesses x) (begin (print "No, lower.") (guess-with-guesses (+ guesses 1) x))]
        [#t      (begin (print "No, higher.") (guess-with-guesses (+ guesses 1) x))]))))

(define guessn (partial guess-with-guesses 0))

(begin (print "Guess my number!") (guessn (random 101)))
