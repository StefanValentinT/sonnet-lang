(def some-symbol :symbol)

(def numbers :(10 1.3))

(def fib
	(fun (n)
		(if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))
	))

(def is-even (fun x
		(if (eq x 0) :true (is-odd (- x 1)))
	))

(def is-odd (fun x
		(if (eq x 0) :false (is-even (- x 1)))
	))

(def main
	(fun ()
		(print "hello world")
		(print (fib (list/get numbers 0)))
		(print ((fun x x) 10))
		(print "Is 7 even: " (is-even 7))
	))