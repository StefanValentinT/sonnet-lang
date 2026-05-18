(def some-symbol :symbol)

(def numbers :(10 1.3))
#(type numbers :(i32 f64))

(def fib (fun (n)
	(if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))
))

(def is-even (fun x
	#(type x i32)
	(if (eq x 0) true (is-odd (- x 1)))
))
#(type is-even (fun-type i32 (type-union true-type false-type)))

(def some-string (concat "hello " "world" "!"))
#(type some-string string-type)

(def is-odd (fun x
	#(type x i32)
	#(def is-zero (eq x 0))
	(if (eq x 0) false (is-even (- x 1)))
))

#(type is-odd (fun-type i32 (type-union true-type false-type)))

(def main
	(fun ()
		(print some-string)
		(print (fib (get numbers 0)))
		(print ((fun x x) 10))
		(print "Is 7 even: " (is-even 7))
	))