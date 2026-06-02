#((fun (add x) (x add x)) (syn a b (+ a b)) 10)

#((fun (incr x) (incr x)) (syn x (set x (+ x 1))) 10)

#(let-syn incr () x
#	(set x (+ x 1))

(do


(def-syntax incr () x
	'(set x (+ x 1)))
(do
	(var x 5)
	(print x)
	(incr x)
	(print x)
)


(do
	(def-syntax comment ls rs (splice ls) (splice rs))
	(var x 10 (comment This is ignored!))
	(comment This too!)
	(set x (+ x 10))
	(print x)
	0
)

)