
((fun (let)
	(let
		(do (macro do-ls do-rs
			((fun (expand-do)
				(expand-do expand-do do-rs 0)
			) (fun (self statements index)
				(if (eq index (- (length statements) 1))
					(get statements index)
					((fun (current-stmt)
						(if (eq (get current-stmt 0) (quote def))
							(list
								(quote let)
								(list (get current-stmt 1) (get current-stmt 2))
								(self self statements (+ index 1))
							)
							(list
								(list (quote fun) (list (quote _discard)) 
									(self self statements (+ index 1)))
								current-stmt
							)
						)
					) (get statements index))
				)
			))
		))
		(let (def (macro def-ls def-rs
				(list (quote let) 
					(list (get def-rs 0) (get def-rs 1))
				)
			))
			(do
				(def when (macro when-ls when-rs
					(list
						(quote if)
						(get when-rs 0)
						(get when-rs 1)
						(quote nihil)
					)
				))
				(def comment (macro ls rs (quote (splice ls) (splice rs))))

				# start user code

				(def age 25 (comment whatever this is a comment))
				(def target 25)
				(comment does not care)
				(when (eq age target)
					(print "Success!")
				)

				(print (+ age 5))

				# end user code
			)
		)
	)
) (macro let-ls let-rs
	((fun (binding body)
		((fun (var val)
			(list 
				(list (quote fun) (list var) body) 
				val
			)
		) (get binding 0) (get binding 1))
	) (get let-rs 0) (get let-rs 1))
))