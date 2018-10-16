(load "./part2.lisp")
(
	defun checker (a) 
	(
		if (listp a) 
		(
			if (listp (car a)) 
			(
				return-from checker (checker (setf a (merger (car a) (cdr a))))
			) 
			(
				return-from checker (checker (cdr a))
			)
		) 
		(
			return-from checker NIL
		)
	) 
)