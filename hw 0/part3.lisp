(load "part2.lisp")
(
	defun inserti (inputList i object) 
	(
		if (eq i 0) 
		(
			return-from inserti (merger (list object) inputList)
		) 
		(
			if (eq inputList NIL) 
			(
				return-from inserti (list object)
			) 
			(
				return-from inserti (cons (car inputList) (inserti (cdr inputList) (- i 1) object))
			)
		)
	)
)