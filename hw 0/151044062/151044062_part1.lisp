(load "./151044062_part2.lisp")	; import merger func from created within this homework.
(defun checker (a)				
	(if (not (eq a NIL))		; if given input is not empty.
		(if (listp a)			; and the input is a list
			(if (not (eq (car a) NIL))	; and first item of the list is not null
				(if (listp (car a)) 	; if the first item is a LIST!!!11
					(checker (merger (car a) (cdr a))) 	; TRUE:  call checker with merged version of the list by first item.
					(cons (car a) (checker (cdr a)))	; FALSE: check then next element and connect it with the first element as a list.
				)
			)
		)
	)
)