(defun merger (left right)
	(if (eq left NIL)
		(list right)
		(if (listp left)
			(cons (car left) (merger (cdr left) right))
			(cons left right)
		)
	)
)