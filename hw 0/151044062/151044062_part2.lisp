(defun merger (left right)	; all items in right will be putted in left.
	(if (eq left NIL)		; if the left is null or not
		right		; TRUE: Create a list with right.
		(if (listp left)	; if left is a list or not.
			(cons (car left) (merger (cdr left) right))	; TRUE: create a list with first item of left and connect it with result of merge reucrsive call.
			(cons left right)							; connect left and right as list.
		)
	)
)