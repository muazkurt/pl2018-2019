(load "./151044062_part2.lisp")
(defun inserti (inputList object target) 		; inputlist is the list to insert item, target is the position, object is what to put.
	(if (eq target 0) 							; if target is 0
		(cons object inputList)					; TRUE: create a list with object and inputlist
		(if (atom inputList) 					; if inputlist is an atom or not
			(list object)						; TRUE: Create a list with object
			(merger (car inputList) (inserti (cdr inputList) object (- target 1))) ; FALSE: merge first item of the inputlist and the result of recursive call's result.
		)
	)
)