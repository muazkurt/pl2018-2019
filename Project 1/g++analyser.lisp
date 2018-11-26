(setq num_legal	"0123456789")
(setq id_legal	"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXVZ")


(defun read_all_lines (fd)
	(setq line (read-line in nil))
	(if (null line)
		line
		(if (equal (length line) 0)
			(read_all_lines fd)
			(cons line (read_all_lines fd))
		)
	)
)

(defun clean_empty (garbage)
	(cond
		((null garbage) nil)
		((or (null (car garbage)) (equal 0 (length (car garbage)))) (clean_empty (cdr garbage)))
		(T (cons (car garbage) (clean_empty (cdr garbage))))
	)
)

(defun ignore_white_spaces (aLine)
	(cond 
		((null aLine) aLine)
		(T
			(setq startp 0)
			(setq pos_space (position #\SPACE aLine :test #'equal))
			(setq pos_tab (position #\TAB aLine :test #'equal))
			(cond
				((and (null pos_space) (null pos_tab)) (list aLine))
				((null pos_space) (cons (subseq aLine startp pos_tab) (ignore_white_spaces (subseq aLine (+ 1 pos_tab)))))
				((null pos_tab) (cons (subseq aLine startp pos_space) (ignore_white_spaces (subseq aLine (+ 1 pos_space)))))
				((> pos_space pos_tab) (cons (subseq aLine startp pos_tab) (ignore_white_spaces (subseq aLine (+ 1 pos_tab)))))
				(T (cons (subseq aLine startp pos_space) (ignore_white_spaces (subseq aLine (+ 1 pos_space)))))
			)
		)
	)
)

(defun operator_split (aString)
	(cond 
		((null aString) aString)
		(T
			(setq 	pos_operator
					(find_min (list (position #\+ aString :test #'equal)
							(if (null (setf temp (position #\- aString :test #'equal)))
								nil
								(if (< temp (- (length aString) 1))
									(if (null (position (char aString (+ 1 temp)) num_legal :test #'equal))
										temp
										nil)
									temp))
							(position #\/ aString :test #'equal)
							(position #\( aString :test #'equal)
							(position #\) aString :test #'equal)
							(position #\* aString :test #'equal))
						(length aString)))
			(if (equal pos_operator (length aString))
				(list aString)
				(if (equal 0 pos_operator)
					(append
						(list (subseq aString 0 (+ 1 pos_operator)))
						(operator_split (subseq aString (+ pos_operator 1))))
					(if (equal pos_operator (- (length aString) 1))
						(list 
							(subseq aString 0 pos_operator)
							(subseq aString pos_operator))
						(append
						(list 
							(subseq aString 0 pos_operator) 
							(subseq aString pos_operator (+ pos_operator 1)))
							(operator_split (subseq aString (+ pos_operator 1))))
					)
				)
			)

		)
	)
)

(defun find_min (aString cur_minimum)
	(if (null aString)
		cur_minimum
		(if (null (car aString))
			(find_min (cdr aString) cur_minimum)
		 	(if (> (car aString) cur_minimum)
				(find_min (cdr aString) cur_minimum)
				(find_min (cdr aString) (car aString))
			)
		)
	)
)

(defun collect_splited (aList)
	(if (null aList)
		aList
		(append (sub_parse (car aList)) (collect_splited (cdr aList)))
	)
)

(defun sub_parse (aList)
	(if (null aList)
		aList
		(append (subsub (clean_empty (ignore_white_spaces (car aList)))) (sub_parse (cdr aList)))
	)
)

(defun subsub (alist)
	(if (null aList)
		aList
		(append (clean_empty (operator_split (car aList))) (subsub (cdr aList)))
	)
)

(defun open_read_close (filename)
	(setq in (open filename :if-does-not-exist nil))
	(if (null in)
		nil
		(and (setq all (read_all_lines in)) (close in))
	)
	 (sub_parse (clean_empty all))
)



(defun string_matcher (left right)
	(if (equal 0 (length left))
		T
		(if (equal (char left 0) (char right 0))
			(string_matcher (subseq left 1) (subseq right 1))
			nil
		)
	)
)

(defun Sign_Parser (input)
	(if (equal '#\- (char input 0))
		(cons '#\- (Int_Parser (subseq input 1)))
		(Int_Parser input)
	)
)

(defun Int_Parser (input)
	(if (equal '#\0 (char input 0)))
		

)

(defun ID_PARSER (input)
	(if (or (null input) (equal (length input) 0))
		nil
		(if (find (char input 0) id_legal :test #'equalp)
			(cons (char input 0) (ID_PARSER (subseq  input 1)))
			nil
		)
	)
)