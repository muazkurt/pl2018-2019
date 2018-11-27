(setq num_legal	"0123456789")
(setq id_legal	"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXVZ")
(setq key_legal (list "and" "or" "not" "equal" "append" "concat" "set" "deffun" "for" "while" "if" "exit"))
(setq bin_legal (list "true" "false"))
(setq opt_legal (list "*" "**" "/" "+" "-" ")" "("))

(defun lexer (inputfilename)
	(tokanize (open_read_close inputfilename))
)

(defun tokanize (contents)
	(if (null contents)
		contents
		(cond
			((find_inList (car contents) opt_legal) (cons (list "Operator" (car contents)) (tokanize (cdr contents))))
			((find_inList (car contents) bin_legal) (cons (list "Binary Value" (car contents)) (tokanize (cdr contents))))
			((find_inList (car contents) key_legal) (cons (list "Keyword" (car contents)) (tokanize (cdr contents))))
			((check_INT (car contents)) (cons (list "Integer Value" (car contents)) (tokanize (cdr contents))))
			((ID_PARSER (car contents)) (cons (list "Id" (car contents)) (tokanize (cdr contents))))
			(T (write "Unknown input type") (write-line (car contents)))
		)
	)
)

(defun open_read_close (filename)
	(setq in (open filename :if-does-not-exist nil))
	(if (null in)
		nil
		(and (setq all (read_all_lines in)) (close in))
	)
	(find** (sub_parse (clean_empty all)))
)

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

(defun find_min (listofIntegers cur_minimum)
	(if (null listofIntegers)
		cur_minimum
		(if (null (car listofIntegers))
			(find_min (cdr listofIntegers) cur_minimum)
		 	(if (> (car listofIntegers) cur_minimum)
				(find_min (cdr listofIntegers) cur_minimum)
				(find_min (cdr listofIntegers) (car listofIntegers))
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

(defun find** (parsed_file_content)
	(if (null parsed_file_content)
		parsed_file_content
		(if (equal #\* (char (car parsed_file_content) 0))
			(if (null (cdr parsed_file_content))
				parsed_file_content
				(if (equal #\* (char (car (cdr parsed_file_content)) 0))
					(append (list (concatenate 'string (car parsed_file_content) (car (cdr parsed_file_content)))) 
						(find** (cdr (cdr parsed_file_content))))
					(append (list (car parsed_file_content)) (find** (cdr parsed_file_content)))
				)
			)
			(append (list (car parsed_file_content)) (find** (cdr parsed_file_content)))
		)
	)
)

(defun find_inList (input keyword_list)
	(if (null keyword_list)
		nil
		(if (string= input (car keyword_list))
			T
			(find_inList input (cdr keyword_list))
		)
	)
)

(defun check_INT (input)
	(if (> (length input) 1)
		(if (equal 
				(position (char num_legal 0) input)
				(find_min (list 
					(position (char num_legal 1) input)
					(position (char num_legal 2) input)
					(position (char num_legal 3) input)
					(position (char num_legal 4) input)
					(position (char num_legal 5) input)
					(position (char num_legal 6) input)
					(position (char num_legal 7) input)
					(position (char num_legal 8) input)
					(position (char num_legal 9) input))
					(length input)))
			nil
			(if (equal #\- (char input 0))
				(INT_PARSER (subseq input 1))
				(INT_PARSER input)
			)
		)
		(INT_PARSER input)
	)
)

(defun INT_PARSER (input)
	(if (or (null input) (equal (length input) 0))
		T
		(if (find (char input 0) num_legal :test #'equal)
			(INT_PARSER (subseq  input 1))
			nil
		)
	)
)

(defun ID_PARSER (input)
	(if (or (null input) (equal (length input) 0))
		T
		(if (find (char input 0) id_legal :test #'equal)
			(ID_PARSER (subseq  input 1))
			nil
		)
	)
)