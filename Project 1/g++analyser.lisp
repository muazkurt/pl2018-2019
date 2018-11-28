;Legal number chars string
(setq num_legal	"0123456789")
;Legal id chars string 
(setq id_legal	"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXVZ")
;Legal keywords in grammar.
(setq key_legal (list "and" "or" "not" "equal" "append" "concat" "set" "deffun" "for" "while" "if" "exit"))
;Legal binary words.
(setq bin_legal (list "true" "false"))
;Legal Operator list.
(setq opt_legal (list "*" "**" "/" "+" "-" ")" "("))


;Opens a g++ source file and check failure of it then print the result.
(defun lexer (inputfilename)
	(tokanize (open_read_close inputfilename))
)

;Checks a list of string depend on the language grammar rules.
;	If the input has a error, 
;		returns all legal strings and their types (as a pair of strings) 'til the error string.
;	Otherwise returns all the string pairs.
(defun tokanize (contents)
	(if (null contents)
		contents
		(cond
			((find_inList (car contents) opt_legal) (cons (list "Operator" (car contents)) (tokanize (cdr contents))))
			((find_inList (car contents) bin_legal) (cons (list "Binary Value" (car contents)) (tokanize (cdr contents))))
			((find_inList (car contents) key_legal) (cons (list "Keyword" (car contents)) (tokanize (cdr contents))))
			((check_INT (car contents)) (cons (list "Integer Value" (car contents)) (tokanize (cdr contents))))
			((ID_PARSER (car contents)) (cons (list "Id" (car contents)) (tokanize (cdr contents))))
			(T (write "Unknown input type ") (write-line (car contents)))
		)
	)
)


;Opens the given input named file. Reads all lines,
;	parses all the lines as ignoring whitespaces: #\TAB #\SPACE
; 	then returns a list of strings that containing parsed file contents.
(defun open_read_close (filename)
	(setq in (open filename :if-does-not-exist nil))
	(cond 
		((null in) (write "No such file") nil)
		(T (setq all (read_all_lines in)) 
			(close in)
			(find** (sub_parse (clean_empty all))))
	)
)

; Reads all the lines from input opened file descriptor.
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

;Input is a list of strings.
;	Clears all empty and 0 length list elements.
;	Returns it.
(defun clean_empty (garbage)
	(cond
		((null garbage) nil)
		((or (null (car garbage)) (equal 0 (length (car garbage)))) (clean_empty (cdr garbage)))
		(T (cons (car garbage) (clean_empty (cdr garbage))))
	)
)


;Input is a string. Finds all #\SPACE and #\TAB characters in the string.
;	Creates two new string and these chars are become starting/ending points.
;EX: "a#\SPACEb#\TABx" -> "a" "b" "x"
;Restul is a list of strings.
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


;	Finds all the inputs in the string and creates at least 2 new string for all operators.
;EX: "a*b" -> "a" "*" "b"
; Result is a list of strings.
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


;Searches for min element in the given listofInteger integer list.
;Result is the min element in list.
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

;	Driver method for splitting operators.
(defun collect_splited (aList)
	(if (null aList)
		aList
		(append (sub_parse (car aList)) (collect_splited (cdr aList)))
	)
)

;	Sub-Driver method for splitting operators.
(defun sub_parse (aList)
	(if (null aList)
		aList
		(append (subsub (clean_empty (ignore_white_spaces (car aList)))) (sub_parse (cdr aList)))
	)
)


;	SubSub-Driver method for splitting operators.
(defun subsub (alist)
	(if (null aList)
		aList
		(append (clean_empty (operator_split (car aList))) (subsub (cdr aList)))
	)
)


;	Searches for 							(... a_i a_i+1 ...) : a_i = a_i+1 = "*"
;	If finds such pair the result becomes 	(... a_i a_i+2 ...) : a_i = "**"
;	Takes a list of strings. Returns a list of strings, these strings are updateded in order to 182-183th line declerations.
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


;Takes 'input' as a string and 'keyword_list' as list of stirings.
;Searches for 'input' in all 'keyword_list' elements.
(defun find_inList (input keyword_list)
	(if (null keyword_list)
		nil
		(if (string= input (car keyword_list))
			T
			(find_inList input (cdr keyword_list))
		)
	)
)


;	Checks if the input string is compatable with the ( I -> [-]*[1-9]*[0-9]+ ) rule.
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


;	Checks if the input string's all chars are in the 'num_legal' string.
(defun INT_PARSER (input)
	(if (or (null input) (equal (length input) 0))
		T
		(if (find (char input 0) num_legal :test #'equal)
			(INT_PARSER (subseq  input 1))
			nil
		)
	)
)

;	Checks if the input string's all chars are in the 'id_legal' string.
(defun ID_PARSER (input)
	(if (or (null input) (equal (length input) 0))
		T
		(if (find (char input 0) id_legal :test #'equal)
			(ID_PARSER (subseq  input 1))
			nil
		)
	)
)