(setq num_legal	"0123456789")
(setq id_legal	"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXVZ")


(defun read_all_lines (fd)
	(setq line (read-line in nil))
	(if (null line)
		line
		(if (equal (length line) 0)
			(read_all_lines fd)
			(cons (clean_empty (ignore_white_spaces line)) (read_all_lines fd))
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


(defun open_read_close (filename)
	(setq in (open filename :if-does-not-exist nil))
	(if (null in)
		nil
		(and (setq all (read_all_lines in)) (close in))
	)
	all
)



(defun string_matcher (left right)
	(if (equal 0 (length left))
		(if (equal #\SPACE (char right 0)))
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