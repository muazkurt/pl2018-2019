
(setq num_legal	"0123456789")
(setq id_legal 	"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXVZ")




(defun open_read_close (filename)
	(let ((in (open filename :if-does-not-exist nil)))
		(write (read-line in nil))
	(close in))
)




(defun string_matcher (left right)
	(if (equal 0 (length left))
		(if (equal ' ' (char right 0)))
		T
		(if (equal (char left 0) (char right 0))
			(string_matcher (subseq left 1) (subseq right 1))
			nil
		)
	)
)







(defun ID_PARSER (input)
	(if (or (null input) (equal (length input) 0))
		nil
		(if (find (char input 0) id_legal :test #'equalp)
			(cons (char input 0) (ID_PARSER (subseq input 1)))
			nil
		)
	)
)