(defun str-int (str)
  (and (stringp str)
       (let ((ret 0))
	 (dotimes (pos (length str))
	   (let ((i (digit-char-p (char str pos))))
	     (if i
	       (setf ret (+ (* ret 10) i))
	       (return-from str-int))))
	 ret)))
