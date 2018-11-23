(defun mirror? (s)
  "判断一个序列是不是回文序列，
  若s是序列且回文返回T，否则返回nil。"
  (and (or (consp s) (vectorp s))
       (let ((len (length s)))
	 (do ((forward 0 (+ forward 1))
	      (back (- len 1) (- back 1)))
	   ((or (>= forward back)
		(not (eql (elt s forward)
			  (elt s back))))
	    (>= forward back))))))
