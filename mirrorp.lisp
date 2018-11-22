(defun mirror? (s)
  "判断一个序列是不是回文序列，
  若s是序列且回文返回T，否则返回nil。"
  (and (or (consp s) (vectorp s))
       (let ((len (length s)))
	 (let ((mid (if (evenp len)
		      (/ len 2)
		      (/ (- len 1) 2))))
	   (equal (subseq s 0 mid)
		  (reverse (subseq s (- len mid))))))))
