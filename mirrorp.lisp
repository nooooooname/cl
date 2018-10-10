(defun mirror? (s)
  "判断一个列表是不是回文列表，若s不是cons对象则返回nil"
  (and (consp s)
       (let ((len (length s)))
	 (let ((mid (if (evenp len)
		      (/ len 2)
		      (/ (- len 1) 2))))
	   (equal (subseq s 0 mid)
		(reverse (subseq s (- len mid))))))))
