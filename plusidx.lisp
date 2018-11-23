(defun plusidx (s)
  "返回一个数字列表s中所有元素加上其索引值之后的新列表，
  不检查s是不是数字列表。"
  (and (consp s)
       (let ((idx 0))
	 (mapcar #'(lambda (x) (setf idx (+ idx 1))
		     (+ x (- idx 1)))
		 s))))
