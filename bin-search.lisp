(defun bin-search (obj vec)
  "从升序向量vec中二分查找元素obj，找到就返回obj，找不到则返回nil。"
  (let ((len (length vec)))
    (and (not (zerop len))
	 (finder obj vec 0 (- len 1)))))

(defun finder (obj vec start end)
  "从升序向量的子区间[start,end]中二分查找元素obj"
  (if (> start end)
    nil
    (let ((range (- end start)))
      (if (zerop range)
	(if (eql obj (svref vec start))
	  obj
	  nil)
	(let ((mid (+ start (round (/ range 2)))))
	  (let ((obj2 (svref vec mid)))
	    (if (> obj obj2)
	      (finder obj vec (+ mid 1) end)
	      (if (< obj obj2)
		(finder obj vec start (- mid 1))
		obj))))))))
