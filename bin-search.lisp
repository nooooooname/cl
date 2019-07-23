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



(defun bin-search (obj vec &key (key #'identity) (test #'-) (start 0) end)
  "从升序向量vec中二分搜索元素obj，找到就返回obj，找不到则返回nil。
  元素的大小由:test的返回值定义，(funcall test a b)的返回值若大于0则 a > b，若小于0则 a < b，若等于0则 a = b。
  :start是搜索的起始位置，:end是搜索的结束位置，若:end没有给定，则搜索到向量的最后一个元素。
  :key是在比较之前作用于向量中元素的函数。"
  (let* ((len (length vec))
	 (contain (lambda (x) (and (>= x 0) (< x len)))))
    (if (null end)
      (setf end (- len 1)))
    (if (or (> start end)
	    (not (funcall contain start))
	    (not (funcall contain end)))
      (return-from bin-search nil)))
  (let ((range (- end start)))
    (if (zerop range)
      (if (zerop (funcall test obj (funcall key (svref vec start))))
	obj
	nil)
      (let* ((mid (+ start (round (/ range 2))))
	     (obj2 (funcall key (svref vec mid))))
	(if (< (funcall test obj obj2) 0)
	  (bin-search obj vec :key key :test test :start start :end (- mid 1))
	  (if (> (funcall test obj obj2) 0)
	    (bin-search obj vec :key key :test test :start (+ mid 1) :end end)
	    obj))))))



(defun cons-testcase (&key (true t) (maxlen 100))
  "上述函数的测试用例生成器。
  第一个返回值是obj，第二个返回值是vec。
  :maxlen用来指定vec的最大长度。
  obj是否在vec中由参数:true的真与假来控制。"
  (let* ((len (do ((r (random maxlen) (random maxlen)))
		((not (zerop r)) r)))
	 (eltlist nil)
	 (vec (dotimes (n len (sort (apply #'vector eltlist) #'<))
		(setf eltlist (append eltlist (list (random 100000)))))))
    (if true
      (values (svref vec (random len)) vec)
      (do ((obj (random 100000) (random 100000)))
	((not (position obj vec)) (values obj vec))))))
