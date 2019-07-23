;; 1.定义一个 tokens 版本 (67 页)，接受 :test 与 :start 参数，缺省分别是 #'constituent 与 0 。(67 页在 4.5 小节)
; a:
(defun tokens (str &key (test #'constituent) (start 0))
  (let ((p1 (position-if test str :start start)))
    (if p1
      (let ((p2 (position-if #'(lambda (c)
				 (not (funcall test c)))
			     str :start p1)))
	(cons (subseq str p1 p2)
	      (if p2
		(tokens str :test test :start p2)
		nil)))
      nil)))


;; 2.定义一个 bin-search (60 页)的版本，接受 :key , :test , start 与 end 参数，有着一般的意义与缺省值。(60 页在 4.2 小节)
; a:
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


;; 3.定义一个函数，接受任何数目的参数，并返回传入的参数。
; a:
(defun argval (&rest args)
  (apply #'values args))


;; 4.修改 most 函数 (105 页)，使其返回 2 个数值，一个列表中最高分的两个元素。(105 页在 6.4 小节)
; a:
(defun most (fn lst)
  (if (null lst)
    (values nil nil)
    (if (< (length lst) 2)
      (values (car lst) nil)
      (let ((deslist (sort lst
			   (lambda (a b) (> (funcall fn a)
					    (funcall fn b))))))
	(values (car deslist) (cadr deslist))))))


;; 5.用 filter (105 页) 来定义 remove-if （没有关键字）。(105 页在 6.4 小节)
; a:
(defun remove-if (fn seq)
  (typecase seq
    (list
      (filter (lambda (x)
		(and (not (funcall fn x)) x))
	      seq))
    (string
      (let* ((lst nil)
	     (seq-list (dotimes (i (length seq) (nreverse lst))
			 (push (char seq i) lst)))
	     (res-list (filter (lambda (x)
				 (and (not (funcall fn x)) x))
			       seq-list))
	     (res ""))
	(dolist (x res-list res)
	  (setf res (concatenate 'string res (string x))))))
    (vector
      (let* ((lst nil)
	     (seq-list (dotimes (i (length seq) (nreverse lst))
			 (push (svref seq i) lst)))
	     (res-list (filter (lambda (x)
				 (and (not (funcall fn x)) x))
			       seq-list)))
	(apply #'vector res-list)))))


;; 6.定义一个函数，接受一个参数丶一个数字，并返回目前传入参数中最大的那个。
; a:
(let ((maxium nil))
  (defun hismax (x)
    (if (null maxium)
      (setf maxium x))
    (if (> x maxium)
      (setf maxium x)
      maxium)))


;; 7.定义一个函数，接受一个参数丶一个数字，若传入参数比上个参数大时，返回真。函数第一次调用时应返回 nil 。
; a:
(let ((lastarg nil))
  (defun gtlast (x)
    (if (null lastarg)
      (progn (setf lastarg x)
	     nil)
      (let ((res (> x lastarg)))
	(setf lastarg x)
	res))))


;; 8.假设 expensive 是一个接受一个参数的函数，一个介于 0 至 100 的整数（包含 100)，返回一个耗时的计算结果。定义一个函数 frugal 来返回同样的答案，但仅在没见过传入参数时调用 expensive 。
; a:
(let ((cache (make-hash-table)))
  (defun frugal (x)
    (multiple-value-bind (res exist) (gethash x cache)
      (if exist
	res
	(progn
	  (setf res (expensive x))
	  (setf (gethash x cache) res)
	  res)))))


;; 9.定义一个像是 apply 的函数，但在任何数字印出前，缺省用 8 进制印出。
; a:
(defun appl8 (&rest args)
  (let ((*print-base* 8))
    (apply #'apply args)))
