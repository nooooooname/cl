(defun factorial (n)
  "n的阶乘"
  (and (numberp n)
       (if (< n 2)
	 1
	 (reduce #'* (range 1 n)))))

(defun range (a b)
  "返回从a到b的整数组成的升序列表，若a>b则返回nil。"
  (do ((i a (+ i 1))
       (ret nil))
    ((> i b) ret)
    (setf ret (append ret (list i)))))

(defun factorial (n)
  "n的阶乘，若参数不合法则返回nil"
  (and (integerp n)
       (cond
	 ((< n 0) nil)
	 ((zerop n) 1)
	 (t (do* ((j 1 (+ j 1))
		 (f 1 (* f j)))
	      ((= j n) f))))))
