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
