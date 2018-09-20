(defun sum (n)
  "计算从1到n的等差数列之和"
  (setq ret (/ (* (+ 1 n) n) 2))
  ret)

;例子
(print (sum 100))
