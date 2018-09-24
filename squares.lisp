(defun squares (start end)
  "循环计算从start到end的平方"
  (do ((i start (+ i 1)))
    ((> i end) t)
    (format t "~A ~A~%" i (* i i))))
