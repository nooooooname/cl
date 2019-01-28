(defun string-integer (str)
  "字符串转整数，参数不合法则返回nil"
  (and (stringp str)
       (every #'digit-char-p str)
       (not (zerop (length str)))
       (let ((res 0))
	 (dotimes (pos (length str))
	   (setf res (+ (* res 10)
			(digit-char-p (char str pos)))))
	 res)))



;;every把第二个参数作为一个序列遍历其中的元素，
;;第一个参数作为测试函数测试每次遍历的元素，
;;测试函数返回T时继续遍历，否则every返回nil，
;;当遍历结束后every返回T。

;;digit-char-p接受一个字符参数，判断它是不是数字字符，
;;是则返回对应的整数，不是则返回nil。

;;(dotimes (迭代变量 终值) 循环体)
;;1.令迭代变量初值为0，执行第3步
;;2.执行循环体，迭代变量+1，循环体可以是多个语句
;;3.比较迭代变量和终值，若小于则回到第2步，
;;  若大于等于则dotimes返回nil，返回值与循环体无关。
