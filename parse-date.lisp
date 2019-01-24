(defconstant month-names	;;各月份简写
	     #("jan" "feb" "mar" "apr" "may" "jun"
	       "jul" "aug" "sep" "oct" "nov" "dec"))

(defun tokens (str test start)
  "辨别符号
  在字符串str中以索引start为起始位置截取符号字符串组成列表并返回
  test是符号字符判断函数，
  当以某个字符为参数调用它返回nil时该字符视为符号字符串间的分隔符
  否则该字符就视为符号字符串的一部分"
  (let ((p1 (position-if test str :start start)))
    (if p1
      (let ((p2 (position-if #'(lambda (c)
				 (not (funcall test c)))
			     str :start p1)))
	(cons (subseq str p1 p2)
	      (if p2
		(tokens str test p2)
		nil)))
      nil)))

(defun constituent (c)
  "判断参数c是不是非空格的可打印字符"
  (and (characterp c)
       (graphic-char-p c)
       (not (char= c #\ ))))

(defun parse-month (str)
  "月份简写str转数字，若str不是月份简写则返回nil"
  (and (stringp str)
       (let ((p (position str month-names
			  :test #'string-equal)))	;; string-equal默认忽略大小写
	 (if p
	   (+ p 1)
	   nil))))

(defun parse-date (str)
  "把形如“16 Aug 1980”的日期字符串转成形如(1980 8 16)的列表
  若参数不合法则返回nil，但不检查参数的正确性"
  (and (stringp str)
       (let ((token-list (tokens str #'constituent 0)))
	 (and (eql (length token-list) 3)
	      (list (parse-integer (third token-list))
		    (parse-month (elt token-list 1))
		    (parse-integer (first token-list)))))))
