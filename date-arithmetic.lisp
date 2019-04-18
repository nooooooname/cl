;;;日期计算：先把日期转换为距离日期原点多少天，通过+,-计算后再转换为日期
(defconstant month	;;从1月1号到1月,2月,...次年1月1号的天数
	     #(0 31 59 90 120 151 181 212 243 273 304 334 365))
(defconstant yzero 2000);;日期原点:2000年1月1号

(defun leap? (y)
  "判断年份y是(t)否(nil)是闰年"
  (if (not (integerp y))
    (error "leap?：错误：参数必须是整数！")
    (and (zerop (mod y 4))
	 (or (zerop (mod y 400))
	     (not (zerop (mod y 100)))))))

(defun date->num (y m d)
  "y年m月d日转成相对于日期原点的天数"
  (if (not (integerp y))
    (error "date->num：错误：参数y必须是整数！")
    (if (not (integerp m))
      (error "date->num：错误：参数m必须是整数！")
      (if (not (integerp d))
	(error "date->num：错误：参数d必须是整数！"))))
  (let ((res (+ (year-num y) (month-num m y) (- d 1))))
    (multiple-value-bind (yy mm dd) (num->date res)
      (if (or (not (= yy y))
	      (not (= mm m))
	      (not (= dd d)))
	(error "date->num：错误：日期不合法！")
	res))))

(defun year-num (y)
  "y年1月1日转成相对于日期原点的天数"
  (if (not (integerp y))
    (error "year-num：错误：参数必须是整数！")
    (let ((d 0))
      (if (> y yzero)
	(dotimes (i (- y yzero) d)
	  (incf d (year-days (+ i yzero))))
	(dotimes (i (- yzero y) d)
	  (decf d (year-days (+ y i))))))))

(defun month-num (m y)
  "y年m月1日相对于y年1月1日的天数"
  (if (not (integerp m))
    (error "month-num：错误：参数m必须是整数！")
    (if (not (integerp y))
      (error "month-num：错误：参数y必须是整数！")
      (if (or (< m 1) (> m 12))
	(error "month-num：错误：非法月份！")
	(+ (svref month (- m 1))
	   (if (and (leap? y) (> m 2)) 1 0))))))

(defun year-days (y)
  "年份y的天数"
  (if (not (integerp y))
    (error "year-days：错误：参数必须是整数！")
    (if (leap? y) 366 365)))

(defun num->date (n)
  "相对于日期原点的天数转日期
  3个返回值分别是年、月、日"
  (multiple-value-bind (y left) (num-year n)
    (multiple-value-bind (m d) (num-month left y)
      (values y m d))))

(defun num-year (n)
  "相对于日期原点的天数转年份
  第1个返回值是年份，若n不是这年1月1日则天数会有剩余
  第2个返回值是剩余天数"
  (if (not (integerp n))
    (error "num-year：错误：参数必须是整数！")
    (if (< n 0)
      (do* ((y (- yzero 1) (- y 1))
	    (d (- (year-days y)) (- d (year-days y))))
	((<= d n) (values y (- n d))))
      (do* ((y yzero (+ y 1))
	    (p 0 d)
	    (d (year-days y) (+ d (year-days y))))
	((> d n) (values y (- n p)))))))

(defun num-month (n y)
  "相对于y年1月1日的天数n转日期
  2个返回值分别是月份和几号"
  (if (not (integerp n))
    (error "num-month：错误：参数n必须是整数！")
    (if (not (integerp y))
      (error "num-month：错误：参数y必须是整数！")))
  (if (leap? y)
    (if (= n 59)
      (return-from num-month (values 2 29))
      (decf n (if (> n 59) 1 0))))
  (let* ((m (position n month :test #'<))
	 (d (svref month (- m 1))))
    (values m (+ (- n d) 1))))

(defun date+ (y m d n)
  "计算y年m月d日之后n天的日期
  若n<0则计算之前-n天的日期
  3个返回值分别是年、月、日"
  (num->date (+ (date->num y m d) n)))

(defun date- (y1 m1 d1 y2 m2 d2)
  "计算y1年m1月d1日比y2年m2月d2日晚几天
  若y1年m1月d1日比y2年m2月d2日早则返回负值"
  (- (date->num y1 m1 d1) (date->num y2 m2 d2)))
