(defun our-equal (x y)
  "判断2个符号列表是否相等"
  (or (eql x y)
      (and (consp x)
	   (consp y)
	   (our-equal (car x) (car y))
	   (our-equal (cdr x) (cdr y)))))
