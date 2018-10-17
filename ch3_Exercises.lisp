;; 1.用箱子表示法表示以下列表：(略)

;; 2.写一个保留原本列表中元素顺序的 union 版本：
; a:
(defun new-union (lst1 lst2)
  (if (null lst2)
    lst1
    (let ((elemt (car lst2)))
      (if (member elemt lst1)
	(new-union lst1 (cdr lst2))
	(new-union (append lst1
			   (list elemt))
		   (cdr lst2))))))

;; 3.定义一个函数，接受一个列表并返回一个列表，指出相等元素出现的次数，并由最常见至最少见的排序：
; a:
(defun occurrences (lst)
  (if (null lst)
    nil
    (let ((ret (occurrences (cdr lst))) (elemt (car lst)))
      (sort
	(if (member elemt ret :key #'car)
	  (mapcar #'(lambda (c)
		      (if (eql (car c) elemt)
			(cons elemt (+ (cdr c) 1))
			c))
		  ret)
	  (append ret (list (cons elemt 1))))
	#'(lambda (x y) (> (cdr x) (cdr y)))))))

;; 4.为什么 (member '(a) '((a) (b))) 返回 nil？
; a: 因为(eql '(a) '(a))返回nil

;; 5.假设函数 pos+ 接受一个列表并返回把每个元素加上自己的位置的列表：
; > (pos+ '(7 5 1 4))
;   (7 6 3 7)
;    使用 (a) 递归 (b) 迭代 (c) mapcar 来定义这个函数。
; a:
;  (a)
(defun pos+ (lst)
