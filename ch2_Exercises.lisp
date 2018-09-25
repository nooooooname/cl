;; 1.描述下列表达式求值之后的结果：
;  (a)
(+ (- 5 1) (+ 3 7))	;a:	14
;  (b)
(list 1 (+ 2 3))	;a:	(1 5)
;  (c)
(if (listp 1) (+ 1 2) (+ 3 4))	;a:	7
;  (d)
(list (and (listp 3) t) (+ 1 2));a:	(NIL 3)

;; 2.给出 3 种不同表示 (a b c) 的 cons 表达式。
; a:
(cons 'a '(b c))
(cons 'a (cons 'b '(c)))
(cons 'a (cons 'b (cons 'c ())))

;; 3.使用 car 与 cdr 来定义一个函数，返回一个列表的第四个元素。
; a:
(defun 4th (lst)
  "返回一个列表的第四个元素"
  (car (cdr (cdr (cdr lst)))))

;; 4.定义一个函数，接受两个实参，返回两者当中较大的那个。
; a:
(defun bigger (a b)
  "返回2个实参中的最大值"
  (if (> a b)
    a
    b))

;; 5.这些函数做了什么？
;  (a)
(defun enigma (x)	;a:	判断列表x中有没有nil,若x不是列表则报错
  (and (not (null x))
       (or (null (car x))
	   (enigma (cdr x)))))
;  (b)
(defun mystery (x y)	;a:	返回元素x是列表y的左数第几项(从0开始计),不在返回nil,若y不是列表则报错
  (if (null y)
    nil
    (if (eql (car y) x)
      0
      (let ((z (mystery x (cdr y))))
	(and z (+ z 1))))))

;; 6.下列表达式， x 该是什么，才会得到相同的结果？
;  (a)
; > (car (x (cdr '(a (b c) d))))
;   B
; a:	car
;  (b)
; > (x 13 (/ 1 0))
;   13
; a:	or
;  (c)
; > (x #'list 1 nil)
;   (1)
; a:	apply

;; 7.只使用本章所介绍的操作符，定义一个函数，它接受一个列表作为实参，如果有一个元素是列表时，就返回真。
; a:
(defun youlist-R (lst)
  "递归判断列表lst中有没有列表，若lst不是列表则返回nil"
  (and (listp lst)
       (not (null lst))
       (if (listp (car lst))
	 T
	 (youlist-R (cdr lst)))))
(defun youlist-I (lst)
  "迭代判断列表lst中有没有列表，若lst不是列表则返回nil"
  (and (listp lst)
       (let ((ret nil))
	 (dolist (obj lst)
	   (setf ret (or ret (listp obj))))
	 ret)))

;; 8.给出函数的迭代与递归版本：
;  a.接受一个正整数，并打印出数字数量的点。
; a:
(defun point-I (num)
  (do ((i num (/ i 10)))
    ((< i 1) t)
    (format t ".")))
(defun point-R (num)
  (if (< num 1)
    t
    (progn
      (format t ".")
      (point-R (/ num 10)))))
;  b.接受一个列表，并返回 a 在列表里所出现的次数。
; a:
(defun acnt-I (lst)
  (if (listp lst)
    (let ((ret 0))
      (dolist (obj lst)
	(if (eql obj 'a)
	  (setf ret (+ ret 1))))
      ret)
    nil))
(defun acnt-R (lst)
  (if (not (listp lst))
    nil
    (if (null lst)
      0
      (if (eql (car lst) 'a)
	(+ 1 (acnt-R (cdr lst)))
	(acnt-R (cdr lst))))))

;; 9.一位朋友想写一个函数，返回列表里所有非 nil 元素的和。他写了此函数的两个版本，但两个都不能工作。请解释每一个的错误在哪里，并给出正确的版本。
;  (a)
(defun summit (lst)
  (remove nil lst)
  (apply #'+ lst))
; a:	remove没有副作用
(defun summit (lst)
  (apply #'+ (remove nil lst)))
;  (b)
(defun summit (lst)
  (let ((x (car lst)))
    (if (null x)
      (summit (cdr lst))
      (+ x (summit (cdr lst))))))
; a:	缺少回溯
(defun summit (lst)
  (if (null lst)
    0
    (let ((x (car lst)))
      (if (null x)
	(summit (cdr lst))
	(+ x (summit (cdr lst)))))))
