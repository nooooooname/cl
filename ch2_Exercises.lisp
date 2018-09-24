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
