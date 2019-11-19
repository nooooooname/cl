;; 1.如果 x 是 a ， y 是 b 以及 z 是 (c d) ，写出反引用表达式仅包含产生下列结果之一的变量:
;  (a) ((C D) A Z)
; a:
`(,z ,x z)
;  (b) (X B C D)
; a:
`(x ,y ,@z)
;  (c) ((C D A) Z)
; a:
`((,@z ,x) z)


;; 2.使用 cond 来定义 if 。
; a:
(defmacro if (test then &optional else)
  `(cond
     (,test ,then)
     (t ,else)))


;; 3.定义一个宏，接受一个数字 n ，伴随着一个或多个表达式，并返回第 n 个表达式的值:
; > (let ((n 2))
;     (nth-expr n (/ 1 0) (+ 1 2) (/ 1 0)))
; 3
; a:
(defmacro nth-expr (n &rest exprs)
  (let ((key 0))
    `(case ,n
       ,@(mapcar (lambda (e) `(,(incf key) ,e))
		 exprs))))


;; 4.定义 ntimes (167 页，译注: 10.5 节)使其展开成一个 (区域)递归函数，而不是一个 do 表达式。
; a:
(defmacro ntimes (n &rest action)
  (let ((g (gensym)))
    `(let ((,g ,n))
       (case ,g
	 (0)
	 (1 ,@action)
	 (t ,@action
	    (ntimes (- ,g 1) ,@action))))))


;; 5.定义一个宏 n-of ，接受一个数字 n 与一个表达式，返回一个 n 个渐进值:
; > (let ((i 0) (n 4))
;     (n-of n (incf i)))
; (1 2 3 4)
; a:
(defmacro n-of (n expr)
  (let ((g (gensym))
	(h (gensym)))
    `(let* ((,g ,expr)
	    (,h (list ,g)))
       (if (= ,g ,n)
	 ,h
	 (append ,h (n-of ,n ,expr))))))


;; 6.定义一个宏，接受一变量列表以及一个代码主体，并确保变量在代码主体被求值后恢复 (revert)到原本的数值。
; a:
(defmacro retain (parms &body body)
  `((lambda ,parms ,@body) ,@parms))


;; 7.下面这个 push 的定义哪里错误？
; (defmacro push (obj lst)
;   `(setf ,lst (cons ,obj ,lst)))
; 举出一个不会与实际 push 做一样事情的函数调用例子。
; a: lst多重求值。(defun push (obj lst) (setf lst (cons obj lst)))


;; 8.定义一个将其参数翻倍的宏:
; > (let ((x 1))
;     (double x)
;     x)
; 2
; a:
(define-modify-macro double ()
		     (lambda (x) (* x 2)))
