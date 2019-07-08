;; 1.将下列表达式翻译成没有使用 let 与 let* ，并使同样的表达式不被求值 2 次。
;  (a) (let ((x (car y)))
;	 (cons x x))
; a:
((lambda (x) (cons x x)) (car y))
;  (b) (let* ((w (car x))
;	      (y (+ w z)))
;	 (cons w y))
; a:
((lambda (w)
   ((lambda (y) (cons w y)) (+ w z)))
 (car x))


;; 2.使用 cond 重写 29 页的 mystery 函数。(译注: 第二章的练习第 5 题的 (b) 部分)
; a:
(defun mystery (x y)
  (cond
    ((null y) nil)
    ((eql (car y) x) 0)
    (t (let ((z (mystery x (cdr y))))
	 (and z (+ z 1))))))


;; 3.定义一个返回其实参平方的函数，而当实参是一个正整数且小于等于 5 时，不要计算其平方。
; a:
(defun sq (x)
  (cond
    ((not (numberp x)) nil)
    ((and (integerp x) (> x 0) (<= x 5)) nil)
    (t (* x x))))


;; 4.使用 case 与 svref 重写 month-num (图 5.1)。
; a:
(defun month-num (m y)
  (case m
    (1 0)
    (t (+ (svref month (- m 1)) (if (leap? y) 1 0)))))


;; 5.定义一个迭代与递归版本的函数，接受一个对象 x 与向量 v ，并返回一个列表，包含了向量 v 当中，所有直接在 x 之前的对象：
; > (precedes #\a "abracadabra")
;   (#\c #\d #\r)
; a:
(defun precedes (x v)
  "迭代生成一个列表，包含了向量 v 当中，所有和x左相邻的对象"
  (do* ((i 0 (+ i 1))
	(pre nil (elt v (- i 1)))
	(obj (elt v i) (elt v i))
	(res nil))
    ((= (+ i 1) (length v)) res)
    (if (eql x obj)
      (if (not (null pre))
	(setf res (append res (list pre)))))))
(defun precedes (x v)
  "递归生成一个列表，包含了向量 v 当中，所有和x左相邻的对象"
  (let ((pos (position x v)))
    (and (not (null pos))
	 (if (zerop pos)
	   (precedes x (subseq v 1))
	   (remove-duplicates
	     (append (list (elt v (- pos 1)))
		     (precedes x (subseq v (+ pos 1)))))))))


;; 6.定义一个迭代与递归版本的函数，接受一个对象与列表，并返回一个新的列表，在原本列表的对象之间加上传入的对象：
; > (intersperse '- '(a b c d))
;   (A - B - C - D)
; a:
(defun intersperse (obj lst)
  "迭代版本的intersperse函数，接受一个对象与列表，
  并返回一个新的列表，在原本列表的对象之间加上传入的对象"
  (and (not (null lst))
       (listp lst)
       (do* ((i 0 (+ i 1))
	     (e (elt lst i) (elt lst i))
	     (res (list e) (append res (list obj) (list e))))
	 ((= (+ i 1) (length lst)) res))))
(defun intersperse (obj lst)
  "递归版本的intersperse函数，接受一个对象与列表，
  并返回一个新的列表，在原本列表的对象之间加上传入的对象"
  (and (not (null lst))
       (listp lst)
       (let ((a (car lst))
	     (d (intersperse obj (cdr lst))))
	 (if (null d)
	   (list a)
	   (append (list a) (list obj) d)))))


;; 7.定义一个接受一系列数字的函数，当且仅当每一对（pair）数字的差为一时，返回真，使用
;  (a) 递归
; a:
(defun pairdiff1 (&rest nums)
  "接收偶数个数字，当且仅当每一对数字相差1时返回真。不检查参数"
  (if (null nums)
    t
    (let ((n1 (car nums))
	  (n2 (cadr nums))
	  (rst (cddr nums)))
      (if (or (= (- n1 n2) 1)
	      (= (- n2 n1) 1))
	(eval (append '(pairdiff1) rst))))))
;  (b) do
; a:
(defun pairdiff1 (&rest nums)
  "接收偶数个数字，当且仅当每一对数字相差1时返回真。不检查参数"
  (do ((rst nums (cddr rst)))
    ((null rst) t)
    (let ((n1 (car rst))
	  (n2 (cadr rst)))
      (if (not (or (= (- n1 n2) 1)
		   (= (- n2 n1) 1)))
	(return nil)))))
;  (c) mapc 与 return
; a:
(defun pairdiff1 (&rest nums)
  "接收偶数个数字，当且仅当每一对数字相差1时返回真。不检查参数"
  (if (null nums)
    t
    (let ((lst1 nil) (lst2 nil))
      (dotimes (n (/ (length nums) 2))
	(setf lst1
	      (append lst1 (list (nth (* n 2) nums))))
	(setf lst2
	      (append lst2 (list (nth (+ (* n 2) 1) nums)))))
      (block nil
	     (mapc #'(lambda (a b)
		       (unless (= (abs (- a b)) 1)
			 (return nil)))
		   lst1 lst2)
	     t))))


;; 8.定义一个单递归函数，返回两个值，分别是向量的最大与最小值。
; a:
(defun maxmin (vec)
  "传入一个有且仅有数字元素的向量，
  返回两个值，分别是向量的最大值与最小值。不检查参数"
  (let ((fst (svref vec 0)))
    (if (= (length vec) 1)
      (values fst fst)
      (multiple-value-bind (maxval minval) (maxmin (subseq vec 1))
	(if (> fst maxval)
	  (return-from maxmin (values fst minval)))
	(if (< fst minval)
	  (return-from maxmin (values maxval fst)))
	(values maxval minval)))))


;; 9.第三章最短路径的程序在找到一个完整的路径时，仍持续遍历队列。在搜索范围比较大时，这可能会产生问题。
;  (a) 使用 catch 与 throw 来变更程序，使其找到第一个完整路径时，直接返回它。
; a:
(defun shortest-path (start end net)
  "返回在有向无环图net中以start为起点，以end为终点的最短路径(之一)。没有路径则返回nil。
  图用邻接表的形式表示：
  ((节点1 邻接节点1 邻接节点2 ... 邻接节点n)
   (节点2 邻接节点1 邻接节点2 ... 邻接节点n)
   ...
   (节点n 邻接节点1 邻接节点2 ... 邻接节点n))"
  (bfs end (list (list start)) net))
(defun bfs (end queue net)
  "用广度优先搜索以queue为待搜索队列在图net上搜索到节点end的最短路径。不存在则返回nil。
  队列中的每一项是一个倒序的已搜索路径"
  (if (null queue)
    nil
    (let ((path (car queue)))
      (let ((node (car path)))
	(if (eql node end)
	  (reverse path)
	  (catch 'found
		 (bfs end
		      (append (cdr queue)
			      (new-paths path node net end))
		      net)))))))
(defun new-paths (path node net end)
  (mapcar #'(lambda (n)
	      (let ((new-path (cons n path)))
		(if (eql n end)
		  (throw 'found (reverse new-path))
		  new-path)))
	  (cdr (assoc node net))))
;  (b) 重写一个做到同样事情的程序，但不使用 catch 与 throw。
; a:
(defun shortest-path (start end net)
  "返回在有向无环图net中以start为起点，以end为终点的最短路径(之一)。没有路径则返回nil。
  图用邻接表的形式表示：
  ((节点1 邻接节点1 邻接节点2 ... 邻接节点n)
   (节点2 邻接节点1 邻接节点2 ... 邻接节点n)
   ...
   (节点n 邻接节点1 邻接节点2 ... 邻接节点n))"
  (bfs end (list (list start)) net))
(defun bfs (end queue net)
  "用广度优先搜索以queue为待搜索队列在图net上搜索到节点end的最短路径。不存在则返回nil。
  队列中的每一项是一个倒序的已搜索路径"
  (and (not (null queue))
       (let* ((path (car queue))
	      (node (car path)))
	 (if (eql node end)
	   (reverse path)
	   (bfs end
		(append (cdr queue)
			(new-paths path node net end))
		net)))))
(defun new-paths (path node net end)
  (mapcar #'(lambda (n)
	      (let ((new-path (cons n path)))
		(if (eql n end)
		  (return-from bfs new-path)
		  new-path)))
	  (cdr (assoc node net))))
