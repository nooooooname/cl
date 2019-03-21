;; 1.定义一个函数，接受一个平方数组（square array，一个相同维度的数组 (n n) )，并将它顺时针转 90 度。
; a:
(defun quarter-turn (arr)
  "思路：先水平翻转，再以主对角线对称"
  (let ((n (array-dimension arr 0)))
    (let ((rvs (make-array (list n n)))
	  (ret (make-array (list n n))))
      (do ((i 0 (+ i 1))) ((= i n))
	(do ((j 0 (+ j 1))) ((= j n))
	  (setf (aref rvs i j) (aref arr (- n 1 i) j))))
      (do ((i 0 (+ i 1))) ((= i n))
	(do ((j 0 (+ j 1))) ((= j n))
	  (setf (aref ret i j) (aref rvs j i))))
      ret)))


;; 2.阅读 368 页的 reduce 说明，然后用它来定义：
;  (a) copy-list
; a:
(defun copy-list (lst)
  (cond ((zerop (length lst)) nil)
	((= (length lst) 1) lst)
	(t (let ((flag nil))
	     (reduce #'(lambda (a b)
			 (if flag
			   (append a (list b))
			   (progn
			     (setf flag t)
			     (list a b))))
		     lst)))))
;  (b) reverse（针对列表）
; a:
(defun reverse (lst)
  (cond ((zerop (length lst)) nil)
	((= (length lst) 1) lst)
	(t (let ((flag nil))
	     (reduce #'(lambda (a b)
			 (if flag
			   (cons b a)
			   (progn
			     (setf flag t)
			     (list b a))))
		     lst)))))


;; 3.定义一个结构来表示一棵树，其中每个节点包含某些数据及三个小孩。定义：
;  (a) 一个函数来复制这样的树（复制完的节点与本来的节点是不相等（ `eql` ）的）
;  (b) 一个函数，接受一个对象与这样的树，如果对象与树中各节点的其中一个字段相等时，返回真。
; a:
(defstruct (node (:print-function	;; 定义节点
		   (lambda (n stream depth)
		     (format stream "#TRI<~A>" (node-elt n)))))
  elt (l nil) (m nil) (r nil))
(defun clone (root)			;; (a)
  (and (not (null root))
       (make-node :elt (node-elt root)
		  :l (clone (node-l root))
		  :m (clone (node-m root))
		  :r (clone (node-r root)))))
(defun tri-find (obj root ==)		;; (b)
  "==是元素比较函数"
  (and (not (null root))
       (or (funcall == obj (node-elt root))
	   (tri-find obj (node-r root) ==)
	   (tri-find obj (node-m root) ==)
	   (tri-find obj (node-l root) ==))))


;; 4.定义一个函数，接受一棵二叉搜索树，并返回由此树元素所组成的，一个由大至小排序的列表。
; a:
(defun des (root)
  (when root
    (append (des (node-r root))
	    (list (node-elt root))
	    (des (node-l root)))))


;; 5.定义 bst-adjoin 。这个函数应与 bst-insert 接受相同的参数，但应该只在对象不等于任何树中对象时将其插入。
;;   勘误: bst-adjoin 的功能与 bst-insert 一模一样。
; （略）


;; 6.任何哈希表的内容可以由关联列表（assoc-list）来描述，其中列表的元素是 (k . v) 的形式，对应到哈希表中的每一个键值对。定义一个函数：
;  (a) 接受一个关联列表，并返回一个对应的哈希表。
; a:
(defun alist-htable (alist)
  (let ((ret (make-hash-table)))
    (dolist (kv alist)
      (setf (gethash (car kv) ret) (cdr kv)))
    ret))
;  (b) 接受一个哈希表，并返回一个对应的关联列表。
; a:
(defun htable-alist (htable)
  "不检查参数是不是一个哈希表"
  (let ((ret nil))
    (maphash #'(lambda (k v)
		 (setf ret (append ret (list (cons k v)))))
	     htable)
    ret))
