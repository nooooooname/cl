(defstruct (node (:print-function	;; 二叉搜索树节点
		   (lambda (n stream depth)
		     (format stream "#BST<~A>" (node-elt n)))))
  elt (l nil) (r nil))

(defun bst-insert (obj root < ==)
  "把元素obj插入到以root为根节点的二叉搜索树中，若root是nil则创建一个节点，
  函数总是返回根节点。后2个参数 < 和 == 用来判断两个元素的大小关系，
  若(funcall < e1 e2)和(funcall == e1 e2)返回T则分别代表e1小于e2和e1等于e2"
  (if (null root)
    (make-node :elt obj)
    (let ((elt (node-elt root)))
      (if (not (funcall == obj elt))
	(if (funcall < obj elt)
	  (setf (node-l root)
		(bst-insert obj (node-l root) < ==))
	  (setf (node-r root)
		(bst-insert obj (node-r root) < ==))))
      root)))

(defun bst-find (obj root < ==)
  "在以root为根节点的二叉搜索树中查找元素obj
  返回元素所在的节点，找不到则返回nil。后2个参数含义同上"
  (and (not (null root))
       (let ((elt (node-elt root)))
	 (if (funcall == obj elt)
	   root
	   (if (funcall < obj elt)
	     (bst-find obj (node-l root) < ==)
	     (bst-find obj (node-r root) < ==))))))

(defun bst-remove (obj root < ==)
  "从以root为根节点的二叉搜索树中删除元素obj
  返回新的根节点，若obj不在树中则无改动。后2个参数含义同上"
  (and (not (null root))
       (let ((elt (node-elt root)))
	 (if (funcall == obj elt)
	   (percolate root)
	   (progn
	     (if (funcall < obj elt)
	       (setf (node-l root)
		     (bst-remove obj (node-l root) < ==))
	       (setf (node-r root)
		     (bst-remove obj (node-r root) < ==)))
	     root)))))

(defun percolate (root)
  "删除二叉搜索树的根节点root，返回新的根节点"
  (let ((l (node-l root)) (r (node-r root)))
    (cond ((null l) r)
	  ((null r) l)
	  (t (if (zerop (random 2))
	       (progn (setf (node-elt root) (node-elt (bst-max l)))
		      (setf (node-l root) (bst-remove-max l)))
	       (progn (setf (node-elt root) (node-elt (bst-min r)))
		      (setf (node-r root) (bst-remove-min r))))
	     root))))

(defun bst-remove-max (root)
  "删除以root为根节点的二叉搜索树的最大节点
  返回此树新的根节点，root不能是nil"
  (if (null (node-r root))
    (node-l root)
    (progn (setf (node-r root)
		 (bst-remove-max (node-r root)))
	   root)))

(defun bst-remove-min (root)
  "删除以root为根节点的二叉搜索树的最小节点
  返回此树新的根节点，root不能是nil"
  (if (null (node-l root))
    (node-r root)
    (progn (setf (node-l root)
		 (bst-remove-min (node-l root)))
	   root)))

(defun bst-min (root)
  "返回以root为根节点的二叉搜索树中最小元素所在的节点"
  (and root
       (or (bst-min (node-l root)) root)))

(defun bst-max (root)
  "返回以root为根节点的二叉搜索树中最大元素所在的节点"
  (and root
       (or (bst-max (node-r root)) root)))

(defun bst-traverse (fn root order)
  "用函数fn遍历以root为根节点的二叉搜索树，返回nil
  遍历顺序由order指定，正数先根、0中根、负数后根"
  (and (numberp order)
       (when root
	 (cond ((> order 0)
		(funcall fn (node-elt root))
		(bst-traverse fn (node-l root) order)
		(bst-traverse fn (node-r root) order))
	       ((< order 0)
		(bst-traverse fn (node-l root) order)
		(bst-traverse fn (node-r root) order)
		(funcall fn (node-elt root)))
	       (t
		(bst-traverse fn (node-l root) order)
		(funcall fn (node-elt root))
		(bst-traverse fn (node-r root) order))))
       nil))


;; cond
;;	疑似condition的简写
;; 语法：
;;	(cond ((条件1) (动作1-1) (动作1-2) ... (动作1-n1))
;;	      ((条件2) (动作2-1) (动作2-2) ... (动作2-n2))
;;		.
;;		.
;;		.
;;	      ((条件m) (动作m-1) (动作m-2) ... (动作m-nm)))
;; 功能：
;;	依次判断条件1到条件m的真假，若直到条件i才是真，则：
;;	依次执行动作i-1到动作i-ni，然后返回动作i-ni的返回值，ni=0时返回T。
;;	若所有条件都是假(nil)或m=0，则所有动作都不执行，cond返回nil。

;; when
;; 语法：
;;	(when (条件)
;;		(动作1)
;;		(动作2)
;;		  .
;;		  .
;;		  .
;;		(动作n))
;; 功能：
;;	若条件为假则返回nil。
;;	否则依次执行动作1到动作n，
;;	并返回动作n的返回值，
;;	若没有动作则返回nil。
