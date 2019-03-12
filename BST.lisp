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
