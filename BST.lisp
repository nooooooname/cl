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

(defun bst-min (root)
  "返回以root为根节点的二叉搜索树中最小元素所在的节点"
  (and root
       (or (bst-min (node-l root)) root)))

(defun bst-max (root)
  "返回以root为根节点的二叉搜索树中最大元素所在的节点"
  (and root
       (or (bst-max (node-r root)) root)))
