(defun our-copy-tree (tree)
  "接受一个树，并返回一份副本"
  (if (consp tree)
    (cons (our-copy-tree (car tree))
	  (our-copy-tree (cdr tree)))
    tree))
