(defun our-subst (new old tree)
  "把tree中所有的old替换成new"
  (if (equal old tree)
    new
    (if (atom tree)
      tree
      (cons (our-subst new old (car tree))
	    (our-subst new old (cdr tree))))))
