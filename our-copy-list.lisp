(defun our-copy-list (lst)
  "复制列表"
  (if (atom lst)
    lst
    (cons (car lst) (cdr lst))))
