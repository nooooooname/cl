(defun our-member (obj lst)
  "递归检测元素obj是否在列表lst中"
  (if (null lst)
    nil
    (if (eql (car lst) obj)
      t
      (our-member obj (cdr lst)))))
