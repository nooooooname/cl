(defun combiner (x)
  "根据参数x的类型返回适用它的“结合”函数"
  (typecase x
    (number #'+)
    (list #'append)
    (t #'list)))
(defun combine (&rest args)
  "把所有的参数“结合”起来，简单起见，假定所有的参数都是同一类型"
  (apply (combiner (car args))
	 args))
