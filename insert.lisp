;; 插入宏
;; (insert lst i e)
;; 将元素e插入到列表lst中索引为i的位置，
;; 若(>= i (length lst))则添加到末尾。
(define-modify-macro insert (i e)
		     (lambda (lst i e)
		       (let* ((len (length lst))
			      (i (min i len)))
			 (append (subseq lst 0 i)
				 (list e)
				 (subseq lst i len)))))
