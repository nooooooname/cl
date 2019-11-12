(load "while.lisp")
(defun quicksort (seq &key (start 0) (end nil) (key #'identity) (cmp #'<))
  "用快速排序算法对序列seq排序，待排序的索引区间为 [start, end)，
  若end为nil则将其视为序列长度，key是应用到每个元素的函数，
  排序规则cmp是一个接受2个参数的函数，若(funcall cmp a b)返回t，
  则在排序后的序列中a的位置比b靠前。"
  (if (null end)
    (setf end (length seq)))
  (let ((i start) (j (- end 1)) (fd nil) (p (funcall key (elt seq start))))
    (while (< i j)
	   (if (funcall cmp (funcall key (elt seq j)) (funcall key (elt seq i)))
	     (progn
	       (rotatef (elt seq j) (elt seq i))
	       (if fd (decf j) (incf i))
	       (setf fd (not fd)))
	     (if fd
	       (incf i)
	       (decf j))))
    (if (> (- i start) 1)
      (quicksort seq :start start :end i :key key :cmp cmp))
    (if (> (- end (incf i)) 1)
      (quicksort seq :start i :end end :key key :cmp cmp)))
  seq)


;; 测试用例生成器
(defun cons-testcase (seqtype &key (len 10) (maxval 100) (minval -100) (key #'identity))
  (let ((w (+ (- maxval minval) 1)) (lst nil))
    (while (>= (decf len) 0)
	   (let ((r (+ (random w) minval)))
	     (setf lst (append lst (list (funcall key r))))))
    (case seqtype
      (:list lst)
      (:vector (apply #'vector lst)))))
