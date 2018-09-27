(defun compress (x)
  "用游程编码算法压缩平坦列表x，若x是原子则返回x"
  (if (null x)
    nil
    (if (listp x)
      (compr (car x) 1 (cdr x))
      x)))

(defun compr (elemt n lst)
  "把一个列表用游程编码算法压缩并返回压缩后的列表
  原列表的前n项都是elemt，剩余项组成列表lst"
  (if (null lst)
    (list (n-elts elemt n))
    (let ((next (car lst)))
      (if (eql elemt next)
	(compr elemt (+ n 1) (cdr lst))
	(cons (n-elts elemt n)
	      (compr next 1 (cdr lst)))))))

(defun n-elts (elemt n)
  (if (> n 1)
    (list n elemt)
    elemt))


(defun uncompress (lst)
  "解压用游程编码算法压缩的列表lst，不检查参数，若lst不是列表则返回lst"
  (if (or (null lst) (atom lst))
    lst
    (let ((elemt (car lst))
	  (rst (uncompress (cdr lst))))
      (if (consp elemt)
	(append (apply #'list-of elemt) rst)
	(cons elemt rst)))))

(defun list-of (n elemt)
  "把被压缩的子项展开成一个列表，不检查参数n的合法性"
  (if (zerop n)
    nil
    (cons elemt (list-of (- n 1) elemt))))
