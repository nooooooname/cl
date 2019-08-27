(defstruct buf
  "环形队列"
  vec (start 0) (end 0) (size 0))

(defun new-buf (len)
  "创建一个环形队列，参数是队列长度"
  (if (> len 0)
    (make-buf :vec (make-array len)
	      :size len)))

(defun buf-isfull (b)
  "返回队列是(t)否(nil)已满"
  (= (mod (+ (buf-end b) 1)
	  (buf-size b))
     (buf-start b)))

(defun buf-cnt (b)
  "返回队列中的元素数量"
  (mod (- (buf-end b) (buf-start b)) (buf-size b)))

(defun buf-push (x b)
  "入队一个元素x，若成功则返回t，若队列已满返回nil"
  (let ((next (mod (+ (buf-end b) 1)
		   (buf-size b))))
    (if (not (= next (buf-start b)))
      (progn
	(setf (svref (buf-vec b) (buf-end b)) x
	      (buf-end b) next)
	t))))

(defun buf-front (buf)
  "查询队首元素但不出队，返回队首元素和查询结果，
  若队列不空则查询结果为t，第一个返回值有效，
  若队列为空则查询结果为nil，第一个返回值无效"
  (if (> (buf-cnt buf) 0)
    (values (svref (buf-vec buf) (buf-start buf)) t)
    (values nil nil)))

(defun buf-pop-front (buf)
  "出队队首元素，返回值同 buf-front "
  (multiple-value-bind (val res) (buf-front buf)
    (if res
      (setf (buf-start buf)
	    (mod (+ (buf-start buf) 1)
		 (buf-size buf))))
    (values val res)))

(defun bref (buf n)
  "查询队列buf中索引值为n的元素，返回元素和查询结果，
  若 0 <= n < (buf-cnt buf)，
  则查询结果为t，第一个返回值有效，
  否则查询结果为nil，第一个返回值无效"
  (if (and (>= n 0)
	   (< n (buf-cnt buf)))
    (let ((vec-index (mod (+ (buf-start buf) n)
			  (buf-size buf))))
      (values (svref (buf-vec buf) vec-index) t))
    (values nil nil)))

(defun buf-clear (b)
  "清空队列b"
  (setf (buf-start b) 0
	(buf-end b) 0))

(defun buf-flush (b strm)
  "将缓冲区b中的内容全部输出到流strm中，
  返回输出的元素个数"
  (let ((cnt (buf-cnt b)))
    (dotimes (i cnt cnt)
      (princ (buf-pop-front b) strm))))
