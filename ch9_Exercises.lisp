;; 1.定义一个函数，接受一个实数列表，若且唯若 (iff)它们是非递减 (nondecreasing)顺序时返回真。
; a:
(defun jnb (lst)
  (or (null lst)
      (and (or (null (cdr lst))
	       (>= (cadr lst) (car lst)))
	   (jnb (cdr lst)))))


;; 2.定义一个函数，接受一个整数 cents 并返回四个值，将数字用 25- , 10- , 5- , 1- 来显示，使用最少数量的硬币。(译注: 25- 是 25 美分，以此类推)
; a:
(defun change (cents)
  (let* ((25- (floor (/ cents 25)))
	 (remainder (rem cents 25))
	 (10- (floor (/ remainder 10)))
	 (remainder (rem remainder 10))
	 (5- (floor (/ remainder 5)))
	 (remainder (rem remainder 5)))
    (values (format nil "25-~A" 25-)
	    (format nil "10-~A" 10-)
	    (format nil "5-~A" 5-)
	    (format nil "1-~A" remainder))))


;; 3.一个遥远的星球住着两种生物， wigglies 与 wobblies 。 Wigglies 与 wobblies 唱歌一样厉害。每年都有一个比赛来选出十大最佳歌手。下面是过去十年的结果:
;  YEAR		1	2	3	4	5	6	7	8	9	10
;  WIGGLIES	6	5	6	4	5	5	4	5	6	5
;  WOBBLIES	4	5	4	6	5	5	6	5	4	5
;  写一个程序来模拟这样的比赛。你的结果实际上有建议委员会每年选出 10 个最佳歌手吗？
; 略


;; 4.定义一个函数，接受 8 个表示二维空间中两个线段端点的实数，若线段没有相交，则返回假，或返回两个值表示相交点的 x 座标与 y 座标。
; a:
(defun line-cross (A1x A1y B1x B1y A2x A2y B2x B2y)
  "判断线段A1B1和A2B2是否相交，若不相交返回nil，
  若相交则返回2个值，分别是交点的x坐标和y坐标。
  若两条线段斜率相同且有无穷多个交点则在不垂直于x轴的情况下返回x坐标最小的交点，
  在垂直于x轴的情况下返回y坐标最小的交点。"
  (labels ((vec-cross (v1 v2)		;; 计算向量v1和v2的叉积
		      (- (* (svref v1 0) (svref v2 1))
			 (* (svref v2 0) (svref v1 1))))
	   (seg-cross (a1 b1 a2 b2)	;; 判断区间[a1,b1]和[a2,b2]是否有重合部分(自动检测a,b顺序)
		      (not (or (< (max a1 b1) (min a2 b2))
			       (> (min a1 b1) (max a2 b2)))))
	   (linear-equation (x1 y1 x2 y2)
			    ;; 求过(x1,y1)和(x2,y2)两点的直线方程，
			    ;; 对于x=x0的直线返回:x和x0，对于y=y0的直线返回:y和y0，
			    ;; 对于y=ax+b的直线返回a和b，若两点重合则返回nil。
			    (and (not (and (= x1 x2) (= y1 y2)))
				 (if (= x1 x2)
				   (values :x x1)
				   (if (= y1 y2)
				     (values :y y1)
				     (let* ((a (/ (- y1 y2) (- x1 x2)))
					    (b (- y1 (* a x1))))
				       (values a b)))))))
    (if (and (seg-cross A1x B1x A2x B2x)
	     (seg-cross A1y B1y A2y B2y))
      (let* ((B2A1 (vector (- A1x B2x) (- A1y B2y)))
	     (B2A2 (vector (- A2x B2x) (- A2y B2y)))
	     (B2B1 (vector (- B1x B2x) (- B1y B2y)))
	     (p (* (vec-cross B2A1 B2A2) (vec-cross B2B1 B2A2)))
	     (A1A2 (vector (- A2x A1x) (- A2y A1y)))
	     (A1B1 (vector (- B1x A1x) (- B1y A1y)))
	     (A1B2 (vector (- B2x A1x) (- B2y A1y)))
	     (q (* (vec-cross A1A2 A1B1) (vec-cross A1B2 A1B1))))
	(multiple-value-bind (a1 b1) (linear-equation A1x A1y B1x B1y)
	  (multiple-value-bind (a2 b2) (linear-equation A2x A2y B2x B2y)
	    (if (eql a1 a2)
	      (let* ((points (list (cons A1x A1y) (cons B1x B1y) (cons A2x A2y) (cons B2x B2y)))
		     (point (second (sort points (lambda (a b)
						   (if (eql a1 :y)
						     (< (cdr a) (cdr b))
						     (< (car a) (car b))))))))
		(values (car point) (cdr point)))
	      (if (and (<= p 0) (<= q 0))
		(case a1
		  (:x (if (realp a2)
			(values b1 (+ (* a2 b1) b2))
			(values b1 b2)))
		  (:y (if (realp a2)
			(values (/ (- b1 b2) a2) b1)
			(values b2 b1)))
		  (t (case a2
		       (:x (values b2 (+ (* a1 b2) b1)))
		       (:y (values (/ (- b2 b1) a1) b2))
		       (t (let* ((x (/ (- b2 b1) (- a1 a2)))
				 (y (+ (* a1 x) b1)))
			    (values x y))))))))))))))


;; 5.假设 f 是一个接受一个 (实数) 参数的函数，而 min 与 max 是有着不同正负号的非零实数，使得 f 对于参数 i 有一个根 (返回零)并满足 min < i < max 。定义一个函数，接受四个参数， f , min , max 以及 epsilon ，并返回一个 i 的近似值，准确至正负 epsilon 之内。
; 略


;; 6.Honer’s method 是一个有效率求出多项式的技巧。要找到 ax3+bx2+cx+d 你对 x(x(ax+b)+c)+d 求值。定义一个函数，接受一个或多个参数 ── x 的值伴随着 n 个实数，用来表示 (n-1) 次方的多项式的系数 ── 并用 Honer’s method 计算出多项式的值。
; a:
(defun polynome (x &rest args)
  (let ((iter 0))
    (dolist (a args iter)
      (setf iter (+ (* iter x) a)))))


;; 7.你的 Common Lisp 实现使用了几个位元来表示定长数？
; a: 64位


;; 8.你的 Common Lisp 实现提供几种不同的浮点数？
; a: 4种
