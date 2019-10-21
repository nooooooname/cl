;;; 实用数学函数
(defun sq (x) (* x x))	;; x^2

(defun mag (x y z)
  "返回以x, y, z为长宽高的立方体的对角线长度"
  (sqrt (+ (sq x) (sq y) (sq z))))

(defun unit-vector (x y z)
  "计算向量<x, y, z>的单位向量，
  三个返回值分别是单位向量的x, y, z。
  若<x, y, z>是0向量则返回三个0。"
  (let ((d (mag x y z)))
    (if (not (zerop d))
      (values (/ x d) (/ y d) (/ z d))
      (values 0 0 0))))

(defstruct (point (:conc-name nil))
  x y z)

(defun distance (p1 p2)
  "计算p1和p2两点之间的距离"
  (mag (- (x p1) (x p2))
       (- (y p1) (y p2))
       (- (z p1) (z p2))))

(defun root (a b c)
  "计算方程 ax^2+bx+c=0 的实数根，若没有实数根则返回nil，
  若有2个实数根则返回2个值，第一个是较小的根，第二个是较大的根。"
  (if (zerop a)
    (if (not (zerop b))
      (/ (- c) b))
    (let ((delta (- (sq b) (* 4 a c))))
      (if (plusp delta)
	(values (/ (- (- b) (sqrt delta)) (* 2 a))
		(/ (+ (- b) (sqrt delta)) (* 2 a)))
	(if (zerop delta)
	  (/ (- b) (* 2 a)))))))

;;; 光线追踪算法
(defparameter *world* nil)	;; 模拟世界(几何体列表)
(defconstant eye (make-point :x 0 :y 0 :z 100))	;; 观测点与光源

(defun tracer (pgm-file &optional (res 1))
  "以光线追踪算法绘制图像，分辨率是res*100×res*100，
  图像平面是在xOy平面上的正方形{ (x,y) | |x|,|y|<50 }"
  (with-open-file (p pgm-file :direction :output :if-exists :supersede)
    (format p "P2 ~A ~A 255" (* res 100) (* res 100))
    (let ((inc (/ res)))
      (do ((y -50 (+ y inc)))
	((< (- 50 y) inc))
	(do ((x -50 (+ x inc)))
	  ((< (- 50 x) inc))
	  (print (color-at x y) p))))))

(defun color-at (x y)
  "返回过点(x, y, 0)射向观测点的光线颜色"
  (multiple-value-bind (xr yr zr)
    (unit-vector (- x (x eye))
		 (- y (y eye))
		 (- (z eye)))
    (round (* (sendray eye xr yr zr) 255))))

(defun sendray (pt xr yr zr)
  "找到从点pt发出的光线<xr, yr, zr>第一个照射到的几何体的照射点，
  并返回反射光线的亮度，若没有照射到任何物体则返回0。"
  (multiple-value-bind (g h) (first-hit pt xr yr zr)
    (if g
      (* (lambert g h xr yr zr) (surface-color g))
      0)))

(defun first-hit (pt xr yr zr)
  "返回过点pt的光线<xr, yr, zr>照射到的第一个几何体与照射点，
  若没有照射到任何物体则返回2个nil。不考虑观测点在几何体内部的情况。"
  (let (geometry hit-point dist)
    (dolist (g *world*)
      (let ((h (intersect g pt xr yr zr)))
	(when h
	  (let ((d (distance h pt)))
	    (when (or (null dist) (< d dist))
	      (setf geometry g hit-point h dist d))))))
    (values geometry hit-point)))

(defun lambert (g pt xr yr zr)
  "根据朗伯定律计算光线<xr, yr, zr>照射到几何体g上的pt点的反射光强度。"
  (multiple-value-bind (xn yn zn) (normal g pt)
    (max 0 (+ (* xr xn) (* yr yn) (* zr zn)))))

;;; 几何体定义与相关函数
(defstruct surface color)	;; 几何体表面，为了简单起见表面只有一种颜色
(defstruct (sphere (:include surface))	;; 球体（:include类似于面向对象中的继承）
  radius center)

(defun defsphere (x y z r c)
  "在模拟世界中添加一个以(x, y, z)为球心，r为半径，表面颜色为c的球体。
  返回添加的球体。"
  (let ((s (make-sphere
	     :radius r
	     :center (make-point :x x :y y :z z)
	     :color c)))
    (push s *world*)
    s))

(defun intersect (g pt xr yr zr)
  "判断从点pt射出的光线<xr, yr, zr>能否照射到几何体g，
  若能照射到则返回照射点，若不能照射到则返回nil。
  不考虑点pt在几何体内部的情况。"
  (funcall (typecase g (sphere #'sphere-intersect))
	   g pt xr yr zr))

(defun sphere-intersect (s pt xr yr zr)
  "判断从点pt射出的光线<xr, yr, zr>能否照射到球体s，
  若能照射到则返回照射点，若不能照射到则返回nil。
  不考虑点pt在球体内部的情况。"
  (let* ((c (sphere-center s))
	 (n (root (+ (sq xr) (sq yr) (sq zr))
		  (* 2 (+ (* (- (x pt) (x c)) xr)
			  (* (- (y pt) (y c)) yr)
			  (* (- (z pt) (z c)) zr)))
		  (- (+ (sq (- (x pt) (x c)))
			(sq (- (y pt) (y c)))
			(sq (- (z pt) (z c))))
		     (sq (sphere-radius s))))))
    (if (and n (plusp n))
      (make-point :x (+ (x pt) (* n xr))
		  :y (+ (y pt) (* n yr))
		  :z (+ (z pt) (* n zr))))))

(defun normal (g pt)
  "计算几何体g上的点pt指向内部的单位法向量。"
  (funcall (typecase g (sphere #'sphere-normal))
	   g pt))

(defun sphere-normal (s pt)
  "计算球体s上的点pt指向内部的单位法向量。"
  (let ((c (sphere-center s)))
    (unit-vector (- (x c) (x pt))
		 (- (y c) (y pt))
		 (- (z c) (z pt)))))
