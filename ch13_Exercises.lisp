;; 1.检验你的编译器是否支持 (observe)内敛声明。
; [勘误] 检验你的编译器是否支持内联声明。
; a: 支持


;; 2.将下述函数重写为尾递归形式。它被编译后能快多少？
; (defun foo (x)
;   (if (zerop x)
;       0
;       (1+ (foo (1- x)))))
; 注意：你需要增加额外的参数。
; a:
(defun foo (x &optional (r 0))
  (if (zerop x)
    r
    (foo (1- x) (1+ r))))
; 尾递归比原函数快了54%


;; 3.为下述程序增加声明。你能让它们变快多少？
;  (a) 在 5.7 节中的日期运算代码。
;  (b) 在 9.8 节中的光线跟踪器 (ray-tracer)。
; a:
;  (a)
(declaim (type (vector fixnum 13) month)
	 (type fixnum yzero))
(defconstant month
	     #(0 31 59 90 120 151 181 212 243 273 304 334 365))
(defconstant yzero 2000)
(declaim (inline leap?))
(defun leap? (y)
  (declare (fixnum y))
  (and (zerop (mod y 4))
       (or (zerop (mod y 400))
	   (not (zerop (mod y 100))))))
(defun date->num (d m y)
  (declare (fixnum d m y))
  (+ (- d 1) (month-num m y) (year-num y)))
(defun month-num (m y)
  (declare (fixnum m y))
  (+ (svref month (- m 1))
     (if (and (> m 2) (leap? y)) 1 0)))
(defun year-num (y)
  (declare (fixnum y))
  (let ((d 0))
    (declare (fixnum d))
    (if (>= y yzero)
      (dotimes (i (- y yzero) d)
	(declare (optimize (speed 3)))
	(incf d (year-days (+ yzero i))))
      (dotimes (i (- yzero y) (- d))
	(declare (optimize (speed 3)))
	(incf d (year-days (+ y i)))))))
(declaim (inline year-days))
(defun year-days (y)
  (declare (fixnum y))
  (if (leap? y) 366 365))
(defun num->date (n)
  (declare (fixnum n))
  (multiple-value-bind (y left) (num-year n)
    (multiple-value-bind (m d) (num-month left y)
      (values d m y))))
(defun num-year (n)
  (declare (fixnum n))
  (if (< n 0)
    (do* ((y (- yzero 1) (- y 1))
	  (d (- (year-days y)) (- d (year-days y))))
      ((<= d n) (values y (- n d))))
    (do* ((y yzero (+ y 1))
	  (prev 0 d)
	  (d (year-days y) (+ d (year-days y))))
      ((> d n) (values y (- n prev))))))
(defun num-month (n y)
  (declare (fixnum n y))
  (if (leap? y)
    (cond ((= n 59) (values 2 29))
	  ((> n 59) (nmon (- n 1)))
	  (t (nmon n)))
    (nmon n)))
(declaim (inline nmon))
(defun nmon (n)
  (declare (fixnum n))
  (let ((m (position n month :test #'<)))
    (declare (fixnum m))
    (values m (+ 1 (- n (svref month (- m 1)))))))
(defun date+ (d m y n)
  (declare (fixnum d m y n))
  (num->date (+ (date->num d m y) n)))
; 添加声明后也没快多少。。。
;  (b)
(declaim (inline sq))
(defun sq (x) (* x x))
(declaim (inline mag))
(defun mag (x y z)
  (sqrt (+ (sq x) (sq y) (sq z))))
(declaim (inline unit-vector))
(defun unit-vector (x y z)
  (let ((d (mag x y z)))
    (values (/ x d) (/ y d) (/ z d))))
(defstruct (point (:conc-name nil))
  x y z)
(declaim (inline distance))
(defun distance (p1 p2)
  (mag (- (x p1) (x p2))
       (- (y p1) (y p2))
       (- (z p1) (z p2))))
(declaim (inline minroot))
(defun minroot (a b c)
  (if (zerop a)
    (/ (- c) b)
    (let ((disc (- (sq b) (* 4 a c))))
      (unless (minusp disc)
	(let ((discrt (sqrt disc)))
	  (min (/ (+ (- b) discrt) (* 2 a))
	       (/ (- (- b) discrt) (* 2 a))))))))
(defstruct surface color)
(declaim (list *world*))
(defparameter *world* nil)
(declaim (point eye))
(defconstant eye (make-point :x 0 :y 0 :z 200))
(defun tracer (pathname &optional (res 1))
  (with-open-file (p pathname :direction :output)
    (format p "P2 ~A ~A 255" (* res 100) (* res 100))
    (let ((inc (/ res)))
      (do ((y -50 (+ y inc)))
	((< (- 50 y) inc))
	(do ((x -50 (+ x inc)))
	  ((< (- 50 x) inc))
	  (print (color-at x y) p))))))
(defun color-at (x y)
  (multiple-value-bind (xr yr zr)
    (unit-vector (- x (x eye))
		 (- y (y eye))
		 (- 0 (z eye)))
    (round (* (sendray eye xr yr zr) 255))))
(defun sendray (pt xr yr zr)
  (multiple-value-bind (s int) (first-hit pt xr yr zr)
    (if s
      (* (lambert s int xr yr zr) (surface-color s))
      0)))
(defun first-hit (pt xr yr zr)
  (let (surface hit dist)
    (dolist (s *world*)
      (declare (optimize (speed 3)))
      (let ((h (intersect s pt xr yr zr)))
	(when h
	  (let ((d (distance h pt)))
	    (when (or (null dist) (< d dist))
	      (setf surface s hit h dist d))))))
    (values surface hit)))
(declaim (inline lambert))
(defun lambert (s int xr yr zr)
  (multiple-value-bind (xn yn zn) (normal s int)
    (max 0 (+ (* xr xn) (* yr yn) (* zr zn)))))
(defstruct (sphere (:include surface))
  radius center)
(declaim (inline defsphere))
(defun defsphere (x y z r c)
  (let ((s (make-sphere
	     :radius r
	     :center (make-point :x x :y y :z z)
	     :color  c)))
    (push s *world*)
    s))
(defun intersect (s pt xr yr zr)
  (funcall (typecase s (sphere #'sphere-intersect))
	   s pt xr yr zr))
(defun sphere-intersect (s pt xr yr zr)
  (let* ((c (sphere-center s))
	 (n (minroot (+ (sq xr) (sq yr) (sq zr))
		     (* 2 (+ (* (- (x pt) (x c)) xr)
			     (* (- (y pt) (y c)) yr)
			     (* (- (z pt) (z c)) zr)))
		     (+ (sq (- (x pt) (x c)))
			(sq (- (y pt) (y c)))
			(sq (- (z pt) (z c)))
			(- (sq (sphere-radius s)))))))
    (if n
      (make-point :x  (+ (x pt) (* n xr))
		  :y  (+ (y pt) (* n yr))
		  :z  (+ (z pt) (* n zr))))))
(defun normal (s pt)
  (funcall (typecase s (sphere #'sphere-normal))
	   s pt))
(declaim (inline sphere-normal))
(defun sphere-normal (s pt)
  (let ((c (sphere-center s)))
    (unit-vector (- (x c) (x pt))
		 (- (y c) (y pt))
		 (- (z c) (z pt)))))
(defun ray-test (&optional (res 1))
  (setf *world* nil)
  (defsphere 0 -300 -1200 200 .8)
  (defsphere -80 -150 -1200 200 .7)
  (defsphere 70 -100 -1200 200 .9)
  (do ((x -2 (1+ x)))
    ((> x 2))
    (do ((z 2 (1+ z)))
      ((> z 7))
      (defsphere (* x 200) 300 (* z -400) 40 .75)))
  (tracer (make-pathname :name "spheres.pgm") res))
; 添加声明后也没快多少。。。


;; 4.重写 3.15 节中的广度优先搜索的代码让它尽可能减少使用构造。
; a:
(defun shortest-path (start end net)
  (bfs end (list (list start)) net))
(defun bfs (end queue net)
  (if (null queue)
    nil
    (let ((path (car queue)))
      (let ((node (car path)))
	(if (eql node end)
	  (nreverse path)
	  (bfs end
	       (nconc (cdr queue)
		      (new-paths path node net))
	       net))))))
(defun new-paths (path node net)
  (mapcar #'(lambda (n)
	      (cons n path))
	  (cdr (assoc node net))))
; [注] 比原函数少用33%的内存空间


;; 5.使用存储池修改 4.7 节中的二叉搜索的代码。
; 略
