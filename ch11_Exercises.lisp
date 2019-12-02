;; 1.替图 11.2 所定义的类定义访问器、 initforms 以及 initargs 。重写相关的代码使其再也不用调用 slot-value 。
; a:
(defclass rectangle ()
  ((height :accessor rectangle-h
	   :initform 1
	   :initarg :height)
   (width :accessor rectangle-w
	  :initform 1
	  :initarg :width)))
(defclass circle ()
  ((radius :accessor circle-r
	   :initform 1
	   :initarg :radius)))
(defmethod area ((x rectangle))
  (* (rectangle-h x) (rectangle-w x)))
(defmethod area ((x circle))
  (* pi (expt (circle-r x) 2)))


;; 2.重写图 9.5 的代码，使得球体与点为类别，而 intersect 及 normal 为通用函数。
; a:
(defclass point ()
  ((x :accessor x
      :initarg :x)
   (y :accessor y
      :initarg :y)
   (z :accessor z
      :initarg :z)))
(defclass surface ()
  ((color :accessor surface-color
	  :initarg :color)))
(defclass sphere (surface)
  ((center :accessor sphere-center
	   :initarg :center)
   (radius :accessor sphere-radius
	   :initarg :radius)))
(defun defsphere (x y z r c)
  (let ((s (make-instance 'sphere
			  :center (make-instance 'point
						 :x x
						 :y y
						 :z z)
			  :radius r
			  :color c)))
    (push s *world*)
    s))
(defmethod intersect ((s sphere) (pt point) xr yr zr)
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
      (make-instance 'point
		     :x (+ (x pt) (* n xr))
		     :y (+ (y pt) (* n yr))
		     :z (+ (z pt) (* n zr))))))
(defun normal ((s sphere) (pt point))
  (let ((c (sphere-center s)))
    (unit-vector (- (x c) (x pt))
		 (- (y c) (y pt))
		 (- (z c) (z pt)))))


;; 3.假设有若干类别定义如下：
;  (defclass a (c d)   ...)  (defclass e ()  ...)
;  (defclass b (d c)   ...)  (defclass f (h) ...)
;  (defclass c ()      ...)  (defclass g (h) ...)
;  (defclass d (e f g) ...)  (defclass h ()  ...)
;  a.画出表示类别 a 祖先的网络以及列出 a 的实例归属的类别，从最相关至最不相关排列。
;  b.替类别 b 也做 (a) 小题的要求。
; a: 图略
;    a,c,d,e,f,g,h,standard-object,t
;    b,d,e,f,g,h,c,standard-object,t


;; 4.假定你已经有了下列函数：
;  precedence ：接受一个对象并返回其优先级列表，列表由最具体至最不具体的类组成。
;  methods ：接受一个通用函数并返回一个列出所有方法的列表。
;  specializations ：接受一个方法并返回一个列出所有特化参数的列表。返回列表中的每个元素是类别或是这种形式的列表 (eql x) ，或是 t （表示该参数没有被特化）。
;  使用这些函数（不要使用 compute-applicable-methods 及 find-method ），定义一个函数 most-spec-app-meth ，该函数接受一个通用函数及一个列出此函数被调用过的参数，如果有最相关可用的方法的话，返回它。
; a:
(defun most-spec-app-meth (f &rest args)
  (let* ((plst (mapcar #'precedence args))
	 (mlst (methods f))
	 (mlst (mapcar (lambda (m)
			 (cons m (specializations m)))
		       mlst))
	 (slst nil))
    (labels ((specificity> (a b)
			   (let* ((prec (nth (- (length plst) (length a))
					     plst))
				  (speca (position (car a) prec))
				  (specb (position (car b) prec)))
			     (if (null speca)
			       (or (not (null specb))
				   (specificity> (cdr a) (cdr b)))
			       (and (not (null specb))
				    (if (/= speca specb)
				      (< speca specb)
				      (specificity> (cdr a) (cdr b)))))))
	     (applicablep (params)
			  (not (member nil
				       (mapcar (lambda (param prec arg)
						 (or (position param prec)
						     (and (listp param)
							  (eql (cadr param) arg))))
					       params plst args)))))
      (dolist (m mlst)
	(if (applicablep (cdr m))
	  (setf slst (append slst (list m)))))
      (sort slst #'specificity> :key #'cdr)
      (if (not (null slst))
	(caar slst)))))


;; 5.不要改变通用函数 area 的行为（图 11.2），
; 题目不完整，略


;; 6.举一个只有通用函数的第一个参数被特化会很难解决的问题的例子。
; 略
