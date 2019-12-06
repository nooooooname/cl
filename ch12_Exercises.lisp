;; 1.画三个不同的树，能够被打印成 ((A) (A) (A)) 。写一个表达式来生成它们。
; a: 图略
;    (let ((ele (list 'a)))
;      (list ele ele ele))
;    (list (list 'a) (list 'a) (list 'a))
;    (let ((ele (list 'a)))
;      (list (copy-list ele) ele ele))


;; 2.假设 make-queue ， enqueue 和 dequeue 是按照图 12.7 中的定义，用箱子表式法画出下面每一步所得到的队列的结构图：
; > (setf q (make-queue))
; (NIL)
; > (enqueue 'a q)
; (A)
; > (enqueue 'b q)
; (A B)
; > (dequeue q)
; A
; 略


;; 3.定义一个函数 copy-queue ，可以返回一个 queue 的拷贝。
; a:
(defun copy-queue (queue)
  (let ((res (cons (copy-list (car queue)) nil)))
    (if (car res)
      (setf (cdr res) (last (car res))))
    res))


;; 4.定义一个函数，接受两个输入参数 object 和 queue ，能将 object 插入到 queue 的首端。
; a:
(defun push-front (object queue)
  (setf (car queue) (cons object (car queue)))
  (if (null (cdar queue))
    (setf (cdr queue) (car queue))))


;; 5.定义一个函数，接受两个输入参数 object 和 queue，能具有破坏性地将 object 的第一个实例 ( eql 等价地) 移到 queue 的首端。
; a:
(defun move-front (object queue)
  (let ((ls (car queue)))
    (setf (car queue) (if (member object ls)
			(cons object (remove object ls))
			ls)
	  (cdr queue) (last (car queue))))
  (car queue))


;; 6.定义一个函数，接受两个输入参数 object 和 lst ( lst 可能是 cdr-circular 列表)，如果 object 是 lst 的成员时返回真。
; a:
(defun ac-member (object lst)
  (do ((sublst lst (cdr lst)))
    ((or (eql sublst lst)
	 (eql (car sublst) object))
     (eql (car sublst) object))))


;; 7.定义一个函数，如果它的参数是一个 cdr-circular 则返回真。
; a:
(defun cdr-circularp (lst)
  (and (consp lst)
       (eql (cdr lst) lst)))


;; 8.定义一个函数，如果它的参数是一个 car-circular 则返回真。
; a:
(defun car-circularp (lst)
  (and (consp lst)
       (eql (car lst) lst)))
