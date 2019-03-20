;; 1.用箱子表示法表示以下列表：(略)

;; 2.写一个保留原本列表中元素顺序的 union 版本：
; a:
(defun new-union (lst1 lst2)
  (if (null lst2)
    lst1
    (let ((elemt (car lst2)))
      (if (member elemt lst1)
	(new-union lst1 (cdr lst2))
	(new-union (append lst1
			   (list elemt))
		   (cdr lst2))))))

;; 3.定义一个函数，接受一个列表并返回一个列表，指出相等元素出现的次数，并由最常见至最少见的排序：
; a:
(defun occurrences (lst)
  (if (null lst)
    nil
    (let ((ret (occurrences (cdr lst))) (elemt (car lst)))
      (sort
	(if (member elemt ret :key #'car)
	  (mapcar #'(lambda (c)
		      (if (eql (car c) elemt)
			(cons elemt (+ (cdr c) 1))
			c))
		  ret)
	  (append ret (list (cons elemt 1))))
	#'(lambda (x y) (> (cdr x) (cdr y)))))))

;; 4.为什么 (member '(a) '((a) (b))) 返回 nil？
; a: 因为(eql '(a) '(a))返回nil

;; 5.假设函数 pos+ 接受一个列表并返回把每个元素加上自己的位置的列表：
; > (pos+ '(7 5 1 4))
;   (7 6 3 7)
;    使用 (a) 递归 (b) 迭代 (c) mapcar 来定义这个函数。
; a:
;  (a)
(defun pos+ (lst)
  (if (null lst)
    nil
    (let ((rev (reverse lst)) (len (length lst)))
      (let ((tail (- (+ (car rev) len) 1)))
	(append (pos+ (reverse (cdr rev)))
	      (list tail))))))
;  (b)
(defun pos+ (lst)
  (do ((rst lst (cdr rst))
       (ret nil)
       (i 0 (+ i 1)))
    ((null rst) ret)
    (setf ret
	  (append ret
		  (list (+ (car rst) i))))))
;  (c)
(defun pos+ (lst)
  (let ((i -1))
    (mapcar #'(lambda (e)
		(setf i (+ i 1))
		(+ e i))
	    lst)))

;; 6.经过好几年的审议，政府委员会决定列表应该由 cdr 指向第一个元素，而 car 指向剩下的列表。定义符合政府版本的以下函数：
;  (a) cons
; a:
(defun cons (a b)
  (append (list b) a))
;  (b) list
(defun list (&rest lst)
  (if (null lst)
    nil
    (cons (car lst) (list (cdr lst)))))
;  (c) length (for lists)
; a:
(defun length (lst)
  (if (null lst)
    0
    (+ 1
       (length (car lst)))))
;  (d) member (for lists; no keywords)
(defun member (obj lst)
  (if (null lst)
    nil
    (if (eql obj (cdr lst))
      lst
      (member obj (car lst)))))

;; 7.修改图 3.6 的程序(游程编码)，使它使用更少 cons 核。 （提示：使用点状列表）
; a:
(defun compress (x)
  (if (consp x)
    (compr (car x) 1 (cdr x))
    x))
(defun compr (elt n lst)
  (if (null lst)
    (list (n-elts elt n))
    (let ((next (car lst)))
      (if (eql next elt)
	(compr elt (+ n 1) (cdr lst))
	(cons (n-elts elt n)
	      (compr next 1 (cdr lst)))))))
(defun n-elts (elt n)
  (if (> n 1)
    (cons n elt)
    elt))

;; 8.定义一个函数，接受一个列表并用点状表示法印出：
; > (showdots '(a b c))
;   (A . (B . (C . NIL)))
; a:
(defun showdots (lst)
  (if (null lst)
    (format t "~A" nil)
    (progn
      (format t "(~A . " (car lst))
      (showdots (cdr lst))
      (format t ")"))))


;; 9.写一个程序来找到 3.15 节里表示的网络中，最长有限的路径 (不重复)。网络可能包含循环。
; a:
(defun longest-path (start end net)
  "返回在有向有环图net中以start为起点，以end为终点的最长路径(之一)。没有路径则返回nil。
  图用邻接表的形式表示：
  ((节点1 邻接节点1 邻接节点2 ... 邻接节点n)
   (节点2 邻接节点1 邻接节点2 ... 邻接节点n)
   ...
   (节点n 邻接节点1 邻接节点2 ... 邻接节点n))"
  (bfs end (list (list start)) net))

(defun bfs (end queue net)
  "用广度优先搜索以queue为待搜索队列在图net上搜索到节点end的最长路径。不存在则返回nil。
  队列中的每一项是一个倒序的已搜索路径
  思路：判空队列，队首出队后用剩下的队列继续搜索，有路径即为最长，无路径则判断队首是否已到达终点。"
  (if (null queue)
    nil
    (let ((path (car queue)))
      (let ((node (car path)))
	(let ((next-path
		(bfs end
		     (append (cdr queue)
			     (new-paths path node net))
		     net)))
	  (if (not (null next-path))
	    next-path
	    (if (eql node end)
	      (reverse path)
	      nil)))))))

(defun new-paths (path node net)
  "返回一个列表，每一项的car是节点node在图net中的邻接点，cdr是path。
  跳过在path中已有的邻接点。
  若node没有邻接点则返回nil。"
  (do ((rst (cdr (assoc node net)) (cdr rst))
       (ret nil))
    ((null rst) ret)
    (let ((n (car rst)))
      (if (not (member n path))
	(setf ret (append ret (list (cons n path))))))))
