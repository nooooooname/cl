;; 仿C语言的for循环宏
;; (for ((a 1) (b 2)...) (< a 10) (incf a)
;;      循环体)
;; 相当于C语言中的
;; for(a = 1, b = 2,...; a < 10; a++)
;; {
;;	循环体
;; }
(defmacro for (init test iter &body body)
  `(do ,init
     ((not ,test))
     ,@body
     ,iter))

;; in宏
;; 返回在choices中是否存在元素和obj相同(eql)
;; 若存在，则该元素后边的表达式都不会被求值
;; 若不存在，则所有表达式都会被求值
(defmacro in (obj &rest choices)
  (let ((g (gensym)))
    `(let ((,g ,obj))
       (or ,@(mapcar (lambda (c) `(eql ,g ,c))
		     choices)))))

;; random-choice宏
;; 从参数中随机选取一个返回
(defmacro random-choice (&rest choices)
  (let ((key -1))
    `(case (random ,(length choices))
       ,@(mapcar (lambda (c) `(,(incf key) ,c))
		 choices))))

;; avg宏
;; 求若干个数字的平均值
(defmacro avg (&rest args)
  `(/ (+ ,@args) ,(length args)))

;; aif宏
;; (aif test then else)
;; 会被展开为
;; (let ((it test))
;;   (if it then else))
;; 这里故意捕捉一个变量it，
;; 使得在then和else中可以
;; 借此获取条件表达式的求值结果。
(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))
