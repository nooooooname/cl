;; 定义一个宏while
;; (while 条件
;;        循环体)
;; 循环体由任意个表达式组成，
;; 逻辑等同于C语言里的while循环。

(defmacro while (test &rest body)
  `(do ()
     ((not ,test))
     ,@body))
