;; ntimes宏
;; (ntimes 次数n
;;         动作)
;; 将动作执行n次。

(defmacro ntimes (n &rest action)
  (let ((g (gensym))
	(h (gensym)))
    `(let ((,h ,n))
       (do ((,g 0 (+ ,g 1)))
	 ((>= ,g ,h))
	 ,@action))))
