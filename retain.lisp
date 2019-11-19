;; retain宏（第十章第6题）
;; (retain 变量列表
;;	   代码主体)
;; 执行代码主体，
;; 但在执行后恢复变量列表中各变量的值。
(defmacro retain (parms &body body)
  `((lambda ,parms ,@body) ,@parms))
