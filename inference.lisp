(defvar *rules* (make-hash-table))
(defmacro <- (con &optional ant)
  ;; 定义规则宏
  ;; 将一条规则存入规则哈希表*rules*中。
  ;; con和ant分别是结论表达式和条件表达式，
  ;; 若条件表达式成立，则结论表达式成立，
  ;; 若没有条件表达式，则结论表达式始终成立。
  `(length (push (cons (cdr ',con) ',ant)
		 (gethash (car ',con) *rules*))))

(defun match (x y &optional binds)
  "以绑定列表binds为基础，尝试匹配x和y，x和y可以是符号或列表。
  若匹配成功，返回2个值，
  第一个返回值是能使x和y匹配的绑定列表，第二个返回值是T。
  若匹配失败则返回一个NIL。"
  (cond
    ((eql x y) (values binds t))
    ((assoc x binds) (match (binding x binds) y binds))
    ((assoc y binds) (match x (binding y binds) binds))
    ((var? x) (values (cons (cons x y) binds) t))
    ((var? y) (values (cons (cons y x) binds) t))
    (t (when (and (consp x) (consp y))
	 (multiple-value-bind (b2 yes) (match (car x) (car y) binds)
	   (and yes (match (cdr x) (cdr y) b2)))))))

(defun var? (x)
  "判断x是不是以“?”开头的符号"
  (and (symbolp x)
       (eql (char (symbol-name x) 0) #\?)))

(defun binding (x binds)
  "递归查询绑定列表binds，返回x所绑定的符号"
  (let ((b (assoc x binds)))
    (if b
      (or (binding (cdr b) binds)
	  (cdr b)))))

(defun prove (expr &optional binds)
  "以绑定列表binds为基础，尝试证明表达式expr。
  若有n组条件可以使expr成立，返回一个长度为n的列表，
  列表中每一项都是一个绑定列表，
  用以表示可以使expr成立的一组条件。
  若expr无条件成立，则返回(NIL)。
  若expr不成立，则返回NIL。"
  (case (car expr)
    (and (prove-and (reverse (cdr expr)) binds))
    (or  (prove-or (cdr expr) binds))
    (not (prove-not (cadr expr) binds))
    (t   (prove-simple (car expr) (cdr expr) binds))))

(defun prove-simple (pred args binds)
  "以绑定列表binds为基础，
  尝试证明以pred为判断式，args为参数列表的表达式。
  若有n组条件可以使表达式成立，返回一个长度为n的列表，
  列表中每一项都是一个绑定列表，
  用以表示可以使表达式成立的一组条件。
  若表达式无条件成立，则返回(NIL)。
  若表达式不成立，则返回NIL。"
  (mapcan #'(lambda (r)
	      (multiple-value-bind (b2 yes) (match args (car r) binds)
		(when yes
		  (if (cdr r)
		    (prove (cdr r) b2)
		    (list b2)))))
	  (mapcar #'change-vars
		  (gethash pred *rules*))))

(defun change-vars (r)
  "将列表r中所有以问号开头的符号递归替换成(gensym “?”)。"
  (sublis (mapcar #'(lambda (v) (cons v (gensym "?")))
		  (vars-in r))
	  r))

(defun vars-in (expr)
  "返回expr中所有以问号开头的符号组成的列表。"
  (if (atom expr)
    (if (var? expr) (list expr))
    (union (vars-in (car expr))
	   (vars-in (cdr expr)))))

(defun prove-and (clauses binds)
  "以绑定列表binds为基础，
  从后往前尝试证明表达式列表clauses中所有的表达式。
  若有n组条件可以使所有表达式同时成立，
  则返回一个长度为n的列表，
  列表中每一项都是一个绑定列表，
  用以表示可以使所有表达式同时成立的一组条件。
  若所有表达式都无条件成立，则返回(NIL)。
  若无法使所有表达式同时成立，则返回NIL。"
  (if (null clauses)
    (list binds)
    (mapcan #'(lambda (b)
		(prove (car clauses) b))
	    (prove-and (cdr clauses) binds))))

(defun prove-or (clauses binds)
  "以绑定列表binds为基础，用函数prove
  从前往后依次尝试证明表达式列表clauses中的各表达式。
  再将每次调用prove返回的列表拼接起来返回。"
  (mapcan #'(lambda (c) (prove c binds))
	  clauses))

(defun prove-not (clause binds)
  "以绑定列表binds为前提，尝试证明表达式clause不成立。
  若clause没有成立的可能，则返回(list binds)。
  否则返回NIL。"
  (unless (prove clause binds)
    (list binds)))

(defmacro with-answer (query &body body)
  ;; 回答查询
  ;; 用函数prove证明表达式query，得到若干个绑定列表，
  ;; 对于每个绑定列表，
  ;; 以变量绑定的符号为值创建同名局部变量，
  ;; 并在其作用域下依次执行body。
  (let ((binds (gensym)))
    `(dolist (,binds (prove ',query))
       (let ,(mapcar #'(lambda (v)
			 `(,v (binding ',v ,binds)))
		     (vars-in query))
	 ,@body))))
