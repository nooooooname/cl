(defun float-limits ()
  "显示24个用来标示浮点表示法的限制的全局常量，返回nil。"
  (dolist (m '(most least))
    (dolist (s '(positive negative))
      (let ((nlst (append (list "")
			  (if (eql m 'least)
			    '(normalized-)))))
	(dolist (n nlst)
	  (dolist (f '(short single double long))
	    (let ((sym (intern (string-upcase
				 (format nil "~A-~A-~A~A-float"
					 m s n f)))))
	      (format t "~40A~A~%" sym (symbol-value sym)))))))))
