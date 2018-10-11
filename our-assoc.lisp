(defun our-assoc (sym lst)
  "仿assoc"
  (and (consp lst)
       (if (eql (car (car lst)) sym)
	 (car lst)
	 (our-assoc sym (cdr lst)))))
