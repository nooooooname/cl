(defmacro as (tag content)
  "<tag>content</tag>"
  `(format t "<~(~A~)>~A</~(~A~)>"
	   ',tag ,content ',tag))

(defmacro with (tag &rest body)
  "~&<tag>~%,@body~&</tag>~%"
  `(progn
     (format t "~&<~(~A~)>~%" ',tag)
     ,@body
     (format t "~&</~(~A~)>~%" ',tag)))

(defmacro brs (&optional (n 1))
  "n个<br>"
  (let ((g (gensym)))
    `(progn
       (fresh-line)
       (dotimes (,g ,n)
	 (princ "<br>"))
       (terpri))))

(defun html-file (base)
  "生成文件名base.html"
  (format nil "~(~A~).html" base))

(defmacro page (name title &rest body)
  "以name为主文件名，title为标题，
  body为内容输出操作创建页面文件。"
  `(with-open-file (*standard-output*
		     (html-file ,name)
		     :direction :output
		     :if-exists :supersede)
     (format t "<!DOCTYPE html>~%")
     (with html
	   (with head
		 (as title ,title))
	   (with body
		 ,@body))))

(defmacro with-link (dest &rest body)
  "<a href=\"(html-file dese)\">,@body</a>"
  `(progn
     (format t "<a href=\"~A\">" (html-file ,dest))
     ,@body
     (princ "</a>")))

(defun link-item (dest text)
  "<li><a href=\"(html-file dese)\">text</a>"
  (princ "<li>")
  (with-link dest
	     (princ text)))

(defun button (dest text)
  (princ "[ ")
  (with-link dest
	     (princ text))
  (format t " ]~%"))

(defun map3 (fn lst)
  "接受一个函数fn和一个列表lst作为参数，
  对于传入列表中的每个元素，都会用三个参数来调用传入函数，
  分别是元素本身，前一个元素，以及后一个元素。
  当没有前一个元素或者后一个元素时，使用 nil 代替。"
  (labels ((rec (curr prev next left)
		(funcall fn curr prev next)
		(when left
		  (rec (car left)
		       curr
		       (cadr left)
		       (cdr left)))))
    (when lst
      (rec (car lst) nil (cadr lst) (cdr lst)))))

(defparameter *sections* nil)

(defstruct item
  id title text)

(defstruct section
  id title items)

(defmacro defitem (id title text)
  `(setf ,id
	 (make-item :id		',id
		    :title	,title
		    :text	,text)))

(defmacro defsection (id title &rest items)
  `(setf ,id
	 (make-section :id	',id
		       :title	,title
		       :items	(list ,@items))))

(defun defsite (&rest sections)
  (setf *sections* sections))

(defconstant contents "contents")
(defconstant index "index")

(defun gen-contents (&optional (sections *sections*))
  (page contents contents
	(with ol
	      (dolist (s sections)
		(link-item (section-id s) (section-title s))
		(brs))
	      (link-item index (string-capitalize index)))))

(defun gen-index (&optional (sections *sections*))
  (page index index
	(with ol
	      (dolist (i (all-items sections))
		(link-item (item-id i) (item-title i))
		(brs)))))

(defun all-items (sections)
  (let ((is nil))
    (dolist (s sections)
      (dolist (i (section-items s))
	(setf is (merge 'list (list i) is #'title<))))
    is))

(defun title< (x y)
  (string-lessp (item-title x) (item-title y)))

(defun gen-site ()
  (map3 #'gen-section *sections*)
  (gen-contents)
  (gen-index))

(defun gen-section (sect <sect sect>)
  (page (section-id sect) (section-title sect)
	(with ol
	      (map3 #'(lambda (item <item item>)
			(link-item (item-id item)
				   (item-title item))
			(brs)
			(gen-item sect item <item item>))
		    (section-items sect)))
	(brs)
	(gen-move-buttons (if <sect (section-id <sect))
			  contents
			  (if sect> (section-id sect>)))))

(defun gen-item (sect item <item item>)
  (page (item-id item) (item-title item)
	(princ (item-text item))
	(brs 3)
	(gen-move-buttons (if <item (item-id <item))
			  (section-id sect)
			  (if item> (item-id item>)))))

(defun gen-move-buttons (back up forward)
  (if back (button back "Back"))
  (if up (button up "Up"))
  (if forward (button forward "Forward")))
