;; 构建HTML字符串
(defun html (head body)
  (format nil "<!DOCTYPE html>~%<html>~%~A~&~A~&</html>" head body))

;; 构建HTML双标签字符串
(defun dual-mark (mark content)
  (format nil "<~A>~A</~A>" mark content mark))

;; 状态码文本对照表
(defconstant status-text (list
			   (cons 200 "OK")
			   (cons 301 "Moved Permanently")
			   (cons 302 "Temporary Redirect")
			   (cons 400 "Bad Request")
			   (cons 401 "Unauthonzed")
			   (cons 403 "Forbidden")
			   (cons 404 "Not Found")
			   (cons 500 "Internal Server Error")
			   (cons 503 "Service Unavailable")))

;; 构建HTTP报文首部关联列表
(defun mkheaders (&rest args)
  (do ((res (list (cons (pop args) (pop args)))
	    (append res (list (cons (pop args) (pop args))))))
    ((null args) res)))

;; 构建HTTP响应文本
(defun mkresp (version status headers entity-body &optional (text (cdr (assoc status status-text))))
  (let ((resp (format nil "HTTP/~A ~A ~A~%" version status text)))
    (dolist (h headers)
      (setf resp (format nil "~A~A: ~A~%" resp (car h) (cdr h))))
    (format nil "~A~%~A" resp entity-body)))

;; 修改或添加首部关联列表的字段
(defun set-header-field (headers h v)
  (let ((p (position h headers :key #'car :test #'equal)))
    (if p
      (setf (cdr (nth p headers)) v)
      (nconc headers (list (cons h v))))
    headers))

;; 序列分割
(defun split (seq pattern &key (count -1) (test #'equal) (start 0) (end nil) (plen (length pattern)))
  (if (or (null end)
	  (> end (length seq)))
    (setf end (length seq)))
  (and (< start end)
       (do ((i start (+ i 1)))
	 ((or (> (+ i plen) end)
	      (zerop count))
	  (if (/= start end)
	    (list (subseq seq start end))))
	 (if (funcall test pattern (subseq seq i (+ i plen)))
	   (if (= i start)
	     (return-from split (split seq pattern :count (- count 1) :test test :plen plen
				       :start (+ i plen) :end end))
	     (return-from split
			  (append (list (subseq seq start i))
				  (split seq pattern :count (- count 1) :test test :plen plen
					 :start (+ i plen) :end end))))))))

;; 读取并解析HTTP请求，依次返回请求的方法、目标、版本、首部关联列表和请求数据
(defun rdreq (stream)
  (let* ((start-line (split (read-line stream) " "))
	 (method (car start-line))
	 (target (cadr start-line))
	 (version (cadr (split (caddr start-line) "/")))
	 (headers (cons nil nil)) (data nil))
    (do* ((line (read-line stream) (read-line stream))
	  (header (split line ": " :count 1) (split line ": " :count 1)))
      ((string= line "") (setf headers (cdr headers)))
      (nconc headers (list (cons (car header) (cadr header)))))
    (let ((content-length (cdr (assoc "Content-Length" headers :test #'string=))))
      (if content-length
	(setf data (with-output-to-string (s)
		     (dotimes (i (parse-integer content-length))
		       (write-char (read-char stream) s))))))
    (values method target version headers data)))
