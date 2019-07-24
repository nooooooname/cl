(defun pseudo-cat (filename)
  "输出一个文件的内容，参数是文件名"
  (with-open-file (strm (make-pathname :name filename)
			:direction :input)
    (do ((line (read-line strm nil 'eof)
	       (read-line strm nil 'eof)))
      ((eql line 'eof))
      (format t "~A~%" line))))
