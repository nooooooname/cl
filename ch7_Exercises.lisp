;; 1.定义一个函数，接受一个文件名并返回一个由字符串组成的列表，来表示文件里的每一行。
; a:
(defun lines (fname)
  (with-open-file (strm fname :direction :input)
    (do ((lst nil (append lst (list next-line)))
	 (next-line (read-line strm nil :eof)
		    (read-line strm nil :eof)))
      ((eql next-line :eof) lst))))


;; 2.定义一个函数，接受一个文件名并返回一个由表达式组成的列表，来表示文件里的每一行。
; a:
(defun lines (fname)
  (with-open-file (strm fname :direction :input)
    (do ((lst nil (append lst (list next-exp)))
	 (next-exp (read strm nil :eof)
		   (read strm nil :eof)))
      ((eql next-exp :eof) lst))))


;; 3.假设有某种格式的文本文件，注解是由 % 字符表示。从这个字符开始直到行尾都会被忽略。定义一个函数，接受两个文件名称，并拷贝第一个文件的内容去掉注解，写至第二个文件。
; a:
(defun uncomment (ifile ofile)
  (with-open-file (istrm ifile :direction :input)
    (with-open-file (ostrm ofile :direction :output :if-exists :supersede)
      (do ((next-line (read-line istrm nil :eof) (read-line istrm nil :eof)))
	((eql next-line :eof))
	(princ (subseq next-line 0 (position #\% next-line)) ostrm)
	(terpri ostrm)))))


;; 4.定义一个函数，接受一个二维浮点数组，将其用简洁的栏位显示。每个元素应印至小数点二位，一栏十个字符宽。（假设所有的字符可以容纳）。你会需要 array-dimensions。
; a:
(defun print-float-array (arr)
  (let* ((dimensions (array-dimensions arr))
	 (rows (car dimensions))
	 (cols (cadr dimensions)))
    (dotimes (i rows)
      (dotimes (j cols)
	(format t "~10,2F" (aref arr i j)))
      (terpri))))


;; 5.修改 stream-subst 来允许万用字符 (wildcard) 可以在模式中使用。若字符 + 出现在 old 里，它应该匹配任何输入字符。
; a:
(defun stream-subst (old new in out)
  (let* ((pos 0)
	 (len (length old))
	 (buf (new-buf len))
	 (from-buf nil))
    (do ((c (read-char in nil :eof)
	    (or (setf from-buf (buf-next buf))
		(read-char in nil :eof))))
      ((eql c :eof))
      (cond ((or (char= #\+ (char old pos))
		 (char= c (char old pos)))
	     (incf pos)
	     (cond ((= pos len)
		    (princ new out)
		    (setf pos 0)
		    (buf-clear buf))
		   ((not from-buf)
		    (buf-insert c buf))))
	    ((zerop pos)
	     (princ c out)
	     (when from-buf
	       (buf-pop buf)
	       (buf-reset buf)))
	    (t
	      (unless from-buf
		(buf-insert c buf))
	      (princ (buf-pop buf) out)
	      (buf-reset buf)
	      (setf pos 0))))
    (buf-flush buf out)))


;; 6.修改 stream-subst 来允许模式可以包含一个用来匹配任何数字的元素，以及一个可以匹配任何英文字符的元素或是一个可以匹配任何字符的元素。模式必须可以匹配任何特定的输入字符。(提示: old 可以不是一个字符串。)
; a:
(defun stream-subst (old new in out)
  "#\?匹配任何数字，#\-匹配任何英文字符，#\+匹配任何字符"
  (let* ((pos 0)
	 (len (length old))
	 (buf (new-buf len))
	 (from-buf nil))
    (do ((c (read-char in nil :eof)
	    (or (setf from-buf (buf-next buf))
		(read-char in nil :eof))))
      ((eql c :eof))
      (cond ((or (char= (elt old pos) #\+)
		 (and (char= (elt old pos) #\?)
		      (digit-char-p c))
		 (and (char= (elt old pos) #\-)
		      (alpha-char-p c))
		 (char= c (elt old pos)))
	     (incf pos)
	     (cond ((= pos len)
		    (princ new out)
		    (setf pos 0)
		    (buf-clear buf))
		   ((not from-buf)
		    (buf-insert c buf))))
	    ((zerop pos)
	     (princ c out)
	     (when from-buf
	       (buf-pop buf)
	       (buf-reset buf)))
	    (t
	      (unless from-buf
		(buf-insert c buf))
	      (princ (buf-pop buf) out)
	      (buf-reset buf)
	      (setf pos 0))))
    (buf-flush buf out)))
