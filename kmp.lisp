(load "buf.lisp")
(defun file-subst (old new ifile ofile)
  "文件字符串替换，传入一个旧字符串、一个新字符串、一个输入文件和一个输出文件，
  用新字符串替换输入文件的内容中所有旧字符串，再把替换后的内容写入输出文件中。
  返回替换了几处，如果输出文件已存在则覆盖。"
  (with-open-file (istrm ifile :direction :input)
    (with-open-file (ostrm ofile :direction :output :if-exists :supersede)
      (let* ((rbuf (new-buf (length old)))
	     (from-filep t)
	     (next (lambda ()
		     (if from-filep
		       (let ((res (read-char istrm nil :eof)))
			 (if (eql res :eof)
			   (buf-flush rbuf ostrm)
			   (buf-push res rbuf))
			 res)
		       (progn
			 (setf from-filep t)
			 (buf-tail rbuf)))))
	     (pass (lambda (n)
		     (dotimes (i n)
		       (princ (buf-pop-front rbuf) ostrm))
		     (setf from-filep (zerop (buf-cnt rbuf)))))
	     (out-new (lambda ()
			(princ new ostrm)
			(buf-clear rbuf)
			(setf from-filep t))))
	(stream-kmp old (kmp-next old) next pass out-new)))))

(defun stream-kmp (target next input pass action)
  (let ((match-cnt 0)
	(matching-index 0)
	(len (length target)))
    (do ((c (funcall input) (funcall input)))
      ((eql c :eof) match-cnt)
      (if (char= c (char target matching-index))
	(if (= matching-index (- len 1))
	  (progn
	    (funcall action)
	    (setf matching-index 0)
	    (incf match-cnt))
	  (incf matching-index))
	;;不匹配
	(let ((k (svref next matching-index)))
	  (funcall pass (- matching-index k))
	  (setf matching-index k)
	  (if (= matching-index -1)
	    (setf matching-index 0)))))))

(defun kmp-next (str)
  "求非空字符串str的next数组，返回一个向量"
  (let ((len (length str)))
    (if (= len 1)
      (vector -1)
      (do ((k -1 k) (i 1 i) (next (list -1) next))
	((= i len) (apply #'vector next))
	(if (or (= k -1)
		(char= (char str (- i 1))
		       (char str k)))
	  (setf next (append next (list (incf k)))
		i (+ i 1))
	  (setf k (elt next k)))))))
