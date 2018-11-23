(defun nth-word (n str)
  "返回以空格分隔的字符串str中的第n个单词，不检查n是否越界"
  (if (> n 1)
    (nth-word (- n 1)
	      (subseq str (+ (position #\  str) 1)))
    (subseq str 0 (position #\  str))))
