(defun ask-number ()
  "等待用户输入一个数字，若用户输入的不是数字则重输"
  (format t "请输入一个数字：")
  (let ((input (read)))
    (if (numberp input)
      input
      (ask-number))))
