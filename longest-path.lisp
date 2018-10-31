(defun longest-path (start end net)
  "返回在有向有环图net中以start为起点，以end为终点的最长路径(之一)。没有路径则返回nil。
  图用邻接表的形式表示：
  ((节点1 邻接节点1 邻接节点2 ... 邻接节点n)
   (节点2 邻接节点1 邻接节点2 ... 邻接节点n)
   ...
   (节点n 邻接节点1 邻接节点2 ... 邻接节点n))"
  (bfs end (list (list start)) net))

(defun bfs (end queue net)
  "用广度优先搜索以queue为待搜索队列在图net上搜索到节点end的最长路径。不存在则返回nil。
  队列中的每一项是一个倒序的已搜索路径
  思路：判空队列，队首出队后用剩下的队列继续搜索，有路径即为最长，无路径则判断队首是否已到达终点。"
  (if (null queue)
    nil
    (let ((path (car queue)))
      (let ((node (car path)))
	(let ((next-path
		(bfs end
		     (append (cdr queue)
			     (new-paths path node net))
		     net)))
	  (if (not (null next-path))
	    next-path
	    (if (eql node end)
	      (reverse path)
	      nil)))))))

(defun new-paths (path node net)
  "返回一个列表，每一项的car是节点node在图net中的邻接点，cdr是path。
  跳过在path中已有的邻接点。
  若node没有邻接点则返回nil。"
  (do ((rst (cdr (assoc node net)) (cdr rst))
       (ret nil))
    ((null rst) ret)
    (let ((n (car rst)))
      (if (not (member n path))
	(setf ret (append ret (list (cons n path))))))))
