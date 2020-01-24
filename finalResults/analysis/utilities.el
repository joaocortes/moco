
(defun joc-trajectories-time (&optional file)
  (interactive "P")
  (save-current-buffer
    (if file   (find-file file))
    (assert (string-match "solver" file))
    (goto-char (point-min))
    ;; (occur "[0-9]*\.[0-9]*/[0-9]*\.[0-9]*[[:blank:]]o")
    (let ((file (or file (buffer-file-name))) start result n)
      (setq n 0)
      (setq result "")
      (let (start)
	(while  (search-forward-regexp
		 "[0-9]*\\.[0-9]*/[0-9]*\\.[0-9]*[[:blank:]]o" (point-max) t )
	  (move-beginning-of-line 1)
	  (search-forward  "/")
	  (setq start (point))
	  (search-forward-regexp  "[0-9]*\\.[0-9]*")
	  (copy-region-as-kill start (point))
	  (setq result (concat result (current-kill 0)))
	  (setq result (concat result ", "))
	  (incf n)
	  ))
      (let ((plot-name
	     (replace-regexp-in-string
	      "solver_" "timePlot_"
	      file) ))
	(with-temp-file 
	    plot-name
	  (goto-char (point-min))
	  (insert result)
	  (unless (= n 0)
	    (search-backward ",")
	    (kill-line)))
	(expand-file-name plot-name)))))

(defun joc-trajectories-memory (&optional file)
  (interactive "P")
  (save-current-buffer 
    (if file   (find-file file))
    (setq file (or file (buffer-file-name)))
    (goto-char (point-min))
    ;; (occur "[0-9]*\.[0-9]*/[0-9]*\.[0-9]*[[:blank:]]o")
    (let ( start result n)
      (setq n 0)
      (setq result nil )
      (let (start time memory)
	(while  (search-forward-regexp "\\[startup\\+" (point-max) t )
	  (assert (string-match "watcher" file))
	  (setq start (point))
	  (search-forward-regexp  "[0-9]*\\.?[0-9]*")
	  (copy-region-as-kill start (point))
	  (setq time  (current-kill 0))
	  (condition-case  nil (search-forward-regexp
				"Current children cumulated memory: ") 
	    (error (setq memory nil) ()))
	  (setq start (point))
	  (search-forward-regexp  "[0-9]*")
	  (copy-region-as-kill start (point))
	  (setq memory (number-to-string (truncate (/ (string-to-number (current-kill 0)) 1000000))))
	  (setq result (append result (list `(,time . ,memory))))
	  (incf n)))
      (let (result-string result-current)
	(while (setq result-current (pop result))
	  (setq time (car result-current))
	  (setq memory (cdr result-current))
	  (setq result-string
		(concat result-string
			time ", " memory "\n")))
(let ((plot-name 
       (replace-regexp-in-string
	     "watcher_" "memoryPlot_"
	     file)))
  (with-temp-file 
      plot-name
	  (erase-buffer)
	  (goto-char (point-min))
	  (insert result-string)
	  (unless (= n 0)
	    (move-beginning-of-line 1)
	    (search-backward ",")
	    (forward-char)
	    (kill-line)
	    ))
  (expand-file-name plot-name))))))
