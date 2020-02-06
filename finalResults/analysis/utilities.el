(defun joc-trajectories-time (&optional file)
  (interactive "P")
  (save-current-buffer
    (let ((file (or file (buffer-file-name))) start result n)
      (setq n 0)
      (with-temp-buffer
	(insert-file-contents-literally file)
	(assert (string-match "solver" file))
		      (goto-char (point-min))
		      ;; (occur "[0-9]*\.[0-9]*/[0-9]*\.[0-9]*[[:blank:]]o")
		      

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
			  )))
      (let ((plot-name
	     (replace-regexp-in-string
	      "solver_" "timePlot_"
	      file) ))
	(if (file-exists-p plot-name) (delete-file plot-name))
	(with-temp-file 
	    plot-name
	  (goto-char (point-min))
	  (insert result)
	  (unless (= n 0)
	    (search-backward ",")
	    (kill-line)
	    ))
	(if  (get-buffer plot-name) (message " esta aberto.." ) )
	(expand-file-name plot-name)))))

(defun joc-trajectories-memory (&optional file)
  (interactive "P")
  (save-current-buffer 
    (setq file (or file (buffer-file-name)))
        ;; (occur "[0-9]*\.[0-9]*/[0-9]*\.[0-9]*[[:blank:]]o")
    (let ( start result n)
      (setq n 0)
      (setq result nil )
      (with-temp-buffer 
	(insert-file-contents-literally file)
	(goto-char (point-min))
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
	    (setq memory (format "%02.2f" (/ (string-to-number (current-kill 0)) 1000000.0)))
	    (setq result (append result (list `(,time . ,memory))))
	    (incf n))))
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
	  (if (file-exists-p plot-name) (delete-file plot-name))
	  (with-temp-file 
	      plot-name
	    (goto-char (point-min))
	    (insert result-string)
	    (unless (= n 0)
	      (move-beginning-of-line 1)
	      (search-backward ",")
	      (forward-char)
	      (kill-line)
	      ))
	  (if  (get-buffer plot-name) (print " esta aberto.." ))
	  (expand-file-name plot-name))))))
