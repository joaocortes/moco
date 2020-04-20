(load-file "../bugUtilities.el")
(message "current compilation functions: %s" compilation-finish-functions)
(defun joc-moco-depure-bugginess-definition (buffer)
  (with-current-buffer buffer
    (when (or
	   (re-search-forward "exception" nil t)
	   (re-search-forward "Sending SIGTERM to process tree" nil t)
	   (re-search-forward "compilation exited" nil t))
      (goto-char (point-min))
      (when (re-search-forward "NullPointerException" nil t)
	(setq joc-moco-depure-code 1)
	(write-region nil nil "./lastCompile")))))
