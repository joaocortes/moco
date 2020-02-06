(load-file "utilities.el")
(require 'cl)

(setq plot-script-path (expand-file-name "./completeTrajectories.wls "))
(defun joc-total-recomputation ()
  (let ((default-directory default-directory)
	(directories '("../finalResultsSC/" "../finalResultsSPL/")))
    (dolist (directory directories)
      (let* ((default-directory (concat default-directory directory))
	     runs)
	(setq runs (directory-files "." nil "run*"))
	(dolist (run runs)
	  (let  ((default-directory
		   (concat default-directory (file-name-as-directory run) "output/")))
	    (dolist (solver-file (directory-files "." nil "^solver*"))
	      (message (joc-trajectories-time solver-file)) )
	    (dolist (watcher-file (directory-files "." nil "^watcher*"))
	      (message (joc-trajectories-memory watcher-file))))))))
  (shell-command plot-script-path ))


