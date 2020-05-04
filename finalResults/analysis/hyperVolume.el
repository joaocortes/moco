(require 'cl)
(defvar joc-moco-analyzer-buffer nil )
(setq joc-moco-analyzer-buffer "*joc moco analyzer output*" )

(defvar joc-moco-analyzer-jar
  (concat "java -cp"
	  " ./target/org.sat4j.moco.threeAlgorithms"
	  "-0.0.1-SNAPSHOT-jar-with-dependencies.jar"
	  " org.sat4j.moco.analysis.Analyzer "))
(defvar joc-moco-analyzer-verbosity 3)
(require 'projectile)
(setq joc-moco-analyzer-verbosity 3 )
(defvar joc-moco-root-folder  (projectile-project-root))
(setq joc-moco-root-folder  (projectile-project-root))

(defun joc-clean-solver-output (file)
  "Removes the time marks run solver prints "
  (setq file-clean (concat (file-name-sans-extension (expand-file-name file)) "_clean.out"))
  (save-excursion
    (with-temp-file file-clean
      (print (current-buffer))
      (insert-file-contents file)
      (while (not (equalp (point) (point-max)))
	(move-beginning-of-line nil)
	(mark-sexp 4 t)
	(exchange-point-and-mark)
	(backward-char)
	(delete-region (point) (mark))
	(deactivate-mark)
	(next-line)
	(move-end-of-line nil))
      ))
  (file-exists-p file-clean))

(defun joc-moco-analyzer-hyperVolume (&optional solver-file)
  "Calculate the hypervolume of the solution in solver-file "
  (interactive)
  (unless solver-file (setq solver-file (buffer-file-name (current-buffer))))
  (let (clean-file instance-file id )
    (with-current-buffer (get-buffer-create joc-moco-analyzer-buffer) (insert (concat "working on " solver-file "\n" )))
    (setq id (joc-moco-get-id-from-output solver-file))
    (setq clean-file (concat (file-name-sans-extension solver-file) "_clean.out"))
    (condition-case nil (delete-file clean-file)
      (error (message "clean file %s empty, no need to erase" clean-file)))
    (joc-clean-solver-output solver-file)
    (unless (file-exists-p (expand-file-name clean-file)) (error "clean file %s not created" clean-file ))
    (setq instance-file (joc-moco-get-instance-from-output solver-file))
    (let ((shell-command-dont-erase-buffer t) hyper-volume (default-directory joc-moco-root-folder) command)
      (setq command
	    (concat
	     joc-moco-analyzer-jar
	     instance-file 
	     " 1:" clean-file
	     " -v "
	     (number-to-string joc-moco-analyzer-verbosity)
	     ))
      (with-current-buffer (get-buffer-create joc-moco-analyzer-buffer)
	(insert command)
	(insert "\n"))
      (shell-command
       command
       (get-buffer-create joc-moco-analyzer-buffer))
      (condition-case nil
	  (with-current-buffer joc-moco-analyzer-buffer  
	    (goto-char (point-max))
	    (re-search-backward ":values \\[\\(.*\\)\\] :min")
	    (setq hyper-volume (match-string 1 ))
	    (goto-char (point-max)))
	(error (setq hyper-volume "-1.0") nil))
      hyper-volume
      )))

(defun joc-moco-calculate-hypervolumes ()
"calculate all hypervolumes of valid files reachable from the
parent directory "
  (interactive)
  (dolist (output     (cl-remove-if-not #'file-directory-p (directory-files-recursively "../" "output$" t)))
    (append-to-file (concat output "\n") nil "./hyperVolume.txt" )
    (dolist (solver-file (cl-remove-if (lambda (string) (string-match "clean" string))
				       (directory-files output t "^solver*")))
    (let (solver-file-clean hyper-volume)
	(setq hyper-volume (joc-moco-analyzer-hyperVolume solver-file))
	(let (id alg)
	  (string-match "\_S\\([0-2]+\\)\\.[a-z]*" solver-file)
	  (setq alg (match-string 1 solver-file))
	  (setq id (joc-moco-get-id-from-output solver-file))
	  (append-to-file  (concat alg ", " hyper-volume  ", " id "\n") nil  "./hyperVolume.txt"))
	))))

(defun joc-moco-get-id-from-output (&optional solver-file)
  "get the id from the name of solver output, solver-file"
  (interactive)
  (unless solver-file (setq solver-file (buffer-file-name (current-buffer))))
   (let (id)
     (condition-case nil			
	 (if (string-match "solver_\\(.*\\)_S[0-9]\\.txt" solver-file)
	     (setq id  (match-string 1 solver-file))
	   (error  "%s  cannot be used like this " solver-file ))
       (error nil))
     id))




(defun joc-moco-get-instance-from-output (&optional solver-file)
  "get the name of the instance from the name of solver output,
solver-file"
  (interactive)
  (unless solver-file (setq solver-file (buffer-file-name (current-buffer))))
  (condition-case nil 
      (let (instance-file id)
	(unless (setq id (joc-moco-get-id-from-output solver-file))
	  (error  "%s  cannot be used like this " solver-file ))
	(setq instance-file (expand-file-name (concat
					       (file-name-directory solver-file)
					       "../instances/"
					       id ".pbmo")))
	(unless (file-exists-p instance-file)
	  (error "Instance %s does not exist" instance-file) )
	instance-file)
    (error nil)))

