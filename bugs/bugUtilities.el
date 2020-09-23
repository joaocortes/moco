(require 'cl)

;;; Run all bugs
(defun joc-moco-test-register-results (buffer desc)
  (with-current-buffer  joc-moco-test-buffer (insert (concat desc "\n")))
  (setq minimal (pop joc-moco-test-bugs))
  (if (not minimal)
      (remove-hook 'compilation-finish-functions 'joc-moco-test-register-results)
    (with-current-buffer  joc-moco-test-buffer (insert (concat minimal "\n")))
    (compile
     (concat
      "java -jar  ./target/org.sat4j.moco.threeAlgorithms-0.0.1-SNAPSHOT-jar-with-dependencies.jar "
      minimal
      " -alg 1"))))
(defun joc-moco-test-run-bugs-starter ()
  (interactive)
  (setq joc-moco-test-bugs  (directory-files-recursively default-directory "minimal.opb$"))
  (setq joc-moco-test-buffer (get-buffer-create "*joc-moco-test*"))
  (add-hook 'compilation-finish-functions 'joc-moco-test-register-results)
  (projectile--run-project-cmd projectile-project-compilation-cmd projectile-compilation-cmd-map
			       :show-prompt nil
			       :prompt-prefix "Run command: "))


;;; depure minimal

(defvar joc-moco-depure-buffer nil)
(defvar joc-moco-depure-code 0)
(defvar joc-moco-depure-last-kill nil)
(defvar joc-moco-depure-size-block 1)
(defvar joc-moco-depure-size-block-max 4)

(defvar joc-moco-depure-run-command ""
  )
(defun joc-moco-depure-bugginess-definition (buffer)
  (with-current-buffer buffer
    (when (or
	   (re-search-forward "exception" nil t)
	   (re-search-forward "compilation exited" nil t))
      (goto-char (point-min))
      (when (re-search-forward "NullPointerException" nil t)
	(setq joc-moco-depure-code 1)
	(write-region nil nil "./lastCompile")))))


(defun* joc-depure-moco-buggy-instance (buffer desc)
  (shell-command "date")
  (message (concat "depure: Did " joc-moco-depure-last-kill "break the example?"))


  ;; Find out if the last compilation broke the example. Set
  ;; joc-moco-depure-code accordingly: 1 if it is intact, 0 if it is broken.
  ;; 
  (when  joc-moco-depure-last-kill
    (if (joc-moco-depure-bugginess-definition buffer)
	(message "bug is intact")
      (message "bug was broken. Going back"))
    (with-current-buffer joc-moco-depure-buffer
      (if (= joc-moco-depure-code 0)
	  (insert joc-moco-depure-last-kill)
	(message "new minimal_certain. Current point at %d." (point))
	(write-region nil nil "./minimal_certain.opb" ))))

  ;; Start to weave of a new kill. 
  (with-current-buffer joc-moco-depure-buffer
    (setq joc-moco-depure-last-kill nil)
    ;; start weaving the next kill
    (when  (re-search-forward " [-+][0-9]* x[0-9]*" nil  t)
      (setq joc-moco-depure-last-kill (match-string 0))
      (delete-region (match-beginning 0)(match-end 0))))

  ;; if the kill is empty, incf the size of the block. If it is
  ;; saturated, end, otherwise start over from point-min.
  (unless joc-moco-depure-last-kill
    (unless (> (decf joc-moco-depure-size-block-max)
	       0)
      (remove-hook 'joc-depure-moco-buggy-instance 'compilation-finish-functions)
      (return-from joc-depure-moco-buggy-instance))
    (with-current-buffer joc-moco-depure-buffer
      (goto-char (point-min)))
    (setq joc-moco-depure-code 1)
    (setq joc-moco-depure-last-kill nil)
    (joc-depure-moco-buggy-instance buffer desc)
    (return-from joc-depure-moco-buggy-instance ))

  (with-current-buffer joc-moco-depure-buffer
    (let ((index joc-moco-depure-size-block-max) (bound))
      (setq bound (save-excursion     
		    (move-end-of-line nil)
		    (point)))
      (while (and (> (decf index) 0)  (re-search-forward " [-+][0-9]* x[0-9]*" bound  t))
	(setq joc-moco-depure-last-kill (concat joc-moco-depure-last-kill (match-string 0)))
	(delete-region (match-beginning 0)(match-end 0))
	(setq bound (save-excursion     
		      (move-end-of-line nil)
		      (point)))))

    (setq joc-moco-depure-code 0)
    (let ((compilation-ask-about-save nil)) (recompile))))

(defun joc-depure-moco-first-run (buffer desc)
  (remove-hook 'compilation-finish-functions  'joc-depure-moco-first-run)
  (add-hook 'compilation-finish-functions  'joc-depure-moco-comment)
  (compile joc-moco-depure-run-command))

;; (defun joc-depure-moco-first-run (buffer desc)
;;   (remove-hook 'compilation-finish-functions  'joc-depure-moco-first-run)
;;   (joc-depure-moco-buggy-instance (get-buffer "*compilation*") "")
;;   (add-hook 'compilation-finish-functions  'joc-depure-moco-buggy-instance))

(defun joc-depure-moco-starter (&optional arg)
  (interactive "P")
  (setq joc-moco-depure-buffer (current-buffer))
  (setq joc-moco-depure-last-kill nil)
  (setq joc-moco-depure-code 1)
  (setq joc-moco-depure-size-block-max 6)
  (setq joc-moco-depure-run-command
	(concat
	 "../../runsolver"
	 " -W "
	 " 300 "
	 "-M "
	 " 10000000 "
	 "java -jar "
	 " ../../target/org.sat4j.moco.threeAlgorithms-0.0.1-SNAPSHOT-jar-with-dependencies.jar "
	 (file-name-nondirectory (buffer-file-name joc-moco-depure-buffer))
	 " -alg 1"))

  (let ((confirmation t) first-compile )
    (when compilation-finish-functions
      (setq confirmation (string-equal
			  (read-string
			   (format
			    "%s might interfere. Continue?"
			    compilation-finish-functions)) "yes")))
    (when confirmation
      (setq first-compile joc-moco-depure-run-command)

      (when arg (when (string-equal (read-string "recompile module?" ) "y")
		  (setq first-compile (concat  "mvn -DskipTests=true package; " first-compile) )))
      (add-hook 'compilation-finish-functions  'joc-depure-moco-first-run)
      (compile first-compile))))

(defun joc-depure-moco-comment (buffer desc)
  (shell-command "date")
  (message (concat "depure: Did " joc-moco-depure-last-kill "break the example?"))

  (if (joc-moco-depure-bugginess-definition buffer)
      (message "bug is intact")
    (message "bug was broken. Going back"))

  (with-current-buffer joc-moco-depure-buffer
    (if (= joc-moco-depure-code 0)
	(progn	(re-search-forward "\*+ " nil t)
		(delete-region (match-beginning 0)
			       (match-end 0)))
      (message "new minimal_certain. Current point at %d." (point))
      (write-region nil nil "./minimal_certain.opb" ))
    (move-beginning-of-line 2)
    (insert "* ")
    (move-beginning-of-line nil)
    (setq joc-moco-depure-code 0)
    (if  (= (point) (save-excursion (move-beginning-of-line 2)))
	(progn
	  (remove-hook 'compilation-finish-functions 'joc-depure-moco-comment)
	  (with-current-buffer joc-moco-depure-buffer
	    (goto-char (point-min))
	    (re-search-forward "min:" nil t)
	    (move-beginning-of-line 1)
	    (flush-lines "^*" ))
	  (add-hook 'compilation-finish-functions 'joc-depure-moco-buggy-instance)
	  (joc-depure-moco-buggy-instance (get-buffer "*compilation*") ""))
      (let ((compilation-ask-about-save nil)) (recompile)))))

