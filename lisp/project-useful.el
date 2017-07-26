(require 'helm)

;; Open any file
;; http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html
(defun open-file (path)
  (let ((open (cond
               ((equal system-type 'cygwin) "cygstart.exe")
               ((equal system-type 'gnu/linux) "xdg-open")
               (t "don't care for now"))))
    (if (file-directory-p path)
	(dired path)
      (call-process-shell-command (concat open " " path)))))

(defmacro project-useful (&rest input-lst)
  (let (projects project-entries)
    (dolist (element input-lst)
      (if (symbolp element)
	  (add-to-list 'projects element)
	(add-to-list 'project-entries (cons (car projects) element))))
    (setq project-useful--projects projects)
    (setq project-useful--project-entries project-entries)))

(macroexpand-1
 '(project-useful algorithms
		 ("root" "~/projects/algorithms/" )
		 ("cmake file" "~/projects/algorithms/CMakeLists.txt")
		 pics
		 ("root" "~/pics/")
		 ("best pic ever" "~/pics/screenshot1488630873.png")))

(defun project-useful--get-project-paths (project)
  "parameter is a string"
  (let (paths)
    (dolist (entry project-useful--project-entries paths)
      ;; use symbol-name because the macro takes symbols as project names.
      (if (equal (symbol-name (car entry)) project)
	  (let ((name-and-path (cdr entry)))
	    (add-to-list 'paths (concat (car name-and-path) ": " (cadr name-and-path))))))
    paths))

(project-useful--get-project-paths "algorithms")

(defun project-useful--list-project-entries (project)
  (helm :sources '((name . "Project entries")
		   (candidates . (lambda ()
				   (project-useful--get-project-paths project)))
		   (action . (lambda (candidate)
			       (let ((candidate-path (cadr (split-string candidate ": "))))
				 (open-file candidate-path)))))))

(defun project-useful-list-projects ()
  (interactive)
  (helm :sources '((name . "Projects")
		   (candidates . project-useful--projects)
		   (action . (lambda (candidate)
			       (project-useful--list-project-entries candidate))))))

(provide 'project-useful)
