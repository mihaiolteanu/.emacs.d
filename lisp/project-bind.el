;; Open any file
;; http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html
(defun open-file (path)
  (let ((open (cond
               ((equal system-type 'cygwin) "cygstart.exe")
               ((equal system-type 'gnu/linus) "xdg-open")
               (t "don't care for now"))))
    (call-process-shell-command (concat open " " path))))

(defmacro make-prefix-map (map-name map-key)
  ;; Create a keymap and give it a name that shows up nicely in the which-key buffer.
  ;; Implemented as seen in helm-command-prefix, but evesed.
  (let (ret
        (map-name-prefix (intern (concat (symbol-name map-name) "-prefix"))))
    (push `(global-set-key (kbd ,map-key) ',map-name-prefix) ret)
    (push `(setq  ,map-name-prefix ,map-name) ret)
    (push `(fset ',map-name-prefix ,map-name) ret)
    (push `(define-prefix-command ',map-name-prefix) ret)
    (push `(defvar ,map-name-prefix) ret)
    (push `(defvar ,map-name (make-sparse-keymap)) ret)
    (push 'progn ret)))

(defmacro project-bind (map-name map-key lst)
  (let (ret)
    (dolist (item lst)
      (let* ((fn (car item))
             (fn-name (intern (concat (symbol-name map-name) "-" (symbol-name fn))))
             (key (cadr item))
             (path (cadr (cdr item))))
        (push `(global-set-key (kbd ,(concat map-key " " key)) ',fn-name)
              ret)
        (push `(defun ,fn-name ()
                 (interactive)
                 (open-file ,path))
              ret)))
    (push `(make-prefix-map ,map-name ,map-key) ret)
    (push 'progn ret)))

;; Example usage
(project-bind renault-M0 "C-c q"
              ((root "r" "C:/work/Reanult_M0")
               (dev "e" "C:/work/Reanult_M0/dev/")))

(provide 'project-bind)
