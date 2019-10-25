(setq inhibit-startup-message t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; §§§ Use package configs
;; Don't need to type :ensure t to every use-package instance
(setq use-package-always-ensure t)

(use-package color-theme-sanityinc-tomorrow
  :config
  (load-theme 'sanityinc-tomorrow-eighties :no-confirm))

(use-package diminish
  :config
  (diminish 'auto-revert-mode)
  (diminish 'eldoc-mode))

(defun github-code-search ()
  "Search code on github for a given language."
  (interactive)
  (let ((language (completing-read
                   "Language: "'("Common Lisp" "Emacs Lisp")))
        (code (read-string "Code: ")))
    (browse-url
     (concat "https://github.com/search?l=" language
             "&type=code&q=" code))))

(use-package ivy
  :diminish ivy-mode
  :init (ivy-mode t)
  :config
  (ivy-toggle-case-fold)

  (use-package counsel
  :diminish counsel-mode
  :init (counsel-mode t))

  (use-package smex)          ; show most recently used commands in counsel-M-x
  (use-package counsel-gtags)

  (setq ivy-height 15
        ivy-fixed-height-minibuffer t ; Do not autoresize the minibuffer
        ivy-initial-inputs-alist nil  ; Do not put a ^ in counsel-M-x
        enable-recursive-minibuffers t
        ivy-use-virtual-buffers t)    ; Helm-mini like behaviour

  (defun counsel-find-file-return-name (&optional initial-input)
    "Return the file name path selected by the user. Copy/pasted
     from counsel-find-file but change the action."
  (interactive)
  (ivy-read "Find file: " 'read-file-name-internal
            :matcher #'counsel--find-file-matcher
            :initial-input initial-input
            :action
            (lambda (x)
              x)
            :preselect (when counsel-find-file-at-point
                         (require 'ffap)
                         (let ((f (ffap-guesser)))
                           (when f (expand-file-name f))))
            :require-match 'confirm-after-completion
            :history 'file-name-history
            :keymap counsel-find-file-map
            :caller 'counsel-find-file))

  (defun counsel-do-ag ()
    "Search using ag in a user-selected folder"
    (interactive)
    (counsel-ag nil (counsel-find-file-return-name)))

  (defun counsel-find-file-root ()
    "Find file in project root."
    (interactive)
    (counsel-file-jump nil (vc-root-dir)))

  (defun counsel-switch-to-eshell-buffer ()
    "Switch to a eshell buffer, or create one. Copy/pasted from
     counsel.el and modified the shell-mode to eshell-mode"
    (interactive)
    (other-window 1)
    (ivy-read "Switch to shell buffer: "
              (counsel-list-buffers-with-mode 'eshell-mode)
              :action #'counsel-switch-to-buffer-or-window
              :caller 'counsel-switch-to-shell-buffer))

  ;; defun counsel-ag (&optional initial-input initial-directory extra-ag-args ag-prompt)
  (defun counsel-ag-root (&optional args)
    "Ag from project root."
    (interactive)
    (counsel-ag nil (vc-root-dir) args))

  (defun counsel-gtags-find-reference-at-point ()
    "By default, counsel-gtags-find-reference takes the symbol at
    point but still keeps the prompt that asks for a symbol
    open. On large projects, this takes a lot of time to load and
    in the meantinme, the promp is basically blocked. So call
    this function with the symbol at point in the first place, so
    no promp for symbol completion is displayed."
    (interactive)
    (let ((symbol-at-point (thing-at-point 'symbol)))
      (if symbol-at-point
          (counsel-gtags-find-reference symbol-at-point)
        ;; Bring the promp up if nothing is under the point
        (call-interactively 'counsel-gtags-find-reference))))

  (defun swiper-isearch-string ()
    (interactive)
    (swiper isearch-string))

  (add-hook 'c-mode-common-hook
            '(lambda ()
	       (local-set-key (kbd "M-.") 'counsel-gtags-dwim)
	       (local-set-key (kbd "M-,") 'counsel-gtags-go-backward)
	       (local-set-key (kbd "C-M-.") 'counsel-gtags-find-reference-at-point)
               ;; C-c C-e is used by something else by default in cc-mode
	       (local-set-key (kbd "C-c C-e") 'counsel-switch-to-eshell-buffer)
               ))

  (ivy-set-actions
   'counsel-M-x
   '(("d" (lambda (f) (describe-function (intern f))) "describe")
     ))

  (defun describe-function-from-ivy ()
    (interactive)
    (ivy-exit-with-action (lambda (s)
                            (describe-function (intern s))
                            (ivy-resume))))

  :bind (("M-y" . counsel-yank-pop)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-r" . counsel-find-file-root)
         ("C-c s" . counsel-ag-root)
         ("C-c S" . counsel-do-ag)
         ("C-c C-e" . counsel-switch-to-eshell-buffer)
         ("C-c C-r" . ivy-resume)
         ("C-z" . ivy-switch-buffer)
         ("C-c i" . counsel-semantic)
         ("C-S-s" . swiper)
         :map counsel-find-file-map
         ("C-l" . counsel-up-directory)
         ("C-j" . ivy-alt-done)
         :map ivy-minibuffer-map
         ("C-i" . ivy-insert-current)
         ("C-j" . ivy-call)
         ("C-w" . ivy-yank-word)
         ("C-o" . ivy-dispatching-done) ; Select actions
         :map isearch-mode-map
         ("C-o" . swiper-isearch-string)
         :map counsel-describe-map
         ("C-q" . describe-function-from-ivy)
         ))

(use-package eshell
  :init
  (setq eshell-hist-ignoredups t
        eshell-save-history-on-exit t)
  :config
  (defun eshell/x ()
    (interactive)
    (insert "exit")
    (eshell-send-input)
    (other-window 1))
  
  (use-package eshell-fringe-status)
  (use-package eshell-fixed-prompt)
  (add-hook 'eshell-mode-hook
	    (lambda ()
	      (eshell-cmpl-initialize)
              (smartscan-mode -1)
              (setenv "GIT_PAGER" "")   ; Make git usable
              (eshell-fixed-prompt-mode)
              (eshell-fringe-status-mode)
              ;; "eshell-mode-map not available" error, if the keys below are put on :bind :map
              (define-key eshell-mode-map [remap eshell-pcomplete] 'completion-at-point)
              (define-key eshell-mode-map (kbd "M-r") 'counsel-esh-history)
              (define-key eshell-mode-map (kbd "M-p") 'eshell-previous-input)
              (define-key eshell-mode-map (kbd "M-n") 'eshell-next-input)
              (define-key eshell-mode-map (kbd "C-g") 'eshell/x)
              (eshell/alias "l" "ls -lah")))

  (defun eshell-here ()
    "Opens up a new shell in the directory associated with the
     current buffer's file. The eshell is renamed to match that
     directory to make multiple eshell windows easier."
    (interactive)
    (let* ((parent (if (buffer-file-name)
                       (file-name-directory (buffer-file-name))
                     default-directory))
           (name   (car (last (split-string parent "/" t))))
           (name   (concat "*" parent " - eshell*")))
      ;; Create a new eshell buffer if one doesn't exist and switch to it
      (if (get-buffer name)
          (switch-to-buffer-other-window (get-buffer name))
        (progn
          (switch-to-buffer-other-window (current-buffer))
          (eshell "new")
          (rename-buffer name)
          (insert (concat "l"))
          (eshell-send-input)))))
  
  :bind (("C-c e" . eshell-here)))

(use-package magit
  :bind (("C-c m l" . magit-log)
	 ("C-c m b" . magit-blame)))

(use-package git-timemachine
  :bind (("C-c m t" . git-timemachine)))

(use-package git-gutter+
  :diminish
  :init
  (if (not (fboundp 'git-commit-mode-font-lock-keywords))
      ;; This function seems missing or something is wrong with it
      ;; commit will not work without first defining it, so here goes:
      (defun git-commit-mode-font-lock-keywords ()
        git-commit-font-lock-keywords-2))
  :config
  (global-git-gutter+-mode +1)
  (set-face-foreground 'git-gutter+-added "olive drab")
  (set-face-foreground 'git-gutter+-deleted "firebrick")
  (set-face-foreground 'git-gutter+-modified "dark magenta")
  :bind
  (("C-x v n" . git-gutter+-next-hunk)
   ("C-x v p" . git-gutter+-previous-hunk)
   ("C-x v C-s" . git-gutter+-show-hunk)
   ("C-x v s" . git-gutter+-stage-hunks)
   ("C-x v u" . git-gutter+-unstage-whole-buffer)
   ("C-x v c" . git-gutter+-commit)
   ("C-x v C-c" . git-gutter+-stage-and-commit)
   ))

(use-package beacon
  :diminish
  :config
  (beacon-mode +1))

(use-package which-key
  :diminish
  :config
  (which-key-mode +1))

(use-package cc-mode
  :config
  (add-hook 'c-mode-common-hook
	    '(lambda ()
	       (setq c-default-style "k&r"
		     c-basic-offset 4)
	       (setq-default fill-column 80)
	       ;; Don't ask for a compile command every time, useful if compile-command
	       ;; is taken from .dir-locals.el, for example.
	       ;; Example for setting the compile-command in .dir-locals.el:
	       ;;
	       ;; ((nil . ((eval . (set (make-local-variable 'compile-command)
	       ;; 			(concat "make -f "
	       ;; 				(file-name-directory
	       ;; 				 (let ((d (dir-locals-find-file ".")))
	       ;; 				   (if (stringp d) d (car d))))
	       ;; 				"makefile"))))))
               (local-set-key (kbd "C-c C-c") 'compile))))

(use-package disaster)			; Disassemble C/C++ code under cursor

(use-package crux
  :config
  :bind (("C-c d" . crux-duplicate-current-line-or-region)
	 ("C-a" . crux-move-beginning-of-line)))

(use-package smartscan
  :diminish
  :config
  (global-smartscan-mode 1))

(use-package wrap-region
  :diminish
  :config
  (wrap-region-mode 1))

(use-package smartparens
  :diminish
  :config
  (require 'smartparens-config)
  (use-package crux)
  (setq sp-base-key-bindings 'paredit
	sp-autoskip-closing-pair 'always
	sp-hybrid-kill-entire-symbol nil)
  (sp-use-paredit-bindings)
  (smartparens-global-mode +1)
  (show-smartparens-global-mode +1)
  (smartparens-global-strict-mode +1)
  (sp-pair "{" nil :post-handlers ; smart curly braces
	   '(((lambda (&rest _ignored)
		(crux-smart-open-line-above)
		(indent-according-to-mode)) "RET")))
  (defun sp-kill-region-or-backward-word ()
    "If the region is active and non-empty, kill the region
    otherwise kill the word as in bash (from stackoverflow)"
    (interactive)
    (call-interactively
     (if (use-region-p) 'sp-kill-region 'sp-backward-kill-word)))
  :bind ("C-w" . sp-kill-region-or-backward-word))

(use-package geiser
  :bind (:map geiser-mode-map
              ("C-." . next-buffer)))

(use-package cmake-mode
  :mode (("\\.cmake\\'" . cmake-mode)
	 ("CMakeLists.txt" . cmake-mode))
  :config
  (use-package cmake-font-lock)
  :init
  (add-hook 'cmake-mode-hook
	    '(lambda ()
	       (set (make-local-variable 'company-backends)
		    '((company-cmake company-yasnippet)))
	       (cmake-font-lock-activate))))

(use-package company
  :diminish
  :config
  (global-company-mode +1)
  ;; (use-package company-quickhelp)
  :bind (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("C-j" . company-show-doc-buffer)))

(use-package avy
  :config
  (setq avy-keys '(?\; ?l ?k ?j ?h ?u ?i ?o ?p ?m))
  :bind ("C-;" . avy-goto-symbol-1))

(use-package org
  :init
  (setq org-startup-indented t)
  :config
  (use-package org-bullets)
  (add-hook 'org-mode-hook
            (lambda ()
              ;; Emacs is very slow with org-bullets-mode on without this inhibiting
              (setq inhibit-compacting-font-caches t)
              (org-bullets-mode 1)))
  ;; Make headers same size and bold
  (mapc (lambda (level)
          (set-face-attribute level nil :height 1.3 :weight 'bold))
        '(org-level-1 org-level-2 org-level-3 org-level-4 org-level-5))
  ;; source code in org files
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-ask-before-returning-to-edit-buffer nil
        org-confirm-babel-evaluate nil) ; Just C-c C-c to evaluate src block, no questions asked
  (setq org-babel-lisp-eval-fn 'sly-eval) ; Use sly instead of the default slime
  ; Force bind these keys, as they are overwriten otherwise by the org-mode
  :bind (:map org-mode-map
              ("C-," . previous-buffer)
              ("<C-tab>" . other-window)))

(use-package openwith
  :diminish
  :config
  (openwith-mode +1)
  (let (open-app)
    (setq open-app (cond
                    ((eq system-type 'cygwin) "cygstart")
                    (t "")))
    (setq openwith-associations `(("\\.pdf\\|\\.png\\|\\.docm\\|\\.xls" ,open-app (file))))))

(use-package dired
  :ensure nil
  :config
  (use-package dired-x
    :ensure nil
    :hook (dired-mode . dired-omit-mode))
  (use-package dired-subtree
    :ensure t)
  (setq dired-listing-switches "-lah")
  (put 'dired-find-alternate-file 'disabled nil) ;enable 'a' command in dired
  (custom-set-faces
   '(dired-directory ((t (:foreground "DodgerBlue1" :weight bold))))
   '(dired-marked ((t (:foreground "orange red" :weight extra-bold)))))
  :hook (dired-mode . dired-hide-details-mode)
  :bind (:map dired-mode-map
              ("C-l" . 'dired-up-directory)              
              ("I" . 'dired-kill-subdir)
              ("<tab>" . 'dired-hide-subdir)
              ("C-i" . 'dired-subtree-toggle)
              ("q" . kill-this-buffer)))

(use-package ivy-dired-history :after (dired savehist)
  :init
  (add-to-list 'savehist-additional-variables
               'ivy-dired-history-variable))

(use-package smart-mode-line
  :config
  (setq sml/theme 'respectful)
  (setq sml/no-confirm-load-theme t)
  ;; Get rid of loading lisp code warning on emacs startup
  (add-hook 'after-init-hook 'sml/setup))

(use-package sly
  :config
  (setq inferior-lisp-program "/usr/bin/sbcl")

  ;; Define functionality for interacting with the sly repl using counsel 
  (defun counsel-sly-mrepl-shortcut ()
    (interactive)
    (ivy-read
     "Action: " 
     (mapcar #'car sly-mrepl-shortcut-alist)
     :action (lambda (string)
               (let ((command (and string
                                   (cdr (assoc string sly-mrepl-shortcut-alist)))))
                 (call-interactively command)))))

  (defun sly-read-package-name (prompt &optional initial-value allow-blank)
    (ivy-read
     "Package: "
     (sly-eval `(slynk:list-all-package-names t))
     :action #'identity))

  (defun counsel-sly-mrepl-history ()
    (interactive)
    (ivy-read
     "History: "
     (ring-elements comint-input-ring)
     :action (lambda (e)
               (insert e))))

  (defun eval-grab-output (string)
    (let ((res nil))
      (sly-eval-async `(slynk:eval-and-grab-output ,string)
        (lambda (result)
          (cl-destructuring-bind (output value) result
            (setf res (car (read-from-string value))))))
      (while (null res)
        (sleep-for 0.1))
      res))

  (defun counsel-sly-eval (string action)
    (let ((result (eval-grab-output string)))
      (ivy-read
       "Symbol: "
       result
       :action action)))

  (defun send-input (expr)
    (insert expr)
    (comint-send-input))

  (defun counsel-sly-package-internal-symbols ()
    (interactive)
    (counsel-sly-eval "(common-lisp-user::package-internal-symbols \*package\*)"
                      `(1 ("o" ,#'insert "insert")
                          ("f" ,(lambda (candidate)
                                  (send-input (format "(find-symbol \"%s\")" candidate)))
                           "find symbol")
                          )))

  ;; sly-mrepl-mode-map symbol not available at the time of
  ;; use-package, so :bind cannot be used here
  (with-eval-after-load 'sly-mrepl
    (define-key sly-mrepl-mode-map [remap isearch-backward] 'counsel-sly-mrepl-history)
    (define-key sly-mrepl-mode-map (kbd ",,") 'counsel-sly-mrepl-shortcut)
    (define-key sly-mrepl-mode-map (kbd ",p") 'sly-mrepl-set-package)
    (define-key sly-mrepl-mode-map (kbd ",s") 'counsel-sly-package-internal-symbols)
    (define-key sly-mrepl-mode-map (kbd ",r") 'sly-restart-inferior-lisp))
  )

;; Open compilation results in a bottom buffer, similar to helm, identical code.
(setq compilation-read-command nil) 
(add-to-list 'display-buffer-alist
	     `(,(rx bos "*compilation" (* not-newline) "*" eos)
	       (display-buffer-in-side-window)
	       (inhibit-same-window . t)
	       (window-height . 0.3)))
;; After compilation, go to the compilation buffer (q key to close it)
(add-hook
 'compilation-finish-functions (lambda (buffer result)
				 (switch-to-buffer-other-window "*compilation*")))
(global-set-key (kbd "C-c C-c") 'compile)

(set-fringe-mode 0)

;; §§§ Other stuff

;; Kill the current buffer instantly
(global-set-key (kbd "C-x k")
                (lambda ()
                  (interactive)
                  (kill-buffer (current-buffer))))

;; https://www.emacswiki.org/emacs/EshellEnhancedLS
(eval-after-load "em-ls"
    '(progn
       (defun ted-eshell-ls-find-file-at-point (point)
         "RET on Eshell's `ls' output to open files."
         (interactive "d")
         (find-file (buffer-substring-no-properties
                     (previous-single-property-change point 'help-echo)
                     (next-single-property-change point 'help-echo))))

       (defadvice eshell-ls-decorated-name (after ted-electrify-ls activate)
         "Eshell's `ls' now lets you click or RET on file names to open them."
         (add-text-properties 0 (length ad-return-value)
                              (list 'help-echo "RET, mouse-2: visit this file"
                                    'mouse-face 'highlight
                                    'keymap ted-eshell-ls-keymap)
                              ad-return-value)
         ad-return-value)
       
       (let ((map (make-sparse-keymap)))
         (define-key map (kbd "RET")      'ted-eshell-ls-find-file-at-point)
         (define-key map (kbd "<return>") 'ted-eshell-ls-find-file-at-point)
         (defvar ted-eshell-ls-keymap map))))

;; Disable bars and the blinking cursor
(mapc (lambda (mode)
        (when (fboundp mode)
          (funcall mode -1)))
      '(menu-bar-mode tool-bar-mode scroll-bar-mode blink-cursor-mode))

;; highlight the current line
(global-hl-line-mode +1)

;; Use spaces instead of tabs
(setq-default indent-tabs-mode nil)

(desktop-save-mode 1)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default fill-column 80)
(setq enable-local-eval t               ;enable dir local variables
      debugger-stack-frame-as-list t    ;show calls as lists in the debugger
      )

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 120
                    :weight 'normal
                    :width 'normal)

;; General navigation keys
(defun revert-buffer-no-confirm ()
  (interactive) (revert-buffer t t))

(global-set-key (kbd "<C-tab>") 'other-window)
(global-set-key (kbd "<C-M-tab>") 'windmove-left)
(global-set-key (kbd "<C-S-iso-lefttab>") 'outline-toggle-children)
(global-set-key (kbd "C-,") 'previous-buffer)
(global-set-key (kbd "C-.") 'next-buffer)
(global-set-key (kbd "C-q") 'execute-extended-command)
(global-set-key (kbd "C-S-r") 'revert-buffer-no-confirm)
(define-key comint-mode-map (kbd "C-n") #'comint-next-input)
(define-key comint-mode-map (kbd "C-p") #'comint-previous-input)
(define-key comint-mode-map (kbd "C-r") #'comint-history-isearch-backward)
(setf comint-prompt-read-only t
      comint-history-isearch t)

(defun mydir ()
  "Can be used in dired buffer to search files recursively.
If point is on a folder, search in that folder, otherwise, search
in the current folder dired is opened in."
  (interactive)
  (ivy-read "Blaaa: "
            (directory-files-recursively
             (if (and (derived-mode-p 'dired-mode)
                      (file-directory-p (dired-get-filename)))
                 (dired-get-filename)
               default-directory)
             "")
            :action #'find-file))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((lisp . t)))

(defun my-html-filter-src-blocks (text backend info)
  "Remove source blocks from html export."
  (when (org-export-derived-backend-p backend 'html)
    "[removed source block]"))

(setq org-src-fontify-natively t
    org-src-tab-acts-natively t
    org-confirm-babel-evaluate nil
    org-edit-src-content-indentation 0)

(add-to-list 'org-export-filter-src-block-functions
             'my-html-filter-src-blocks)


