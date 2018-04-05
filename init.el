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

(use-package ivy
  :diminish ivy-mode
  :init (ivy-mode t)
  :config

  (use-package counsel
  :diminish counsel-mode
  :init (counsel-mode t))

  (setq ivy-height 15
        ivy-fixed-height-minibuffer t ; Do not autoresize the minibuffer
        ivy-auto-select-single-candidate t
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
    (ivy-read "Switch to shell buffer: "
              (counsel-list-buffers-with-mode 'eshell-mode)
              :action #'counsel-switch-to-buffer-or-window
              :caller 'counsel-switch-to-shell-buffer))

  ;; defun counsel-ag (&optional initial-input initial-directory extra-ag-args ag-prompt)
  (defun counsel-ag-root ()
    "Ag from project root."
    (interactive)
    (counsel-ag nil (vc-root-dir)))

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
  :config
  (add-hook 'eshell-mode-hook
	    (lambda ()
	      (eshell-cmpl-initialize)
              (smartscan-mode -1)
              (setenv "GIT_PAGER" "")   ; Make git usable
              ;; "eshell-mode-map not available" error, if the keys below are put on :bind :map
              (define-key eshell-mode-map [remap eshell-pcomplete] 'completion-at-point)
              (define-key eshell-mode-map (kbd "M-r") 'counsel-esh-history)
              (define-key eshell-mode-map (kbd "M-p") 'eshell-previous-input)
              (define-key eshell-mode-map (kbd "M-n") 'eshell-next-input)))

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
          (switch-to-buffer (get-buffer name))
        (progn
          (eshell "new")
          (rename-buffer name)))))
  
  :bind (("C-c e" . eshell-here)))

(use-package magit
  :bind (("C-c m l" . magit-log)
	 ("C-c m b" . magit-blame)))

(use-package git-timemachine
  :bind (("C-c m t" . git-timemachine)))

(use-package git-gutter+
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
  :config
  (beacon-mode +1))

(use-package which-key
  :config
  (which-key-mode +1))

(use-package cc-mode
  :config
  (add-hook 'c-mode-common-hook
	    '(lambda ()
	       (setq c-default-style "k&r"
		     c-basic-offset 4)
	       (setq-default fill-column 95)
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
	       (setq compilation-read-command nil) 
	       ;; Open compilation results in a bottom buffer, similar to helm, identical code.
	       (add-to-list 'display-buffer-alist
			    `(,(rx bos "*compilation" (* not-newline) "*" eos)
			      (display-buffer-in-side-window)
			      (inhibit-same-window . t)
			      (window-height . 0.3)))
	       ;; After compilation, go to the compilation buffer (q key to close it)
	       (add-hook
		'compilation-finish-functions (lambda (buffer result)
						(switch-to-buffer-other-window "*compilation*")))
	       (local-set-key (kbd "C-c C-c") 'compile)
	       (which-function-mode +1))))

(use-package disaster)			; Disassemble C/C++ code under cursor

(use-package crux
  :config
  :bind (("C-c d" . crux-duplicate-current-line-or-region)
	 ("C-a" . crux-move-beginning-of-line)))

(use-package smartscan
  :diminish global-smartscan-mode
  :config
  (global-smartscan-mode 1))

(use-package wrap-region
  :ensure t
  :diminish wrap-region-mode
  :config
  (wrap-region-mode 1))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
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

(use-package cmake-mode
  :ensure t
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
  :ensure t
  :diminish company-mode
  :config (global-company-mode +1)
  :bind (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)))

(use-package avy
  :config
  (setq avy-keys '(?\; ?l ?k ?j ?h ?u ?i ?o ?p ?m))
  :bind ("C-;" . avy-goto-symbol-1))

(use-package dired+
  :ensure t
  :init
  (setq dired-guess-shell-alist-user
	'(("\\.pdf\\'" "zathura")
	  ("\\.png\\'\\|\\.jpe?g\\'" "feh --scale-down --auto-rotate -B black *")))
  (load "dired+")
  (add-hook 'dired-mode-hook
	    (lambda ()
	      (local-set-key (kbd "C-l") 'dired-up-directory)))
  :bind ("C-!" . dired-do-shell-command) ;same as !, but easier to type
  )

(use-package org
  ; Force bind these keys, as they are overwriten otherwise by the org-mode
  :bind (:map org-mode-map
              ("C-," . previous-buffer)
              ("<C-tab>" . other-window)
              ))

(use-package openwith
  :ensure t
  :diminish openwith-mode
  :config
  (openwith-mode +1)
  (let (open-app)
    (setq open-app (cond
                    ((eq system-type 'cygwin) "cygstart")
                    (t "")))
    (setq openwith-associations `(("\\.pdf\\|\\.png\\|\\.exe\\|\\.docm\\|\\.xls" ,open-app (file))))))

(use-package dired-ranger
  :ensure t
  :bind (:map dired-mode-map
	      ("W" . dired-ranger-copy)
	      ("X" . dired-ranger-move)
	      ("Y" . dired-ranger-paste)))

(use-package buffer-sections
  :ensure nil
  :bind (("C-x ]" . forward-section)
	 ("C-x [" . backward-section)
	 ("C-x C-]" . insert-section)
	 ("C-x C-[" . search-sections)))

;; §§§ Other stuff
;; https://www.emacswiki.org/emacs/EshellEnhancedLS
(eval-after-load "em-ls"
    '(progn
       (defun ted-eshell-ls-find-file-at-point (point)
         "RET on Eshell's `ls' output to open files."
         (interactive "d")
         (find-file (buffer-substring-no-properties
                     (previous-single-property-change point 'help-echo)
                     (next-single-property-change point 'help-echo))))

       (defun pat-eshell-ls-find-file-at-mouse-click (event)
         "Middle click on Eshell's `ls' output to open files. From Patrick Anderson via the wiki."
         (interactive "e")
         (ted-eshell-ls-find-file-at-point (posn-point (event-end event))))

       (let ((map (make-sparse-keymap)))
         (define-key map (kbd "RET")      'ted-eshell-ls-find-file-at-point)
         (define-key map (kbd "<return>") 'ted-eshell-ls-find-file-at-point)
         (define-key map (kbd "<mouse-2>") 'pat-eshell-ls-find-file-at-mouse-click)
         (defvar ted-eshell-ls-keymap map))

       (defadvice eshell-ls-decorated-name (after ted-electrify-ls activate)
         "Eshell's `ls' now lets you click or RET on file names to open them."
         (add-text-properties 0 (length ad-return-value)
                              (list 'help-echo "RET, mouse-2: visit this file"
                                    'mouse-face 'highlight
                                    'keymap ted-eshell-ls-keymap)
                              ad-return-value)
         ad-return-value)))

;; Disable bars and the blinking cursor
(mapc
 (lambda (mode)
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

;; enable dir local variables
(setq enable-local-eval t)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; General navigation keys
(global-set-key (kbd "<C-tab>") 'other-window)
(global-set-key (kbd "C-,") 'previous-buffer)
(global-set-key (kbd "C-.") 'next-buffer)
(global-set-key (kbd "C-q") 'execute-extended-command)


(defun useful-documents-and-paths ()
  (interactive)
  (ivy-read
   "Select:"
   '("Concedii - //phoenix/Backup/05_Delivery-Main/03_Embedded/01_SU1301/08_Planificarea_resurselor/2017/2017_CONTI_DEV_Holiday_plan.xlsx"
     "OIL - /H/03_Prod_Dev/20_Specifications/80_OpenItems/Customer/OIL_223_HUD_Entry.xlsm"
     "CIL - /H/01_Proj_Org/10_Organization/20_CM_Plan/CIL_MFA2HUD.xlsm"
     "RVL_LIST - /H/02_Quality/90_Reviews/200_Review_List/RVL_MFA2_HUD.xlsm"
     "SwProjectPlan - H:/14_SW_223eHUD/01_SW_Proj_Org/10_SW_Organization/10_Project_Plan/SPRP_223eHUD.docm"
     "DiagORG - /H/14_SW_223eHUD/01_SW_Proj_Org/40_DIAG_ORG"
     "223eHUD Root - /C/work/223eHUD"
     "Customer Standards - /H/03_Prod_Dev/20_Specifications/13_CustomerStandards_223eHUD"
     "Customer Requirements - /H/03_Prod_Dev/20_Specifications/10_CustomerRequirements_223eHUD/10_Delivery"
     "Daimler Mirror - /H/01_Proj_Org/99_ENX_Mirror/Headup_MFA2/05_Software/223HUDe")
   :action (lambda (x)
             (let ((path (car (last (split-string x)))))
               (if (file-directory-p path)
                   (w32-shell-execute "explore" path) ;Open folders in explorer
                 (find-file (car (last (split-string path)))))))))
