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

(use-package helm
  :demand t
  :diminish helm-mode
  :config
  (require 'helm-config)
  (helm-mode)
  ;;Make Helm window at the bottom WITHOUT using any extra package
  ;;https://www.reddit.com/r/emacs/comments/345vtl/make_helm_window_at_the_bottom_without_using_any/
  (add-to-list 'display-buffer-alist
	       `(,(rx bos "*helm" (* not-newline) "*" eos)
		 (display-buffer-in-side-window)
		 (inhibit-same-window . t)
		 (window-height . 0.3)))
  ;; See https://github.com/bbatsov/prelude/pull/670 for a detailed
  ;; discussion of these options. (taken from prelude)
  (setq helm-split-window-in-side-p           t
	helm-buffers-fuzzy-matching           t
	helm-move-to-line-cycle-in-source     nil
	helm-ff-search-library-in-sexp        t
	helm-ff-file-name-history-use-recentf t)
  ;; Enable/disable follow mode with C-c C-f in a helm buffer and keep it afterwards.
  ;; (setq helm-follow-mode-persistent t)
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))
  (defun helm-find-root ()
    "Find file in project root."
    (interactive)
    (helm-find-1 (vc-root-dir)))
  :bind-keymap (("C-c h" . helm-command-map))
  :bind (("M-x" . helm-M-x)
	 ("C-z" . helm-mini) 
	 ("M-y" . helm-show-kill-ring)
	 ("C-h f" . helm-apropos)
	 ("C-h C-l" . helm-locate-library)
	 ("C-x C-f" . helm-find-files)
         ("C-x C-r" . helm-find-root)
	 ("C-c g" . helm-google-suggest)
	 ("C-c i" . helm-semantic-or-imenu)
	 :map helm-command-map
	 ("o" . helm-occur)
	 ("I" . helm-imenu-in-all-buffers)
	 :map isearch-mode-map
	 ("C-o" . helm-occur-from-isearch)
	 :map helm-map
	 ("C-S-P" . helm-follow-action-backward)
	 ("C-S-n" . helm-follow-action-forward)))

;; (use-package ivy
;;   :diminish ivy-mode
;;   :config
;;   (setq ivy-height 15)
;;   (setq ivy-auto-select-single-candidate t)
;;   (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-call)
;;   (define-key counsel-find-file-map (kbd "C-l") 'counsel-up-directory)
;;   (add-hook 'c-mode-common-hook
;;             '(lambda ()
;; 	       (local-set-key (kbd "M-.") 'counsel-gtags-dwim)
;; 	       (local-set-key (kbd "M-,") 'counsel-gtags-go-backward)
;; 	       (local-set-key (kbd "C-M-.") 'helm-gtags-find-rtag)
;; 	       (local-set-key (kbd "C-c M-s") 'helm-gtags-find-symbol)))
;;   :bind
;;   (("M-y" . counsel-yank-pop)
;;    ("M-x" . counsel-M-x)
;;    ("C-x C-f" . counsel-find-file)
;;    ("C-x C-r" . counsel-git)
;;    ("C-c s" . counsel-git-grep)
;;    ))

(use-package helm-gtags
  :demand t
  :config
  (setq helm-gtags-ignore-case t
	helm-gtags-auto-update t
	helm-gtags-use-input-at-cursor t
	helm-gtags-pulse-at-cursor t
	helm-gtags-suggested-key-mapping t)
  (add-hook 'c-mode-common-hook
	    '(lambda ()
	       (helm-gtags-mode 1)
	       (local-set-key (kbd "C-j") 'helm-gtags-select)
	       (local-set-key (kbd "M-.") 'helm-gtags-find-tag)
	       (local-set-key (kbd "M-,") 'helm-gtags-pop-stack)
	       (local-set-key (kbd "C-M-.") 'helm-gtags-find-rtag)
	       (local-set-key (kbd "C-c M-s") 'helm-gtags-find-symbol))))

(use-package helm-ls-git
  :bind (("C-c l" . helm-ls-git-ls)))

(use-package helm-ag
  :bind (("C-c s" . helm-do-ag-project-root)
	 ("C-c S" . helm-do-ag)))

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
		(indent-according-to-mode)) "RET"))))

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
  :config (global-company-mode +1))

(use-package avy
  :config
  (setq avy-keys '(?\; ?l ?k ?j ?h ?u ?i ?o ?p ?m)
	avy-all-windows nil)
  :bind ("C-;" . avy-goto-word-1))

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

(use-package eshell
  :ensure t
  :bind ("C-c e" . eshell)
  :config
  (add-hook 'eshell-mode-hook
	    (lambda ()
	      (eshell-cmpl-initialize)
              (smartscan-mode -1)
              (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
              (define-key eshell-mode-map (kbd "M-r") 'helm-eshell-history)
              (define-key eshell-mode-map (kbd "M-p") 'eshell-previous-input)
              (define-key eshell-mode-map (kbd "M-n") 'eshell-next-input)
              )))

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

;; (defun counsel-find-file-root ()
;;     "Find file in project root."
;;     (interactive)
;;     (counsel-find-file (vc-root-dir)))

(use-package dired-ranger
  :ensure t
  :bind (:map dired-mode-map
	      ("W" . dired-ranger-copy)
	      ("X" . dired-ranger-move)
	      ("Y" . dired-ranger-paste)))

(use-package project-useful
             :ensure nil
	     :config
	     (project-useful ntt-utils
			     ("Concedii" "//phoenix/Backup/05_Delivery-Main/03_Embedded/01_SU1301/08_Planificarea_resurselor/2017/2017_CONTI_DEV_Holiday_plan.xlsx")
			     223eHUD
			     ("Requirements(CDD, Certs, K-Matrix)" "H:/03_Prod_Dev/20_Specifications/10_CustomerRequirements_223eHUD/10_Delivery")
			     ("CIL" "H:/01_Proj_Org/10_Organization/20_CM_Plan/CIL_MFA2HUD.xlsm")
			     ("OIL" "H:/03_Prod_Dev/20_Specifications/80_OpenItems/Customer/OIL_223_HUD_Entry.xlsm")
			     ("RVL_LIST" "H:/02_Quality/90_Reviews/200_Review_List/RVL_MFA2_HUD.xlsm")
			     ("SwProjectPlan - SPRP" "H:/14_SW_223eHUD/01_SW_Proj_Org/10_SW_Organization/10_Project_Plan/SPRP_223eHUD.docm")
			     ("DiagORG" "H:/14_SW_223eHUD/01_SW_Proj_Org/40_DIAG_ORG")
			     ("Root" "C:/work/223eHUD")
			     ("Weekly Meeting Minutes" "H:/14_SW_223eHUD/01_SW_Proj_Org/30_SW_Minutes/02_Weekly_Meeting"))
	     :bind (("C-c p" . project-useful-list-projects)))

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

