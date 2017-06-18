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
	     (setq helm-follow-mode-persistent t)
	     (when (executable-find "curl")
	       (setq helm-google-suggest-use-curl-p t))
             :bind-keymap (("C-c h" . helm-command-map))
             :bind (("M-x" . helm-M-x)
                    ("C-z" . helm-mini) 
                    ("M-y" . helm-show-kill-ring)
                    ("C-h f" . helm-apropos)
                    ("C-h C-l" . helm-locate-library)
                    ("C-x C-f" . helm-find-files)
		    ("C-c g" . helm-google-suggest)
                    ("C-c i" . helm-semantic-or-imenu)
                    :map helm-command-map
                    ("o" . helm-occur)
                    ("I" . helm-imenu-in-all-buffers)
                    :map isearch-mode-map
                    ("C-o" . helm-occur-from-isearch)))

(use-package helm-gtags
             :demand t
	     :config
	     (setq helm-gtags-ignore-case t
		   helm-gtags-auto-update t
		   helm-gtags-use-input-at-cursor t
		   helm-gtags-pulse-at-cursor t
		   helm-gtags-suggested-key-mapping t)
	     (defun config-hooks ()
	       (helm-gtags-mode 1)
	       (local-set-key (kbd "C-j") 'helm-gtags-select)
	       (local-set-key (kbd "M-.") 'helm-gtags-find-tag)
	       (local-set-key (kbd "M-,") 'helm-gtags-pop-stack)
	       (local-set-key (kbd "C-M-.") 'helm-gtags-find-rtag))
	     (add-hook 'c-mode-common-hook 'config-hooks))

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

(use-package beacon
             :config
             (beacon-mode +1))

(use-package which-key
             :config
             (which-key-mode +1))

(use-package cc-mode
             :config
             (defun c-hook ()
               (setq c-default-style "linux"
                     c-basic-offset 4))
             (add-hook 'c-mode-common-hook 'c-hook))

(use-package crux
             :config
	     :bind (("C-c d" . crux-duplicate-current-line-or-region)))

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
	     (setq sp-base-key-bindings 'paredit
		   sp-autoskip-closing-pair 'always
		   sp-hybrid-kill-entire-symbol nil)
	     (sp-use-paredit-bindings)
	     (smartparens-global-mode +1)
	     (show-smartparens-global-mode +1)
	     (smartparens-global-strict-mode +1))

(use-package cmake-mode
             :ensure t
	     :mode (("\\.cmake\\'" . cmake-mode)
		    ("CMakeLists.txt" . cmake-mode))
	     :config
	     (use-package company)
	     (use-package cmake-font-lock)
	     :init
	     (add-hook 'cmake-mode-hook '(lambda ()
					   (make-local-variable 'company-backends)
					   (setq company-backends '(company-cmake company-yasnippet))
					   (company-mode 1)
					   (cmake-font-lock-activate))))

(use-package company
             :ensure t
             :config (global-company-mode +1))

;;; Not sure yet
;; (use-package god-mode
;;              :ensure t
;; 	     :config
;; 	     (god-mode 1)
;; 	     :bind (("<escape>" . god-mode-all)))

(use-package project-bind
             :ensure nil                ; Local package, don't go stupidly searching for it on melpa
	     :config
	     (project-bind renaultM0 "C-c p r"
			   ((root "r" "C:/work/Reanult_M0")
			    (eeprom-layout "e" "C:/work/Reanult_M0/dev/pkg/nvms/tool/eed_layouttool/adapt/input")
			    (eeprom-write "w" "C:/work/Reanult_M0/dev/pkg/nvms/tool/eed_layouttool/adapt/output/DOSHex")))
	     (project-bind ntt-utils "C-c p n"
			   ((concedii "c" "//phoenix/Backup/05_Delivery-Main/03_Embedded/01_SU1301/08_Planificarea_resurselor/2017/2017_CONTI_DEV_Holiday_plan.xlsx")
			    (conti-transfer "t" "O:/MihaiOlteanu")))
	     (project-bind 223eHUD "C-c p h"
			   ((root "r" "O:/MihaiOlteanu")
			    (dds-iso14229 "d" "C:/work/223eHUD/docs"))))

(use-package buffer-sections
             :ensure nil
             :bind (("C-x ]" . forward-section)
                    ("C-x [" . backward-section)
                    ("C-x C-]" . insert-section)
                    ("C-x C-[" . search-sections)))

;; §§§ Other stuff
;; Disable bars and the blinking cursor
(mapc
 (lambda (mode)
   (when (fboundp mode)
     (funcall mode -1)))
 '(menu-bar-mode tool-bar-mode scroll-bar-mode blink-cursor-mode))

;; highlight the current line
(global-hl-line-mode +1)

(desktop-save-mode 1)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; General navigation keys
(global-set-key (kbd "<C-tab>") 'other-window)
(global-set-key (kbd "C-,") 'previous-buffer)
(global-set-key (kbd "C-.") 'next-buffer)

;; (global-set-key (kbd "C-[") 'backward-delete-char)
;; (global-set-key (kbd "M-[") 'backward-kill-word)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-source-names-using-follow (quote ("Occur")))
 '(package-selected-packages
   (quote
    (cmake-font-lock company company-cmake cmake-mode smartparens which-key use-package magit helm-ls-git helm-ag git-timemachine color-theme-sanityinc-tomorrow beacon))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
