;;; init.el --- Config file form Emacs -*- lexical-binding: t -*-

;;; Global package setup.
(require 'package)
(package-initialize)

(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'load-path
             "~/.emacs.d/lisp/")

;; Bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;;; Looks and feels, global keys and global settings.
;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Disable bars and the blinking cursor
(mapc (lambda (mode)
        (when (fboundp mode)
          (funcall mode -1)))
      '(menu-bar-mode tool-bar-mode scroll-bar-mode blink-cursor-mode))

(global-hl-line-mode +1)                ; highlight the current line
(desktop-save-mode 1)
(set-fringe-mode 0)
(put 'compile-command 'safe-local-variable #'stringp)

(setq-default
 fill-column 80
 indent-tabs-mode nil) ; Use spaces instead of tabs

;; Use ESC for 'keyboard-quit, instead of C-g
(define-key key-translation-map [escape] (kbd "C-g"))
;; Don't treat C-i as identical to TAB (i.e. rebinding C-i will not rebind TAB
;; at the same time). This means C-i will need to be specified as <C-i> when
;; binding it
(define-key input-decode-map [?\C-i] [C-i])

(setq org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-confirm-babel-evaluate nil
      org-edit-src-content-indentation 0
      org-confirm-elisp-link-function nil
      browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium"
      compilation-read-command nil
      use-package-always-ensure t
      inhibit-startup-message t
      package-enable-at-startup nil
      enable-local-eval t
      debugger-stack-frame-as-list t    ; Show calls as lists in the debugger
      scroll-preserve-screen-position 2 ; Keep point fixed when C-v scrolling
      confirm-kill-processes nil
      backup-directory-alist
          `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms
          `((".*" ,temporary-file-directory t)))

(set-face-attribute 'default nil
                    :family "Source Code Pro"
                    :height 120
                    :weight 'normal
                    :width 'normal)

;; Open compilation results in a bottom buffer, similar to helm, identical code.
(add-to-list 'display-buffer-alist
	     `(,(rx bos "*compilation" (* not-newline) "*" eos)
	       (display-buffer-in-side-window)
	       (inhibit-same-window . t)
	       (window-height . 0.3)))
;; After compilation, go to the compilation buffer (q key to close it)
(add-hook 'compilation-finish-functions
          (lambda (buffer result)
            (switch-to-buffer-other-window "*compilation*")))

(mapc (lambda (hook)
        (add-hook hook (lambda ()
                         (prettify-symbols-mode 1)
                         (setq lisp-prettify-symbols-alist nil)
                         (push '("lambda" . "λλ") lisp-prettify-symbols-alist)                         
                         (push '("defun" . "ƒƒ") lisp-prettify-symbols-alist)
                         (push '("defmacro" . "mm") lisp-prettify-symbols-alist)
                         (push '("mapcar" . "»»") lisp-prettify-symbols-alist))))
      '(lisp-mode-hook emacs-lisp-mode-hook scheme-mode-hook))

(defun revert-buffer-no-confirm ()
  (interactive)
  (revert-buffer t t))

(use-package cl :ensure t)
(use-package anaphora :ensure t)

;; Common Lisp
(use-package sly :ensure t)
(use-package common-lisp-snippets :ensure t)

;;; Use-Packages for extra functionality.
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
                   "Language: "
                   '(" " "Emacs Lisp" "Common Lisp")))
        (code (read-string "Code: ")))
    (browse-url
     (concat "https://github.com/search?l=" language
             "&type=code&q=" code))))

(defun google-search-str (str)
  (browse-url
   (concat "https://www.google.com/search?q=" str)))

(defun google-search ()
  "Google search region, if active, or ask for search string."
  (interactive)
  (if (region-active-p)
      (google-search-str
       (buffer-substring-no-properties (region-beginning)
                                       (region-end)))
    (google-search-str (read-from-minibuffer "Search: "))))

(use-package avy
  :ensure t
  :config
  (setq avy-timeout-seconds 0.2)
  (setq avy-all-windows nil))

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

  (defun counsel-find-file-root ()
    "Find file in project root."
    (interactive)
    (counsel-file-jump nil (vc-root-dir)))

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

  (ivy-set-actions
   'counsel-M-x
   '(("d" (lambda (f) (describe-function (intern f))) "describe")
     ))

  (defun describe-function-from-ivy ()
    (interactive)
    (ivy-exit-with-action (lambda (s)
                            (describe-function (intern s))
                            (ivy-resume)))))

(use-package eshell
  :init
  (setq eshell-hist-ignoredups t
        eshell-save-history-on-exit t)
  :config
  (use-package eshell-fringe-status)
  (use-package eshell-fixed-prompt)
  (add-hook 'eshell-mode-hook
	    (lambda ()
	      (eshell-cmpl-initialize)
              (smartscan-mode -1)
              (setenv "GIT_PAGER" "")   ; Make git usable              
              (eshell-fringe-status-mode)
              (bind-keys :map eshell-mode-map
                         ([remap eshell-pcomplete] . completion-at-point)
                         ("C-r" . counsel-esh-history)
                         ("<up>" . previous-line)
                         ("<down>" . next-line)
                         ("<left>" . left-char)
                         ("<right>" . right-char)))))

(use-package git-timemachine)
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
  (set-face-foreground 'git-gutter+-added    "olive drab")
  (set-face-foreground 'git-gutter+-deleted  "firebrick")
  (set-face-foreground 'git-gutter+-modified "dark magenta"))

(use-package which-key
  :diminish
  :config
  (which-key-mode +1))

(use-package cc-mode
  :config
  (add-hook
   'c-mode-common-hook
   '(lambda ()
      (setq c-default-style "k&r"
            c-basic-offset 4)
      (local-set-key (kbd "C-c C-c") 'compile)
      (local-set-key (kbd "M-.")     'counsel-gtags-dwim)
      (local-set-key (kbd "M-,")     'counsel-gtags-go-backward)
      (local-set-key (kbd "C-M-.")   'counsel-gtags-find-reference-at-point))))

(use-package crux
  :config
  :bind (("C-c d" . crux-duplicate-current-line-or-region)
	 ("C-a" . crux-move-beginning-of-line)))

(use-package smartscan
  :diminish
  :config
  (global-smartscan-mode 1))

(use-package smartparens
  :diminish
  :config
  (require 'smartparens-config)
  (setq sp-base-key-bindings 'paredit
	sp-autoskip-closing-pair 'always
	sp-hybrid-kill-entire-symbol nil)
  ;; (sp-use-paredit-bindings)
  (smartparens-global-mode +1)
  (show-smartparens-global-mode +1)
  (smartparens-global-strict-mode +1)
  (defun sp-kill-region-or-backward-word ()
    "If the region is active and non-empty, kill the region
    otherwise kill the word as in bash (from stackoverflow)"
    (interactive)
    (call-interactively
     (if (use-region-p) 'sp-kill-region 'sp-backward-kill-word))))

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
  (use-package company-quickhelp)
  :bind (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("C-j" . company-show-doc-buffer)))

(use-package org
  :config
  (custom-set-faces
   '(org-link ((t (:foreground "#6699cc" :underline nil)))))
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
  (setq org-startup-indented t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-ask-before-returning-to-edit-buffer nil
        org-confirm-babel-evaluate nil  ; Just C-c C-c to evaluate src block, no questions asked
        org-babel-lisp-eval-fn 'sly-eval) ; Use sly instead of the default slime
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
  (use-package dired-subtree)
  (setq dired-listing-switches "-lah")
  (put 'dired-find-alternate-file 'disabled nil) ;enable 'a' command in dired
  (custom-set-faces
   '(dired-directory ((t (:foreground "deepskyblue" :weight bold))))
   '(dired-marked    ((t (:foreground "orange red" :weight extra-bold))))
   '(dired-header    ((t (:foreground "blanchedalmond" :weight bold :height 165)))))
  :hook (dired-mode . dired-hide-details-mode))

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
  
  (defun counsel-sly-mrepl-shortcut ()
    "Interact with the sly repl using counsel."
    (interactive)
    (ivy-read
     "Action: " 
     (mapcar #'car sly-mrepl-shortcut-alist)
     :action (lambda (string)
               (let ((command (and string
                                   (cdr (assoc string sly-mrepl-shortcut-alist)))))
                 (call-interactively command)))))

  (defun counsel-sly-mrepl-history ()
    "Use counsel to search through mrepl history."
    (interactive)
    (ivy-read
     "History: "
     (ring-elements comint-input-ring)
     :action (lambda (e)
               (insert e))))

  (add-hook 'sly-mrepl-mode-hook
            (lambda ()
              (bind-keys :map sly-mrepl-mode-map
                  ([remap isearch-backward] . counsel-sly-mrepl-history)
                  (",," . counsel-sly-mrepl-shortcut)
                  (",p" . sly-mrepl-set-package)
                  (",r" . sly-restart-inferior-lisp))))
  ;; Remove M-n and M-p from sly-editing-mode-map in sly.el. Don't know how else
  ;; to enable smartscan in lisp buffers since sly overrides them.
  )

(use-package yasnippet
  :diminish t
  :config
  (yas-global-mode 1)
  (define-key yas-minor-mode-map (kbd "SPC") yas-maybe-expand)
  (yas-reload-all))

(defun buffer-toggle ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun insert-or-kill ()
  (interactive)
  (if (region-active-p)
      (sp-kill-region-or-backward-word)
    (yank)))

(defun eshell-root ()
  "Keep an eshell that we can come back to."
  (interactive)
  (let ((eshell-buffer-name "root-eshell"))
    (eshell)))

;; Patch for git-gutter+ to work with tramp
;; https://github.com/nonsequitur/git-gutter-plus/issues/42#issuecomment-464463744
(with-eval-after-load 'git-gutter+
   (defun git-gutter+-remote-default-directory (dir file)
     (let* ((vec (tramp-dissect-file-name file))
            (method (tramp-file-name-method vec))
            (user (tramp-file-name-user vec))
            (domain (tramp-file-name-domain vec))
            (host (tramp-file-name-host vec))
            (port (tramp-file-name-port vec)))
       (tramp-make-tramp-file-name method user domain host port dir)))

   (defun git-gutter+-remote-file-path (dir file)
     (let ((file (tramp-file-name-localname (tramp-dissect-file-name file))))
       (replace-regexp-in-string (concat "\\`" dir) "" file))))


(use-package hydra
  :ensure t)

(defhydra hydra-git ()
  ("e" git-gutter+-previous-hunk "previous hunk")
  ("d" git-gutter+-next-hunk "next hunk")
  ("s" git-gutter+-stage-hunks "stage")
  ("r" git-gutter+-revert-hunk "revert")
  ("R" vc-revert-buffer "revert buffer")
  ("a" vc-diff "diff")
  ("c" git-gutter+-commit "commit" :exit t)
  ("p" magit-push-current "push")
  ("l" magit-log-all "log-all" :exit t)
  ("q" nil "quit")
  ("s-g" nil "quit"))

(defhydra hydra-buffer ()
  ("d" split-window-horizontally "split h")
  ("c" split-window-vertically "split v")
  ("s" enlarge-window-horizontally "left")
  ("f" shrink-window-horizontally "right")
  ("k" delete-window "delete" :exit t)
  ("q" nil "quit"))

(defhydra hydra-file (:exit t)
  ("s" (lambda () (interactive) (switch-to-buffer "*scratch*"))
       "*scratch*")
  ("i" (lambda () (interactive) (switch-to-buffer "init.el"))
       "init.el")
  ("m" (lambda () (interactive) (switch-to-buffer "*Messages*"))
       "*Messages*")
  ("q" nil "quit"))

(bind-keys
 ("<S-insert>" . insert-or-kill)
 ("C-c C-c" . compile)
 ("C-x k"   . kill-current-buffer)
 ("<C-tab>" . other-window)
 ("C-,"     . previous-buffer)
 ("C-."     . next-buffer)
 ("C-S-r"   . revert-buffer-no-confirm)
 ("C-f"     . counsel-find-file)
 ("C-r"     . counsel-find-file-root)
 ("C-s"     . swiper)
 ("s-b"     . buffer-toggle)
 ("s-e"     . eshell-root)
 ("s-E"     . eshell)
 ("s-d"     . dired)
 ("C-z"     . undo)

 ("s-a" . avy-goto-char-timer)
 
 ("C-v" . (lambda () (interactive) (scroll-up-command   15)))
 ("M-v" . (lambda () (interactive) (scroll-down-command 15)))
 ;; Jump to mark
 ("C-S-SPC" . (lambda () (interactive) (set-mark-command 0)))
 
 ;; Smartparens
 ("C-w"       . google-search)
 ;;("" . sp-forward-sexp)
 ;;("" . sp-up-sexp)
 ;;("C-e" . sp-backward-up-sexp)
 ;;("C-d" . sp-down-sexp)
 ("s-u" . sp-raise-sexp)
 ("M-o" . sp-wrap-round)
 ("M-l" . sp-forward-slurp-sexp)
 ("s-l" . sp-forward-barf-sexp)
 ("M-c" . sp-copy-sexp)
 ("M-u" . sp-unwrap-sexp)
 ;; Kill sexp and remove the whole line if there is nothing on it anymore.
 ("M-d" . sp-kill-sexp)
 
 ("C-<backspace>" . sp-kill-word)
 ("M-i" . sp-indent-defun)
 
 ;; Hydras
 ("s-g"   . hydra-git/body)
 ("s-w"   . hydra-buffer/body)
 ("s-f"   . hydra-file/body)
 
 ;; Counsel
 ("M-y"     . counsel-yank-pop)
 ("C-c C-r" . ivy-resume)
 ("C-c i"   . counsel-semantic)
 ([remap isearch-forward] . swiper)
 ("s-s"     . counsel-ag)
 
 :map counsel-find-file-map
 ("C-l"     . counsel-up-directory)
 ("C-j"     . ivy-alt-done)

 :map ivy-minibuffer-map
 ("C-j"     . ivy-call)
 ("C-w"     . ivy-yank-word)
 ("C-o"     . ivy-dispatching-done) ; Select actions
 ("<escape>" . minibuffer-keyboard-quit)

 :map isearch-mode-map
 ("C-o"     . swiper-isearch-string)

 :map counsel-describe-map
 ("C-q"     . describe-function-from-ivy)

 :map dired-mode-map
 ("d"     . dired-next-line)
 ("e"     . dired-previous-line)
 ("s"     . dired-up-directory)              
 ("f"     . dired-find-alternate-file)
 ("g"     . dired-subtree-toggle)
 ("q"     . kill-this-buffer)

 :map emacs-lisp-mode-map
 ("M-e" . eval-defun)

 :map sly-mode-map
 ("M-e" . sly-eval-defun)
 )

(with-eval-after-load 'scheme
  (bind-keys
   :map scheme-mode-map
   ("M-e" . geiser-eval-definition)
   ("C-." . next-buffer) 
   ("C-," . previous-buffer)))

(add-to-list 'load-path "~/.emacs.d/lisp/mugur")
(require 'mugur)
(setf mugur-qmk-path "/home/mihai/projects/qmk_firmware")
(mugur-load-keybindings)

;; ("template"
;;     (( ) ( ) ( ) ( ) ( ) ( ) ( )     ( ) ( ) ( ) (C-x 0) ( ) ( ) ( )
;;      ( ) ( ) ( ) ( ) ( ) ( ) ( )     ( ) ( ) ( )   ( )   ( ) ( ) ( )
;;      ( ) ( ) ( ) ( ) ( ) ( )             ( ) ( )   ( )   ( ) ( ) ( )
;;      ( ) ( ) ( ) ( ) ( ) ( ) ( )     ( ) ( ) ( )   ( )   ( ) ( ) ( )
;;      ( ) ( ) ( ) ( ) ( )                     ( )   ( )   ( ) ( ) ( )
;;                          ( ) ( )     ( ) ( )
;;                              ( )     ( )
;;                      ( ) ( ) ( )     ( ) ( ) ( )))

(mugur-keymap
 :rgblight-enable t
 :layers
  '(("base" vertical
     ((C-f1)       (vol-down) (vol-up) ( ) ( ) (---) (reset) 
      (---)         (C-x k)     (w)          (e)              (r)       (t)   (---) 
      (---)           (a)      (G s)        (M d)            (C f)      (g) 
      (osm S)         (z)       (x)          (c)              (v)       (b)   (---)
      (C)          (---)     (---)        (---)            (tab)
                                                                        (tab) (---)
                                                                              (---)     
                                                             (bspace) (space) (---) 
                                                             
      (---) (---)   (---)          (---)    (---)  (---)   (---)
      (---)  (y)     (u)        (lt num i)   (o)   (---)   (---)
             (h)    (C j)          (M k)    (G l)   (p)    (---)
      (---)  (n)     (m)          (comma)   (dot)   (q)   (osm S)
                    (---)          (up)     (down) (left) (right)
                                                    (---)  (---)
                                                         (C-x b)
      (lt media escape) (lt movement enter) (lt symbols pscreen)))
  
    ("num"
     (( ) ( ) ( ) ( ) ( ) ( ) ( )     ( ) ( ) ( ) ( ) ( ) ( ) ( )
      ( ) ( ) (1) (2) (3) ( ) ( )     ( ) ( ) ( ) ( ) ( ) ( ) ( )
      ( ) (0) (4) (5) (6) ( )             ( ) ( ) ( ) ( ) ( ) ( )
      ( ) (0) (7) (8) (9) ( ) ( )     ( ) ( ) ( ) ( ) ( ) ( ) ( )
      ( ) ( ) ( ) ( ) ( )                     ( ) ( ) ( ) ( ) ( )
                          ( ) ( )     ( ) ( )
                              ( )     ( )
                      ( ) ( ) ( )     ( ) ( ) ( )))
   
    ("movement"
     (( )  ( )         ( )         ( )        ( )     ( )   ( )     ( ) ( ) ( ) ( ) ( ) ( ) ( )
      ( )  ( )     (C-u C-space)   (up)   (S-insert) (M-<)  ( )     ( ) ( ) ( ) (sp-backward-up-sexp) ( ) ( ) ( )
      ( ) (C-a)       (left)      (down)    (right)  (C-e)              ( ) (sp-backward-sexp) ( ) (sp-forward-sexp) ( ) ( )
      ( ) (undo)       ( )         (  )       ( )    (M->)  ( )     ( ) ( ) ( ) ( ) ( ) ( ) ( )
      ( )  ( )         ( )         (  )       ( )                           ( ) ( ) ( ) ( ) ( )
                                                        ( ) ( )     ( ) ( )
                                                            ( )     ( )
                                           (delete) (C-tab) ( )     ( ) ( ) ( )))

    ("symbols"
     (( ) ( )   ("[")  ("]")   ({)   (})  ( )     ( ) ( ) ( ) ( ) ( ) ( ) ( )
      ( ) (~)    ( )   ("'") ("\"") ("`") ( )     ( ) ( ) ( ) ( ) ( ) ( ) ( )
      ( ) (";")  (:)    (-)  ("(")  (")")             ( ) ("'(1 2 3)") ( ) ( ) ( ) ( )
      ( ) ("/") ("\\")  (=)   (+)    (_)  ( )     ( ) ( ) ( ) ( ) ( ) ( ) ( )
      ( ) ( )    (|)    (<)   (>)                     ( ) ( ) ( ) ( ) ( )
                                     ( ) ( )     ( ) ( )
                                         ( )     ( )
                               (!) ("?") ( )     ( ) ( ) ( )))

    ("media"
     (( )       ( )        ( )       ( )        ( )      ( ) ( )     ( ) ( ) ( ) ( ) ( ) ( ) ( )
      (rgb_sad) (rgb_sai)     ( )     (ms_up)   (ms_wh_up)  ( ) ( )     ( ) ( ) ( ) ( ) ( ) ( ) ( )
      (rgb_vad) (rgb_vai ) (ms_left) (ms_down)  (ms_right)  ( )             ( ) ( ) ( ) ( ) ( ) ( )
      (rgb_hud) (rgb_hui)     ( )       ( )    (ms_wh_down) ( ) ( )     ( ) ( ) ( ) ( ) ( ) ( ) ( )
      (rgb_tog)    ( )        ( )       ( )        ( )                          ( ) ( ) ( ) ( ) ( )
                                                            ( ) ( )     ( ) ( )
                                                                ( )     ( )
                                      (ms_btn2) (ms_btn1) (ms_btn3)     ( ) ( ) ( )))))

(mugur-load-keybindings)

(defun yocto-find-var-definition ()
  (interactive)
  (let ((var (cl-reduce
              (lambda (rest word)
                (if (s-uppercase-p word)
                    (concat rest "_" word)
                  rest))
              (s-split "_" (symbol-name (symbol-at-point)))))
        (yocto-page "https://www.yoctoproject.org/docs/latest/ref-manual/ref-manual.html#var-%s"))
    (browse-url
     (format yocto-page var))))

(add-to-list 'load-path "~/.emacs.d/lisp/mugur")
(require 'mugur)

(setf mugur-qmk-path          "/home/mihai/projects/qmk_firmware"
      mugur-keyboard-name     "ergodox_ez"
      mugur-layout-name       "LAYOUT_ergodox"
      mugur-keymap-name       "mugur_test"
      )

(mugur-mugur
 '(("base" 
    C-f1    vol-down  vol-up   -x-   -x-  -x- reset
    -x-     (C-x k)     w      e     r    t   git-pass 
    -x-        a      (G s)  (M d) (C f)  g 
    (OSM S)      z        x      c     v    b   forti-pass
    (DANCE a ent)       -x-      -x-    -x-    -x-
    
    tab   split-window-below
    -x-
    bspace space  -x-
    ;;############################################
    -x- -x-  -x-     -x-      lead  -x-    -x-
    -x-  y    u   (LT NUM i)   o   -x-    -x-
    h  (C j)   (M k)    (G l)  p     -x-
    -x-  n    m     comma     dot   q   (OSM S)
    -x-      up     down  left  right
    
    -x-               -x-
    (C-x b)     
    (LT media escape) (LT movement ent) (LT symbols pscreen))

   ("num"
    --- --- --- --- --- --- ---
    --- ---  1   2   3  --- --- 
    ---  0   4   5   6  --- 
    ---  0   7   8   9  --- --- 
    --- --- --- --- --- 
    --- --- 
    ---     
    --- --- ---

    --- --- --- --- --- --- ---
    --- --- --- --- --- --- ---
    --- --- --- --- --- ---
    --- --- --- --- --- --- ---
    --- --- --- --- ---
    --- ---
    ---
    --- --- ---)  

   ("movement"
    ---  ---       ---      ---     ---    --- --- 
    ---  ---  (C-u C-space)  up   S-insert M-< --- 
    ---  C-a      left      down   right   C-e 
    ---  undo      ---      ---     ---    M-> ---
    ---  ---       ---      ---     --- 
    ---  --- 
    ---
    delete C-tab ---
    ;;############################################
    --- ---      ---                ---                 ---       --- ---
    --- ---      ---          sp-backward-up-sexp       ---       --- ---
    --- sp-backward-sexp            ---           sp-forward-sexp --- ---
    --- ---      ---                ---                 ---       --- ---
    --- ---      ---                ---                 ---
    --- ---
    ---
    --- --- ---)

   ("symbols"
    ---  ---  ?\[  ?\]  ?\{  ?\}  ---
    ---  ?\~  ---  ?\'  ?\"  ?\`  ---
    ---  ?\;  ?\:  ?\-  ?\(  ?\) 
    ---  ?\/  ?\\  ?\=  ?\+  ?\_  ---
    ---  ---  ?\|  ?\<  ?\> 
    ---  ---
    ---
    ?\! ?\? ---
    
    --- --- --- --- --- --- ---
    --- --- --- --- --- --- ---
    --- --- --- --- --- ---
    --- --- --- --- --- --- ---
    --- --- --- --- ---
    --- ---
    ---
    --- --- ---)

   ("media"
    ---       ---        ---     ---        ---     --- --- 
    rgb_sad  rgb_sai     ---    ms_up    ms_wh_up   --- ---
    rgb_vad  rgb_vai  ms_left  ms_down   ms_right   --- 
    rgb_hud  rgb_hui     ---     ---    ms_wh_down  --- ---
    rgb_tog    ---       ---     ---        --- 
    --- ---
    --- 
    ms_btn2  ms_btn1  ms_btn3
    
    --- --- --- --- --- --- ---
    --- --- --- --- --- --- ---
    --- --- --- --- --- ---
    --- --- --- --- --- --- ---
    --- --- --- --- ---
    --- ---
    ---
    --- --- ---)))

(defun signed-of-by-me ()
  (interactive)
  (insert "Signed-off-by: Mihai.Olteanu <Mihai.Olteanu@analog.com>"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(counsel-ag-base-command "ag --nocolor --nogroup %s --ignore-dir build")
 '(package-selected-packages
   (quote
    (load-relative doom-modeline lastfm counsel-web somafm org-web-tools esqlite mpv yaml-mode wrap-region which-key wgrep utop use-package smex smartscan smartparens smart-mode-line sly-repl-ansi-color racer package-lint-flymake org2web org-bullets openwith o-blog nov markdown-preview-mode magit macrostep ivy-youtube ivy-dired-history inf-ruby html-to-markdown haskell-mode git-timemachine git-gutter-fringe git-gutter-fringe+ geiser flycheck-rust flycheck-package eshell-fringe-status eshell-fixed-prompt elpy elixir-yasnippets ediprolog disaster dired-subtree dired-ranger diminish crux counsel-gtags counsel-dash company-quickhelp company-c-headers color-theme-sanityinc-tomorrow cmake-font-lock circe caml camcorder beacon auto-complete-c-headers alchemist ace-flyspell)))
 '(safe-local-variable-values (quote ((mangle-whitespace . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dired-directory ((t (:foreground "deepskyblue" :weight bold))))
 '(dired-header ((t (:foreground "blanchedalmond" :weight bold :height 165))))
 '(dired-marked ((t (:foreground "orange red" :weight extra-bold))))
 '(org-link ((t (:foreground "#6699cc" :underline nil)))))
