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
                         (push '("mapcar" . "»»") lisp-prettify-symbols-alist)
                         (push '("car"    . "11") lisp-prettify-symbols-alist)
                         (push '("cadr"   . "22") lisp-prettify-symbols-alist)
                         (push '("caddr"  . "33") lisp-prettify-symbols-alist)
                         (push '("cadddr" . "44") lisp-prettify-symbols-alist))))
      '(lisp-mode-hook emacs-lisp-mode-hook))

(defun revert-buffer-no-confirm ()
  (interactive)
  (revert-buffer t t))

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
  (counsel-web-suggest
   (when (region-active-p)
     (buffer-substring-no-properties (region-beginning)
                                     (region-end)))))

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

  (use-package counsel-web
    :config
    (setf counsel-web-suggest-action 'google-search-str)
    (setf counsel-web-engine 'google))
  
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
              (eshell-fixed-prompt-mode)
              (eshell-fringe-status-mode)
              (bind-keys :map eshell-mode-map
                         ("C-r" . counsel-esh-history)
                         ("M-p" . eshell-previous-input)
                         ("M-n" . eshell-next-input))))
  :bind (("C-c e" . eshell)))

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
  (set-face-foreground 'git-gutter+-modified "dark magenta")
  :bind
  (("C-x v n"   . git-gutter+-next-hunk)
   ("C-x v p"   . git-gutter+-previous-hunk)
   ("C-x v C-s" . git-gutter+-show-hunk)
   ("C-x v s"   . git-gutter+-stage-hunks)
   ("C-x v u"   . git-gutter+-unstage-whole-buffer)
   ("C-x v c"   . git-gutter+-commit)
   ("C-x v C-c" . git-gutter+-stage-and-commit)))

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

(use-package python
  :config
  (use-package company-anaconda)
  (use-package anaconda-mode
    :defer t
    :after (company-anaconda)
    :init
    (add-to-list 'company-backends 'company-anaconda)
    (add-hook 'python-mode-hook 'anaconda-mode))
  (use-package elpy
    :config
    (add-hook 'elpy-mode-hook
              (lambda () (highlight-indentation-mode -1)))))

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
     (if (use-region-p) 'sp-kill-region 'sp-backward-kill-word)))
  :bind
  ("C-w" . sp-kill-region-or-backward-word)
  ("M-b" . sp-backward-symbol))

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

(use-package avy
  :config
  (setq avy-keys '(?\; ?l ?k ?j ?h ?u ?i ?o ?p ?m))
  (setq avy-timeout-seconds 0.2
        avy-all-windows nil)
  :bind ("C-;" . avy-goto-char-timer))

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
  :config
  (use-package dired-x
    :hook (dired-mode . dired-omit-mode))
  (use-package dired-subtree)
  (setq dired-listing-switches "-lah")
  (put 'dired-find-alternate-file 'disabled nil) ;enable 'a' command in dired
  (custom-set-faces
   '(dired-directory ((t (:foreground "deepskyblue" :weight bold))))
   '(dired-marked    ((t (:foreground "orange red" :weight extra-bold))))
   '(dired-header    ((t (:foreground "blanchedalmond" :weight bold :height 165)))))
  :hook (dired-mode . dired-hide-details-mode)
  :bind (:map dired-mode-map
              ("C-l"   . 'dired-up-directory)              
              ("I"     . 'dired-kill-subdir)
              ("<tab>" . 'dired-hide-subdir)
              ("C-i"   . 'dired-subtree-toggle)
              ("q"     . kill-this-buffer)))

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

(defun eshell-toggle ()
  "Toggle between eshell buffers.
If you are in a shell buffer, switch the window configuration
back to your code buffers.  Otherwise, create at least one shell
buffer if it doesn't exist already, and switch to it.  On every
toggle, the current window configuration is saved in a register."
  (interactive)
  (if (eq major-mode 'eshell-mode)
      (progn (window-configuration-to-register ?Z)
             (jump-to-register ?W))
    ;; Save current window config and jump to shell
    (window-configuration-to-register ?W)
    (condition-case nil
        (jump-to-register ?Z)
      (error
       (eshell)
       (when (= (length (window-list)) 2)
         (other-window 1)
         (eshell 1)
         (other-window 1))))
    (window-configuration-to-register ?Z)))

(defun buffer-toggle ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun insert-or-kill ()
  (interactive)
  (if (region-active-p)
      (sp-kill-region-or-backward-word)
    (yank)))

(bind-keys
 ("C-c C-c" . compile)
 ("C-x k"   . kill-current-buffer)
 ("<C-tab>" . other-window)
 ("C-,"     . previous-buffer)
 ("C-."     . next-buffer)
 ("C-q"     . execute-extended-command)
 ("C-S-r"   . revert-buffer-no-confirm)
 ("s-k"     . sp-kill-sexp) 
 ("s-t"     . eshell-toggle)
 ("s-b"     . buffer-toggle)

 ("C-v" . (lambda () (interactive) (scroll-up-command 10)))
 ("M-v" . (lambda () (interactive) (scroll-down-command 10)))
 ;; Jump to mark
 ("C-S-SPC" . (lambda () (interactive) (set-mark-command 0)))

 ;; Smartscan
 ("C-M-p" . smartscan-symbol-go-backward)
 ("C-M-n" . smartscan-symbol-go-forward)

 ;; Smartparens
 ("C-w"       . sp-kill-region-or-backward-word)
 ;; ("C-s-f" . sp-forward-parallel-sexp)
 ;; ("C-s-b" . sp-backward-parallel-sexp)
 ("C-f"   . sp-forward-sexp)
 ("C-M-b" . sp-backward-sexp)
 ("C-s-b" . sp-backward-up-sexp)
 ("C-s-f" . sp-down-sexp)

  ;; Counsel
 ("M-y"     . counsel-yank-pop)
 ("C-x C-f" . counsel-find-file)
 ("C-x C-r" . counsel-find-file-root)
 ("C-c s"   . counsel-ag)
 ("C-c C-r" . ivy-resume)
 ("C-z"     . ivy-switch-buffer)
 ("C-c i"   . counsel-semantic)
 ([remap isearch-forward] . swiper)
 ("C-S-s"   . swiper)
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
 ("C-q"     . describe-function-from-ivy))


(add-to-list 'load-path "~/.emacs.d/lisp/mugur")
(require 'mugur)
(setf mugur-qmk-path "/home/mihai/projects/qmk_firmware")
(mugur-load-keybindings)

(mugur-keymap
 :rgblight-enable t
 :layers
  '(("base" vertical
     ((C-f1)       (vol-down) (vol-up) (vuiet-play-pause) (vuiet-next) (---) (reset) 
      (---)         (C-x k)     (w)          (e)              (r)       (t)   (---) 
      (---)           (a)      (G s)        (M d)            (C f)      (g) 
      (osm S)         (z)       (x)          (c)              (v)       (b)   (---)
      (tg xwindow)   (---)     (---)        (---)            (---) 
                                                                  (pscreen p) (---) 
                                                                              (M-x)     
                                       (lt emacs_r bspace) (lt xwindow space) (tab) 

      (---) (---)   (---)          (---)       (---) (---)   (---)
      (---)  (y) (lt num_up u)  (lt num i)      (o)  (---)   (---)
             (h)    (C j)      (lt symbols k)  (M l)  (p)    (---)
      (---)  (n)     (m)          (comma)      (dot)  (q)   (osm S)
                    (---)          (up)       (down) (left) (right)
                                                 (---)  (pscreen n)
                                                              (C-z)
                     (lt media escape) (lt emacs_l enter) (pscreen)))

    ("xwindow" (0 1 1)
     (( ) ( ) ( ) ( ) ( ) ( ) ( )     ( ) ( )   ( )     ( )    ( )    ( )    ( )
      ( ) ( ) ( ) ( ) ( ) ( ) ( )     ( ) ( )   ( )    (G-b)   ( )    ( )    ( )
      ( ) ( ) ( ) ( ) ( ) ( )             ( ) (C-t f) (C-t e) (G-t) (C-t z) ( )
      ( ) ( ) ( ) ( ) ( ) ( ) ( )     ( ) ( )   ( )     ( )    ( )    ( )    ( )
      ( ) ( ) ( ) ( ) ( )                       ( )     ( )    ( )    ( )    ( )
                          ( ) ( )     ( ) ( )
                              ( )     ( )
                      ( ) ( ) ( )     ( ) (C-t C-t) (C-t !)))
  
    ("num"
     (( ) ( ) ( ) ( ) ( ) ( ) ( )     ( ) ( ) ( ) ( ) ( ) ( ) ( )
      ( ) ( ) (1) (2) (3) ( ) ( )     ( ) ( ) ( ) ( ) ( ) ( ) ( )
      ( ) (0) (4) (5) (6) ( )             ( ) ( ) ( ) ( ) ( ) ( )
      ( ) (0) (7) (8) (9) ( ) ( )     ( ) ( ) ( ) ( ) ( ) ( ) ( )
      ( ) ( ) ( ) ( ) ( )                     ( ) ( ) ( ) ( ) ( )
                          ( ) ( )     ( ) ( )
                              ( )     ( )
                      ( ) ( ) ( )     ( ) ( ) ( )))
   
    ("num_up"
     (( )  ( )  ( ) ( )  ( )   ( ) ( )     ( ) ( ) ( ) ( ) ( ) ( ) ( )
      ( )  ( )  (!) (@) (hash) ( ) ( )     ( ) ( ) ( ) ( ) ( ) ( ) ( )
      ( ) ("?") ($) (%)  (^)   ( )             ( ) ( ) ( ) ( ) ( ) ( )
      ( ) ("?") (&) (*)  ( )   ( ) ( )     ( ) ( ) ( ) ( ) ( ) ( ) ( )
      ( )  ( )  ( ) ( )  ( )                       ( ) ( ) ( ) ( ) ( )
                               ( ) ( )     ( ) ( )
                                   ( )     ( )
                           ( ) ( ) ( )     ( ) ( ) ( )))
  
    ("emacs_l"
     (( )  (C-{ a)    (C-x })     (C-x 3)   (C-x {)   ( )   ( )     ( ) ( ) ( ) ( ) ( ) ( ) ( )
      ( )  ( )     (C-u C-space)   (up)   (S-insert) (M-<)  ( )     ( ) ( ) ( ) ( ) ( ) ( ) ( )
      ( ) (C-a)       (left)      (down)    (right)  (C-e)              ( ) ( ) ( ) ( ) ( ) ( )
      ( ) (undo)       ( )         (  )       ( )    (M->)  ( )     ( ) ( ) ( ) ( ) ( ) ( ) ( )
      ( )  ( )         ( )         (  )       ( )                           ( ) ( ) ( ) ( ) ( )
                                                        ( ) ( )     ( ) ( )
                                                            ( )     ( )
                                   (delete) (C-tab) (mark-sexp)     ( ) ( ) ( )))

    ("emacs_r"
     (( ) ( ) ( ) ( ) ( ) ( ) ( )     ( ) ( ) ( ) (C-x 0) ( ) ( ) ( )
      ( ) ( ) ( ) ( ) ( ) ( ) ( )     ( ) ( ) ( )   ( )   ( ) ( ) ( )
      ( ) ( ) ( ) ( ) ( ) ( )             ( ) ( )   ( )   ( ) ( ) ( )
      ( ) ( ) ( ) ( ) ( ) ( ) ( )     ( ) ( ) ( )   ( )   ( ) ( ) ( )
      ( ) ( ) ( ) ( ) ( )                     ( )   ( )   ( ) ( ) ( )
                          ( ) ( )     ( ) ( )
                              ( )     ( )
                      ( ) ( ) ( )     ( ) ( ) ( )))

    ("symbols"
     (( ) ( )   ("[")  ("]")   ({)   (})  ( )     ( ) ( ) ( ) ( ) ( ) ( ) ( )
      ( ) (~)    ( )   ("'") ("\"") ("`") ( )     ( ) ( ) ( ) ( ) ( ) ( ) ( )
      ( ) (";")  (:)    (-)  ("(")  (")")             ( ) ( ) ( ) ( ) ( ) ( )
      ( ) ("/") ("\\")  (=)   (+)    (_)  ( )     ( ) ( ) ( ) ( ) ( ) ( ) ( )
      ( ) ( )    (|)    (<)   (>)                     ( ) ( ) ( ) ( ) ( )
                                     ( ) ( )     ( ) ( )
                                         ( )     ( )
                                 ( ) ( ) ( )     ( ) ( ) ( )))

    ("media"
     (   ( )       ( )        ( )       ( )        ( )      ( ) ( )     ( ) ( ) ( ) ( ) ( ) ( ) ( )
      (rgb_sad) (rgb_sai)     ( )     (ms_up)   (ms_wh_up)  ( ) ( )     ( ) ( ) ( ) ( ) ( ) ( ) ( )
      (rgb_vad) (rgb_vai ) (ms_left) (ms_down)  (ms_right)  ( )             ( ) ( ) ( ) ( ) ( ) ( )
      (rgb_hud) (rgb_hui)     ( )       ( )    (ms_wh_down) ( ) ( )     ( ) ( ) ( ) ( ) ( ) ( ) ( )
      (rgb_tog)    ( )        ( )       ( )        ( )                          ( ) ( ) ( ) ( ) ( )
                                                            ( ) ( )     ( ) ( )
                                                                ( )     ( )
                                      (ms_btn2) (ms_btn1) (ms_btn3)     ( ) ( ) ( )))))

(mugur-load-keybindings)

(defun signed-of-by-me ()
  (interactive)
  (insert "Signed-off-by: Mihai.Olteanu <Mihai.Olteanu@analog.com>"))
