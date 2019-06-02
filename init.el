;; -*- lexical-binding: t; -*-

;; * just in case this was launched with `emacs -q --load ~/.emacs.amb/init.el`
(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

;; * Bootstrap straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; * UI
;; ** Get rid of the dated stuff
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

;; ** default size
(when (display-graphic-p)
  (add-to-list 'default-frame-alist '(height . 72))
  (add-to-list 'default-frame-alist '(width . 150)))

;; ** dired
(add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode 1)))

;; ** folding plz
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; ** Which Key
(use-package which-key
  :straight t
  :init (setq which-key-separator " "
	      which-key-prefix-prefix "+")
  :config (which-key-mode))

;; ** Theme
(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-one-light t)
  (load-theme 'doom-one t))

(defvar *darkmode* t)

(defun amb:dark-mode ()
  "Sets the theme to doom-one."
  (interactive)
  (enable-theme 'doom-one)
  (setq *darkmode* t))

(defun amb:light-mode ()
  "Sets the theme to doom-one-light."
  (interactive)
  (enable-theme 'doom-one-light)
  (setq *darkmode* nil))

(defun amb:toggle-theme ()
  "Toggles the current theme between doom-one and doom-one-light."
  (interactive)
  (if (eq *darkmode* t)
      (amb:light-mode)
    (amb:dark-mode)))

(use-package all-the-icons
  :straight t)

(use-package doom-modeline
  :straight t
  :init (setq doom-modeline-buffer-file-name-style 'truncate-from-project)
  :config (doom-modeline-mode 1))

;; ** a non-audible, non-obnoxious bell
(setq ring-bell-function
      (lambda ()
	(let ((orig-fg (face-foreground 'mode-line)))
	  (set-face-foreground 'mode-line "#F2804F")
	  (run-with-idle-timer 0.1 nil
			       (lambda (fg) (set-face-foreground 'mode-line fg))
			       orig-fg))))
;; * Terminal sanity
(unless (display-graphic-p) (xterm-mouse-mode 1))

;; * Packages
;; ** Text Editing
;; *** evil
(use-package evil
  :straight t
  :init (setq evil-disable-insert-state-bindings t)
  :config (evil-mode 1))

(use-package evil-surround
  :after evil
  :straight t
  :config (evil-surround-mode))
(use-package evil-visualstar
  :after evil
  :straight t
  :config (evil-visualstar-mode))
(use-package evil-commentary
  :after evil
  :straight t
  :config
  (evil-commentary-mode))
(use-package evil-unimpaired
  :after evil
  :straight (:host github :repo "CeleritasCelery/evil-unimpaired")
  :config (evil-unimpaired-mode))
(use-package evil-replace-with-register
  :after evil
  :straight t
  :init (setq evil-replace-with-register-key (kbd "gr"))
  :config (evil-replace-with-register-install))
(use-package evil-exchange
  :after evil
  :straight t
  :config (evil-exchange-install))
(use-package evil-collection
  :after evil
  :straight t
  :init (setq evil-want-keybinding nil)
  :config (evil-collection-init))

;; *** smartparens
(use-package smartparens
  :straight t
  :hook (prog-mode . smartparens-strict-mode)
  :config (require 'smartparens-config))

;; *** autocompletion
(use-package company
  :straight t
  :config (global-company-mode 1))

;; *** editorconfig
(use-package editorconfig
  :straight t
  :defer t
  :config (editor-config-mode 1))

;; *** undo
(use-package undo-tree
  :straight t
  :defer t)

;; ** Navigation
(use-package helm
  :straight t
  :defer t
  :init
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  (setq helm-candidate-number-list 50))

(require 'helm-config)
(helm-mode 1)

;; TODO: helm-ag or w/e
(straight-use-package 'projectile)
(straight-use-package 'helm-projectile)
(straight-use-package 'helm-swoop)
(straight-use-package 'ace-jump-mode)
(use-package ace-link
  :straight t
  :config (ace-link-setup-default))

(use-package avy
  :straight t
  :defer t)

;; Play nicely with tmux
(use-package tmux-pane
  :straight t
  :config
  (tmux-pane-mode))

;; ** Git
(use-package magit
  :straight t)

(use-package evil-magit
  :straight t)
;; ** Org and outline/outshine
(use-package org
  :straight t)

(use-package outshine
  :straight t
  :hook prog-mode)

;; * Return of the macOS
(setq is-mac (eq system-type 'darwin))
(when is-mac
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (setq ns-use-proxy-icon nil)
  (setq frame-title-format nil)
  (setq ns-command-modifier 'meta)
  (setq ns-option-modifier 'super))

;; * the all-important keybindings ** custom commands..
;; ** custom functions...
(defun amb:find-file-dwim (restrict-to-open-buffers)
  "Grab the helm and go to a project file quickly.

If called with a prefix arg, restricts to open buffers; by default, any file."
  (interactive "P")
  (if restrict-to-open-buffers
      (call-interactively #'helm-buffers-list)
    (if (projectile-project-p)
        (call-interactively #'helm-projectile-find-file-dwim)
      (call-interactively #'helm-find-files))))

(defun amb:edit-init-file ()
  "Edit init file"
  (interactive)
  (find-file user-init-file))

;; ** ...and their keybindings
;; reclaim M-0, M-1, ..., M-9
(dotimes (n 10)
  (global-unset-key (kbd (format "M-%d" n))))

(use-package general
  :straight t
  :config (progn ;; let's go crazy
;; *** global
      (general-define-key "M-x" 'helm-M-x)

      (general-define-key
       :states '(normal)
       "/" 'helm-swoop)

;; *** macOS-specific
      (when is-mac
        (general-define-key "M-`" 'other-frame))

;; *** evil-state-specific
      (general-define-key
       :states '(insert replace)
       "C-h" 'help-command
       "C-k" 'kill-line)

      (general-define-key
       :states '(normal)
       "U" 'undo-tree-visualize)

;; *** package-specific
      (general-define-key
       :keymaps 'dired-mode-map
       :states '(normal)
       "h" 'dired-up-directory
       "l" 'dired-find-file)

      (general-define-key
       :keymaps 'helm-map
       "TAB" 'helm-execute-persistent-action
       "C-i" 'helm-execute-persistent-action ;; make tab work in terminals
       "C-z" 'helm-select-action)

      (general-define-key
       :keymaps 'outshine-mode-map
       :states '(normal visual insert emacs)
       "<tab>" 'outshine-cycle
       "<backtab>" 'outshine-cycle-buffer)

      (general-define-key
       :keymaps 'smartparens-mode-map
       :states '(normal visual insert emacs)
       "C-)" 'sp-forward-slurp-sexp)

;; *** leader key
      (general-define-key
       :states '(normal visual insert emacs)
       :prefix "SPC"
       :non-normal-prefix "M-m"
       ;; "/"     '(counsel-rg :which-key "ripgrep") ; I'd need counsel for this
       "SPC" '(amb:find-file-dwim :which-key "find you a file")
       "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
       ":" '(helm-M-x :which-key "M-x")
       "." '((lambda ()(interactive)(dired ".")) :which-key "current directory")
       "U" '(undo-tree-visualize :which-key "undo-tree")

       "b" '(:ignore t :which-key "buffers")
       "bb" '(helm-buffers-list :which-key "buffers list")
       "bd" '(kill-buffer :which-key "kill buffer")
       "bs" '((lambda ()(interactive)(switch-to-buffer "*scratch*")) :which-key "scratch buffer")

       "e" '(:ignore t :which-key "emacs/init")
       "ef" '(amb:edit-init-file :which-key "edit init.el")
       "eR" '((lambda ()(interactive)(load-file user-init-file)) :which-key "reload init.el")

       "f" '(:ignore t :which-key "files")
       "fed" '(amb:edit-init-file :which-key "edit init.el")
       "ff" '(helm-find-files :which-key "find file")
       "fs" '(save-buffer :which-key "save file")

       "g" '(:ignore t :which-key "git")
       "gm" 'magit-dispatch
       "gs" '(magit-status :which-key "status")
       "gS" '(magit-stage-file :which-key "stage file")

       "h" '(help-command :which-key "halp, wtf")

       "j" '(:ignore t :which-key "jump")
       "jj" '(avy-goto-char-timer :which-key "jump to [start typing]")
       "jc" '(ace-jump-mode :which-key "jump to char")

       "o" '(:ignore t :which-key "")
       "of" '(evil-first-non-blank :which-key "goto first non-blank")

       "p" '(:ignore t :which-key "project repos")
       "pf"    '(helm-projectile-find-file-dwim :which-key "find files")

       "q" '(:ignore t :which-key "morituri te salutant")
       "qq" 'kill-emacs

       "t" '(:ignore t :which-key "toggles")
       "tD" 'toggle-debug-on-error
       "tt" '(amb:toggle-theme :which-key "light/dark theme")

       "w" '(:ignore t :which-key "windows")
       "wl" '(windmove-right :which-key "move right")
       "wm" '(delete-other-windows :which-key "only me")
       "wh" '(windmove-left :which-key "move left")
       "wk" '(windmove-up :which-key "move up")
       "wj" '(windmove-down :which-key "move bottom")
       "wv" '(split-window-right :which-key "split right")
       "ws" '(split-window-below :which-key "split bottom")
       "wd" '(delete-window :which-key "delete window")

       ")" '(:ignore t :which-key "smartparens")
       ")b" '(sp-forward-barf-sexp :which-key "barf forwards")
       ")s" '(sp-forward-slurp-sexp :which-key "slurp forwards")

       "(" '(:ignore t :which-key "smartparens")
       "(b" '(sp-backward-barf-sexp :which-key "barf backwards")
       "(s" '(sp-backward-slurp-sexp :which-key "slurp backwards"))))
