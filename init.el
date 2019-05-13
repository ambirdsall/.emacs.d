;; -*- lexical-binding: t; -*-

;; * handle the case where this is launched with `emacs -q --load ~/.emacs.amb/init.el`
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

;; ** TODO: default size
(when (display-graphic-p)
  (add-to-list 'default-frame-alist '(height . 72))
  (add-to-list 'default-frame-alist '(width . 150)))

;; ** folding plz
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; ** Which Key
(use-package which-key
  :straight t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode))

;; ** Theme
(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-one t))

(use-package all-the-icons
  :straight t)

(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode))

;; * Terminal sanity
(unless (display-graphic-p) (xterm-mouse-mode 1))

;; * Packages
;; ** Text Editing
(use-package evil
  :straight t
  :config
  (evil-mode 1)
  (setq evil-disable-insert-state-bindings t))

(use-package evil-surround
  :straight t
  :config (evil-surround-mode))
(use-package evil-visualstar
  :straight t
  :config (evil-visualstar-mode))
(use-package evil-commentary
  :straight t
  :config
  (evil-commentary-mode))
(use-package evil-unimpaired
  :straight (:host github :repo "CeleritasCelery/evil-unimpaired")
  :config (evil-unimpaired-mode))


(use-package smartparens
  :straight t
  :config
  (require 'smartparens-config)
  (add-hook 'prog-mode-hook 'smartparens-strict-mode))

;; ** Navigation
(use-package helm
  :straight t
  :defer t
  :init
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  (setq helm-candidate-number-list 50)
  :config (require 'helm-config))

;; TODO: helm-ag or w/e
(straight-use-package 'projectile)
(straight-use-package 'helm-projectile)
(straight-use-package 'helm-swoop)
(straight-use-package 'ace-jump-mode)


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
;; ** Org
(use-package org
  :straight t)

(use-package outshine
  :straight t
  :config
  (add-hook 'prog-mode-hook 'outshine-mode))

;; * Return of the macOS
(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (setq ns-use-proxy-icon  nil)
  (setq frame-title-format nil)
  (setq ns-command-modifier 'meta)
  (setq ns-option-modifier 'super))

;; * the all-important keybindings
;; ** custom commands...

(defun amb:find-file-dwim (restrict-to-open-buffers)
  "Grab the helm and go to a project file quickly.

If called with a prefix arg, restricts to open buffers; by default, any file."
  (interactive "P")
  (if restrict-to-open-buffers
      (call-interactively #'helm-buffers-list)
    (if (projectile-project-p)
        (call-interactively #'helm-projectile-find-file)
      (call-interactively #'helm-find-files))))

(defun amb:edit-init-file ()
  "Edit init file"
  (interactive)
  (find-file user-init-file))

;; ** ...and their keybindings
(use-package general
  :straight t
  :config (progn ;; set keybinding s in here, yo
;; *** global keybindings
      (general-define-key "M-x" 'helm-M-x)

;; *** package-specific keybindings
      (general-define-key
       :states '(normal visual insert emacs)
       :keymaps 'outshine-mode-map
       "<backtab>" 'outshine-cycle-buffer)

      (general-define-key
       :states '(normal visual insert emacs)
       :keymaps 'smartparens-mode-map
       "C-)" 'sp-forward-slurp-sexp)

      (general-define-key
       :keymaps 'helm-map
       "TAB" 'helm-execute-persistent-action
       "C-i" 'helm-execute-persistent-action ;; make tab work in terminals
       "C-z" 'helm-select-action)

      (general-define-key
       :states '(normal)
       "/" 'helm-swoop)

;; *** leader key keybindings
      (general-define-key
       :states '(normal visual insert emacs)
       :prefix "SPC"
       :non-normal-prefix "M-m"
       ;; "/"   '(counsel-rg :which-key "ripgrep") ; I'd need counsel for this
       "SPC" '(amb:find-file-dwim :which-key "find you a file")
       "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
       ":" '(helm-M-x :which-key "M-x")
       "." '((lambda ()(interactive)(dired ".")) :which-key "current directory")
       "p" '(:ignore t :which-key "project repos")
       "pf"  '(helm-projectile-find-file-dwim :which-key "find files")
       "ef" '(amb:edit-init-file :which-key "edit init.el")
       "eR" '((lambda ()(interactive)(load-file user-init-file)) :which-key "reload init.el")
       "fed" '(amb:edit-init-file :which-key "edit init.el")
       "fs"  '(save-buffer :which-key "save file")
       ;; git
       "g" '(:ignore t :which-key "git")
       "gs" '(magit-status :which-key "status")
       ;; Buffers
       "b" '(:ignore t :which-key "buffers")
       "bb"  '(helm-buffers-list :which-key "buffers list")
       "bd"  '(kill-buffer :which-key "kill buffer")
       ;; help
       "h" '(:ignore t :which-key "wtf help")
       "hf" '(describe-function :which-key "describe function")
       "hk" '(describe-key :which-key "describe key")
       "hv" '(describe-variable :which-key "describe variable")
       ;; jumping
       "j" '(:ignore t :which-key "jump")
       "jj" '(ace-jump-mode :which-key "jump to char")
       ;; quitting
       "q" '(:ignore t :which-key "morituri te salutant")
       "qq" 'kill-emacs
       ;; Window
       "w" '(:ignore t :which-key "windows")
       "wl"  '(windmove-right :which-key "move right")
       "wh"  '(windmove-left :which-key "move left")
       "wk"  '(windmove-up :which-key "move up")
       "wj"  '(windmove-down :which-key "move bottom")
       "wv"  '(split-window-right :which-key "split right")
       "ws"  '(split-window-below :which-key "split bottom")
       "wd"  '(delete-window :which-key "delete window"))))
