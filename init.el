;; Minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

;; Bootstrap straight
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

;; Vim things up
(use-package evil
  :straight t
  :config
  (evil-mode 1))

;; Theme
(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-one t))

;; Helm
(use-package helm
  :straight t
  :init
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  (setq helm-candidate-number-list 50))

;; Which Key
(use-package which-key
  :straight t
  :init
  (setq which-key-separator " ")
  (setq which-key-prefix-prefix "+")
  :config
  (which-key-mode))

;; Return of the macOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)
(setq ns-command-modifier 'meta)
(setq ns-option-modifier 'super)

;; the all-important keybindings
(use-package general
  :straight t
  :config (general-define-key
  :states '(normal visual insert emacs)
  :prefix "SPC"
  :non-normal-prefix "M-m"
  ;; "/"   '(counsel-rg :which-key "ripgrep") ; I'd need counsel for this
  "TAB" '(switch-to-prev-buffer :which-key "previous buffer")
  ":" '(helm-M-x :which-key "M-x")
  "pf"  '(helm-find-file :which-key "find files")
  "fs"  '(save-buffer :which-key "save file")
  ;; Buffers
  "bb"  '(helm-buffers-list :which-key "buffers list")
  ;; Window
  "wl"  '(windmove-right :which-key "move right")
  "wh"  '(windmove-left :which-key "move left")
  "wk"  '(windmove-up :which-key "move up")
  "wj"  '(windmove-down :which-key "move bottom")
  "wv"  '(split-window-right :which-key "split right")
  "ws"  '(split-window-below :which-key "split bottom")
  "wx"  '(delete-window :which-key "delete window")
))

