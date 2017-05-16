;; packages
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

(use-package ace-jump-mode
  :ensure t
  :bind (("C-c SPC" . ace-jump-mode)))

(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window)))

(use-package beacon
  :ensure t
  :diminish beacon-mode
  :config
  (beacon-mode 1))

(use-package better-defaults
  :ensure t)

(use-package cider
  :ensure t
  :config
  (setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))"))

(use-package clojure-mode
  :ensure t)

(use-package clj-refactor
  :ensure t
  :config (add-hook 'clojure-mode-hook (lambda ()
                                       (clj-refactor-mode 1))))

(use-package company
  :ensure t
  :bind (("C-c /" . company-complete))
  :config (global-company-mode))

(use-package direx
  :ensure t)

(use-package helm-config
  :demand
  :bind-keymap ("C-c h" . helm-command-map))

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("C-x f" . helm-find-files)
         ("C-x r b" . helm-filtered-bookmarks)
         ("C-h a" . helm-apropos)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)
         ("M-y" . helm-show-kill-ring)
         :map helm-command-map
         ("o" . helm-occur))

  :config
  (setq helm-M-x-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t))

(use-package helm-projectile
  :ensure t)

(use-package js2-mode
  :ensure t)

(use-package json-mode
  :ensure t
  :config (setq json-reformat:indent-width 2))

(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status)))

(use-package monokai-theme
  :ensure t
  :config
  (blink-cursor-mode nil)
  (setq blink-cursor-blinks 2)
  (load-theme 'monokai t))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config (setq markdown-command "multimarkdown"))

(use-package multi-term
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :bind (("C-c m n" . mc/mark-next-like-this)
         ("C-c m a" . mc/mark-all-like-this)
         ("C-c m p" . mc/mark-previous-like-this)
         ("C-c m s" . mc/mark-next-symbol-like-this)
         ("C-c m S" . mc/mark-previous-symbol-like-this)))

(defun start-love-game ()
  (interactive)
  (shell-command "love ."))

(use-package lua-mode
  :ensure t
  :bind (("<f5>" . start-love-game))
  :mode "\\.lua\\'")

(use-package company-lua
  :ensure t
  :config
  (add-to-list 'company-backends 'company-lua))

(use-package paredit
  :ensure t
  :config
  (add-hook 'slime-repl-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook       'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook 'paredit-mode)
  (add-hook 'ielm-mode-hook             'paredit-mode)
  (add-hook 'lisp-mode-hook             'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook          'paredit-mode))

(use-package projectile
  :ensure t
  :config
  (projectile-mode))

(defun -goto-dashboard ()
  "Only display the dashboard"
  (interactive)
  (delete-other-windows)
  (switch-to-buffer "*dashboard*"))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-items '((recents  . 5)
                          (bookmarks . 10)
                          (projects . 10)))
  :bind (("<f12>" . -goto-dashboard)))

(use-package rainbow-delimiters
  :ensure t
  :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-mode))

(use-package restclient
  :ensure t)

(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup))

(use-package synosaurus
  :ensure t
  :config
  (setq synosaurus-backend 'synosaurus-backend-openthesaurus)
  (setq synosaurus-choose-method 'popup))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config (global-undo-tree-mode 1))

(use-package web-mode
  :ensure t)

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode))


(defvar b6n/environment 'default)

(use-package prettify-symbols
  :init
  (global-prettify-symbols-mode t)
  :config
  (setq prettify-symbols-alist
      '(("lambda" . ?λ)
        ("fn" . ?λ)
        ("<=" . ?≤)
        (">=" . ?≥))))

;; OS stuff
(setq x-select-enable-clipboard t)

;; Mac stuff
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; spelling
(setq ispell-program-name "aspell")
(ispell-change-dictionary "english")

;; Keyboard and Unicode stuff
(set-keyboard-coding-system 'iso-latin-1)

;; ido
(ido-mode)
(ido-everywhere 1)

;; Global key settings
(global-set-key (kbd "C-c q") 'join-line)
(global-set-key (kbd "C-x 4 m") 'bookmark-jump-other-window)

;; arrow keys
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)



;; org mode
(setq org-directory "~/org")
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-inbox-for-pull "~/org/flagged.org")

(add-hook 'org-mode-hook (lambda () (ispell-change-dictionary "de_DE-neu")))

(add-hook 'text-mode-hook 'flyspell-mode)

;; custom stuff

;; lisp stuff
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)


(defun b6n-on-mark-activated ()
  (setq cursor-type 'bar))

(defun b6n-on-mark-deactivated ()
  (setq cursor-type 'box))

(add-hook 'activate-mark-hook 'b6n-on-mark-activated)

(add-hook 'deactivate-mark-hook 'b6n-on-mark-deactivated)

;; windows
(when (eq system-type 'windows-nt)
  (setq null-device "/dev/null"))

(remove-hook 'text-mode-hook #'turn-on-auto-fill)

(linum-mode -1)

(defun b6n/linums-while-goto-line ()
  "Displays the line numbers and then invokes goto-line"
  (interactive)
  (let ((was-on linum-mode)
        (inhibit-quit t))
    (unless linum-mode
      (linum-mode 1))
    (with-local-quit
      (call-interactively 'goto-line))
    (unless was-on
      (linum-mode -1))
    (when inhibit-quit (setq quit-flag nil))))

(global-set-key (kbd "M-g g") 'b6n/linums-while-goto-line)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("97d039a52cfb190f4fd677f02f7d03cf7dbd353e08ac8a0cb991223b135ac4e6" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
