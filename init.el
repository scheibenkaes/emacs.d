
;; packages
(require 'package)

;(add-to-list 'package-archives '("tromey" . "https://tromey.com/elpa/"))
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

(use-package better-defaults
  :ensure t)

(use-package cider
  :ensure t
  :init
  (setq cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))"))

(use-package clojure-mode
  :ensure t)

(use-package clj-refactor
  :ensure t)

(use-package color-theme-sanityinc-tomorrow
  :ensure t)

(use-package company
  :ensure t
  :init (global-company-mode))

(use-package direx
  :ensure t)

(use-package js2-mode
  :ensure t)

(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status)))

(use-package paredit
  :ensure t
  :init
  (add-hook 'slime-repl-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook       'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook 'paredit-mode)
  (add-hook 'ielm-mode-hook             'paredit-mode)
  (add-hook 'lisp-mode-hook             'paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook          'paredit-mode))

(use-package rainbow-delimiters
  :ensure t
  :init (rainbow-delimiters-mode t))

(use-package restclient
  :ensure t)

(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)
         ;; old M-x
         ("C-c C-c M-x" . execute-extended-command)))

(use-package synosaurus
  :ensure t)

(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode 1))

(use-package web-mode
  :ensure t)

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode))


(defvar b6n/environment 'default)


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
(global-set-key (kbd "C-x f") 'ido-find-file)
(global-set-key (kbd "C-x 4 m") 'bookmark-jump-other-window)

;; arrow keys
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)


;; javascript
;; https://github.com/mooz/js2-mode
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; themes
(blink-cursor-mode nil)
(load-theme 'sanityinc-tomorrow-day t)


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
    ("97d039a52cfb190f4fd677f02f7d03cf7dbd353e08ac8a0cb991223b135ac4e6" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default)))
 '(synosaurus-backend (quote synosaurus-backend-openthesaurus))
 '(synosaurus-choose-method (quote popup)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
