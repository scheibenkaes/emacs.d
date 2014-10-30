;; fonts
(set-face-attribute 'default nil :height 100)

;; OS stuff
(setq x-select-enable-clipboard t)

;; Mac stuff
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; spelling
(setq ispell-program-name "aspell")
(ispell-change-dictionary "en")

;; Keyboard and Unicode stuff
(set-keyboard-coding-system 'iso-latin-1)


;; javascript
;; https://github.com/mooz/js2-mode
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; packages
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(ac-slime
    ace-jump-mode
    clojure-mode
    warm-night-theme
    rainbow-delimiters
    starter-kit
    starter-kit-bindings
    starter-kit-eshell
    starter-kit-lisp
    undo-tree
    yasnippet
    yasnippet-bundle
    ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; themes

(load-theme 'warm-night)
(set-cursor-color "#e67e22")


;; modes
(global-undo-tree-mode 1)

(rainbow-delimiters-mode)

;; yas

(setq yas/root-directory "~/.emacs.d/yas-snippets")

(yas/load-directory yas/root-directory)

;; org mode

(setq org-directory "~/org")
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-inbox-for-pull "~/org/flagged.org")

(add-hook 'org-mode-hook (lambda () (ispell-change-dictionary "german8")))

;; auto complete
;; http://www.emacswiki.org/emacs/AutoComplete
;; Installation: http://cx4a.org/software/auto-complete/manual.html#Installation
(add-to-list 'load-path "~/.emacs.d/ac/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac/ac-dict")
(ac-config-default)

;; ac-slime

(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

;; ace-jump
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; custom stuff

(add-hook 'slime-repl-mode-hook 'paredit-mode)

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

(defun linums-while-goto-line ()
  "Displays the line numbers and then invokes goto-line"
  (interactive)
  (let ((was-on linum-mode))
    (unless linum-mode
      (global-linum-mode 1))
    (call-interactively 'goto-line)
    (unless was-on
      (global-linum-mode -1))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("6209442746f8ec6c24c4e4e8a8646b6324594308568f8582907d0f8f0260c3ae" "fe6330ecf168de137bb5eddbf9faae1ec123787b5489c14fa5fa627de1d9f82b" "c3284c1f58b2be08d30bad4780bc052c53e6b06ad5f6ca68d23f3d1a07c8d013" "d921083fbcd13748dd1eb638f66563d564762606f6ea4389ea9328b6f92723b7" "e16a771a13a202ee6e276d06098bc77f008b73bbac4d526f160faa2d76c1dd0e" "21d9280256d9d3cf79cbcf62c3e7f3f243209e6251b215aede5026e0c5ad853f" "68769179097d800e415631967544f8b2001dae07972939446e21438b1010748c" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
