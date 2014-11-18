(defvar b6n/environment 'default)

;; fonts
(set-face-attribute 'default nil :height 100)

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

(load-theme 'warm-night t)
(set-cursor-color "#e67e22")


;; modes
(global-undo-tree-mode 1)

(rainbow-delimiters-mode t)

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


