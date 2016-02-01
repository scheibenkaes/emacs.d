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

;; Git
(setq b6n-custom-git-install (concat (expand-file-name "~") "/dev/git/"))

(defun b6n-setup-custom-git-env ()
  (message "Setting up custom git env")
  (setenv "GIT_EXEC_PATH" (concat (expand-file-name "~") "/dev/git/"))
  (setq magit-git-executable "~/dev/git/git")
  (global-set-key (kbd "C-c g") 'magit-status))

(if (not (file-exists-p b6n-custom-git-install))
  (warn "No default GIT install found. A new version of git is required for magit")
  (b6n-setup-custom-git-env))

;; ido
(ido-mode)
(ido-everywhere 1)

;; Global key settings
(global-set-key (kbd "C-c q") 'join-line)
(global-set-key (kbd "C-x f") 'ido-find-file)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-x 4 m") 'bookmark-jump-other-window)

;; arrow keys
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; javascript
;; https://github.com/mooz/js2-mode
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; packages
(require 'package)
; Ignoring marmalade right now, seems that it's redundant
;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("tromey" . "https://tromey.com/elpa/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(auto-complete
    slime
    ace-jump-mode
    ace-window
    clojure-mode
    color-theme-sanityinc-tomorrow
    rainbow-delimiters
    better-defaults
    js2-mode
    undo-tree
    smex
    direx
    paredit
    yasnippet))

(defun install-packages-from-list (ps)
  (dolist (p ps)
    (when (not (package-installed-p p))
      (package-install p))))

(install-packages-from-list my-packages)

;; additional packages

(defvar my-additional-packages
  '(feature-mode
    restclient
    web-mode
    company
    synosaurus
    clj-refactor) "Packages which are not needed on all machines")

;; themes
(blink-cursor-mode nil)
(load-theme 'sanityinc-tomorrow-bright t)

;; modes
(global-undo-tree-mode 1)

(rainbow-delimiters-mode t)

;; yas
(yas-global-mode)

;; cider

(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)

;; org mode
(setq org-directory "~/org")
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-inbox-for-pull "~/org/flagged.org")

(add-hook 'org-mode-hook (lambda () (ispell-change-dictionary "de_DE-neu")))



;; auto complete
(global-company-mode)

;; ace-jump
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; ace-window
(if (package-installed-p 'ace-window)
    (global-set-key (kbd "C-x o") 'ace-window)
  (message "No ace-window using plain old %s" "C-x o"))

;; custom stuff

;; paredit
(add-hook 'slime-repl-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook       'paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook 'paredit-mode)
(add-hook 'ielm-mode-hook             'paredit-mode)
(add-hook 'lisp-mode-hook             'paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook          'paredit-mode)

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


