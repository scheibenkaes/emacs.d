;; OS stuff
(setq x-select-enable-clipboard t)

;; Mac stuff
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; spelling
(setq ispell-program-name "aspell")
; (ispell-change-dictionary "german8")

;; Keyboard and Unicode stuff
(set-keyboard-coding-system 'iso-latin-1)

;; packages
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit
                      starter-kit-eshell
                      starter-kit-bindings
                      starter-kit-lisp
                      undo-tree
                      js2-mode
                      clojure-mode
                      yasnippet
                      yasnippet-bundle))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; modes
(global-undo-tree-mode 1)

;; org mode

(setq org-directory "~/org")
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-mobile-inbox-for-pull "~/org/flagged.org")

;; auto complete
;; http://www.emacswiki.org/emacs/AutoComplete
;; Installation: http://cx4a.org/software/auto-complete/manual.html#Installation
(add-to-list 'load-path "~/.emacs.d/ac/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac/ac-dict")
(ac-config-default)



