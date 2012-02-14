(setq x-select-enable-clipboard t)
(setq ispell-program-name "aspell")
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; packages
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(set-keyboard-coding-system 'iso-latin-1)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit
                      starter-kit-bindings
                      starter-kit-lisp
                      undo-tree
                      yasnippets
                      yasnippet-bundle))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; modes
(undo-tree-mode 1)
