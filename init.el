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

;; modes
(undo-tree-mode 1)
