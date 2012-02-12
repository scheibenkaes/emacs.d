(setq x-select-enable-clipboard t)
 
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(set-keyboard-coding-system 'iso-latin-1)
(if (window-system)
    (progn
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier nil)))

(setq ispell-program-name "aspell")
