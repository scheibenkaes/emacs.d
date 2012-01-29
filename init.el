(setq x-select-enable-clipboard t)
 
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(set-keyboard-coding-system 'iso-latin-1)
(if (window-system)
    (progn
      ;; "fix" the broken keyboard                                        
;      (global-set-key "\M-l" '(lambda () (interactive) (insert "@")))
;      (global-set-key "\M-5" '(lambda () (interactive) (insert "[")))
;      (global-set-key "\M-6" '(lambda () (interactive) (insert "]")))
;      (global-set-key "\M-7" '(lambda () (interactive) (insert "|")))
;      (global-set-key "\M-/" '(lambda () (interactive) (insert "\\")))
;      (global-set-key "\M-8" '(lambda () (interactive) (insert "{")))
;      (global-set-key "\M-9" '(lambda () (interactive) (insert "}")))
     ; (global-set-key "\M-n" '(lambda () (interactive) (insert "~")))
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier nil)
      ))

(global-linum-mode 1)
