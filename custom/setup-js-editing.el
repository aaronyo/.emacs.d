;; JS Editing
;; Use js2-mode for .js files
(setq-default indent-tabs-mode nil)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . js-mode))
;; Leave the error highlighting to ESLint
(setq
 js2-mode-show-parse-errors nil
 js2-mode-show-strict-warnings nil
 js2-basic-offset 2
 fci-rule-column 80
 fci-rule-character ?|
 fci-rule-color "color-236")
(add-hook 'js2-mode-hook 'flycheck-mode)
(add-hook 'js2-mode-hook 'fci-mode)
;; Set eslint executable based on buffer
(load-library "set-eslint-executable")

(require 'prettier-js)
(add-hook 'js-mode-hook
          (lambda ()
                        (add-hook 'before-save-hook 'prettier-before-save)))

;; Context coloring
;; A string color that is neutral when context coloring
(set-face-foreground 'font-lock-string-face "color-246")
;; Hook for js2-mode
(add-hook 'js2-mode-hook 'context-coloring-mode)
;;(require 'context-coloring)
