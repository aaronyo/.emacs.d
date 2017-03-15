;; JS Editing
;; Use js2-mode for .js files
(setq-default indent-tabs-mode nil)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

(add-hook 'js2-mode-hook 'flycheck-mode)
(add-hook 'js2-mode-hook 'fci-mode)

;; Set eslint executable based on buffer
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "bin/eslint"))
         (eslint (and root
                      (expand-file-name "bin/eslint"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

;; Set prettier executable based on buffer
(defun my/use-prettier-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "bin/prettier"))
         (prettier (and root
                      (expand-file-name "bin/prettier"
                                        root))))
    (when (and prettier (file-executable-p prettier))
      (setq-local prettier-command prettier))))

(add-hook 'js2-mode-hook #'my/use-prettier-from-node-modules)



(require 'prettier-js)
(add-hook 'js-mode-hook
          (lambda ()
                        (add-hook 'before-save-hook 'prettier-before-save)))

;; Context coloring
;; A string color that is neutral when context coloring
(set-face-foreground 'font-lock-string-face "color-246")
;; Hook for js2-mode
(add-hook 'js2-mode-hook 'context-coloring-mode)

(setq
 ;; Leave the error highlighting to ESLint
 js2-mode-show-parse-errors nil
 js2-mode-show-strict-warnings nil

 js2-basic-offset 2

 ;; controls on-the-fly indent level for json-mode
 js-indent-level 2

 json-reformat:indent-width 2
 json-reformat:pretty-string\? t

 fci-rule-column 80
 fci-rule-character ?|
 fci-rule-color "color-236"

 prettier-target-mode "js2-mode"

 )
