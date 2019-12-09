;;; setup-js-eddint -- configuration for JS and TypesScript editing
;;; Commentary:
;;

;;; Code:

(setq lexical-binding t)
(custom-set-default 'use-package-always-ensure t)

(use-package prettier-js)
(use-package js2-mode)
(use-package tide)
(use-package json-mode)
(use-package context-coloring)
(use-package tide)

(setq-default indent-tabs-mode nil)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

(with-eval-after-load 'flycheck
  (flycheck-add-mode 'javascript-eslint 'typescript-mode))

(defun my/find-dominating (rel-path)
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                rel-path))
         (full-path (and root
                      (expand-file-name rel-path
                                        root))))
    full-path))

(defvar-local
  flycheck-javascript-eslint-executable nil)
(defun my/use-dominating-eslint ()
(setq-local
 flycheck-javascript-eslint-executable (my/find-dominating "bin/eslint")))

(defvar-local prettier-js-command nil)
(defun my/use-dominating-prettier ()
  (setq-local prettier-js-command (my/find-dominating "bin/prettier")))

(defvar-local tide-tsserver-executable nil)
(defun my/use-dominating-tsserver ()
  (setq-local tide-tsserver-executable (my/find-dominating "bin/tsserver")))

(defun my/setup-js2-mode ()
  (flycheck-mode +1)
  (fci-mode +1)
  (prettier-js-mode +1)
  (context-coloring-mode +1)
  (my/use-dominating-eslint)
  (my/use-dominating-prettier)
  )

(defun my/setup-tide-mode ()
  (my/use-dominating-tsserver)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (if (not (member 'javascript-eslint (flycheck-checker-get 'typescript-tide 'next-checkers)))
           (flycheck-add-next-checker 'typescript-tide 'javascript-eslint)
    nil)
  (company-mode +1)
  (my/use-dominating-prettier)
  (my/use-dominating-eslint)
  (prettier-js-mode +1)
  (fci-mode +1)
  )

;; eslint --fix is too slow to run with every save
(defun my/eslint-fix-file ()
  (message "eslint --fixing the file" (buffer-file-name))
  (shell-command (concat flycheck-javascript-eslint-executable " --fix " (buffer-file-name))))

(defun eslint-fix ()
  "Rewrite the current buffer's file using eslint-fix and then revert."
  (interactive)
  (my/eslint-fix-file)
  (revert-buffer t t))

(add-hook 'js2-mode-hook #'my/setup-js2-mode)
(add-hook 'typescript-mode-hook #'my/setup-tide-mode)

(setq typescript-indent-level 2)

;; Context coloring
;; A string color that is neutral when context coloring
(set-face-foreground 'font-lock-string-face "color-246")

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
 )
