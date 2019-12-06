;; JS Editing
;; Use js2-mode for .js files

(setq-default indent-tabs-mode nil)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

(add-hook 'js2-mode-hook 'flycheck-mode)
(add-hook 'js2-mode-hook 'fci-mode)

(add-hook 'rjsx-mode-hook 'flycheck-mode)

;; (add-hook 'rjsx-mode-hook 'fci-mode)


(defun my/find-dominating (rel-path)
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                rel-path))
         (full-path (and root
                      (expand-file-name rel-path
                                        root))))
    full-path))

;; Set eslint executable based on buffer
(defun my/use-dominating-eslint ()
  (setq-local flycheck-javascript-eslint-executable (my/find-dominating "bin/eslint")))

(add-hook 'flycheck-mode-hook #'my/use-dominating-eslint)

;; Set prettier executable based on buffer
(defun my/use-dominating-prettier ()
  (setq-local prettier-js-command (my/find-dominating "bin/prettier")))

(add-hook 'js2-mode-hook #'my/use-dominating-prettier)
(add-hook 'rjsx-mode-hook #'my/use-dominating-prettier)

;;
;; Tide and typescript for esling
;;

;; Set prettier executable based on buffer
(defun my/use-dominating-tsserver ()
  (setq-local tide-tsserver-executable (my/find-dominating "bin/tsserver")))

(with-eval-after-load 'flycheck
  (flycheck-add-mode 'javascript-eslint 'typescript-mode))

(defun setup-tide-mode ()
  (my/use-dominating-tsserver)
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (if (not (member 'javascript-eslint (flycheck-checker-get 'typescript-tide 'next-checkers)))
           (flycheck-add-next-checker 'typescript-tide 'javascript-eslint)
    nil)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(defun eslint-fix-file ()
  (message "eslint --fixing the file" (buffer-file-name))
  (shell-command (concat flycheck-javascript-eslint-executable " --fix " (buffer-file-name))))

(defun eslint-fix ()
  (interactive)
  (eslint-fix-file)
  (revert-buffer t t))


;; formats the buffer before saving
;; -- nope -- Let prettier do it
;; (add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'typescript-mode-hook #'my/use-dominating-prettier)
(add-hook 'typescript-mode-hook #'my/use-dominating-eslint)
(add-hook 'typescript-mode-hook 'prettier-js-mode)
(add-hook 'typescript-mode-hook 'fci-mode)

(setq typescript-indent-level 2)


;;(require 'prettier-js)
(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'rjsx-mode-hook 'prettier-js-mode)

;; (add-hook 'prettier-js-mode
;;           (lambda ()
;;             (add-hook 'before-save-hook 'prettier-before-save)))

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

;; prettier-target-mode "prettier-js-mode"

 )
