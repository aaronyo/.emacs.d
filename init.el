;; Add package repos
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(setq dired-use-ls-dired nil)

;; ido and flx-ido
(add-hook 'after-init-hook
	  (lambda()
	    (ido-mode 1)
	    (ido-everywhere 1)
	    (flx-ido-mode 1)
	    ;; disable ido faces to see flx highlights.
	    (setq ido-use-faces nil)))

;; Auto Save setup
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
                `((".*" ,temporary-file-directory t)))
(add-to-list 'load-path "~/.emacs.d/3p/")

;; git gutter
(add-hook 'after-init-hook
	  (lambda()
            (global-git-gutter-mode +1)))

;; Backup file setup
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
                `((".*" ,temporary-file-directory t)))

;; Show column numbers
(column-number-mode 1)

;; Highlight matching paren
(show-paren-mode 1)

;; Kill trailing white space
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Smex - Meta-x using ido
;; Loading is delayed until first use
(autoload 'smex "smex"
    "Smex is a M-x enhancement for Emacs, it provides a convenient interface to
your recently and most frequently used commands.")
(global-set-key (kbd "M-x") 'smex)

;; quiet, please! No dinging!
;; http://stuff-things.net/2015/10/05/emacs-visible-bell-work-around-on-os-x-el-capitan/
(setq visible-bell nil)
(setq ring-bell-function (lambda ()
			   (invert-face 'mode-line)
			   (run-with-timer 0.1 nil 'invert-face 'mode-line)))

;; Projectile
(setq projectile-enable-caching t)
(add-hook 'after-init-hook #'projectile-global-mode)
(global-set-key (kbd "C-x t") 'projectile-find-file)

;; Window navigation
(global-set-key (kbd "ESC <left>") 'windmove-left)
(global-set-key (kbd "ESC <right>") 'windmove-right)
(global-set-key (kbd "ESC <up>") 'windmove-up)
(global-set-key (kbd "ESC <down>") 'windmove-down)

;; Multiple cursors
(global-set-key (kbd "^[ >") 'mc/mark-next-like-this)
(global-set-key (kbd "^[ <") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c ^[ <") 'mc/mark-all-like-this)

;; Commenting regions
(global-set-key (kbd "C-x /") 'comment-region)
(global-set-key (kbd "C-x \\") 'uncomment-region)


;; ESLint -- look in node_modules for executable
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
		(or (buffer-file-name) default-directory)
		"node_modules"))
	 (eslint (and root
		      (expand-file-name "node_modules/eslint/bin/eslint.js"
					root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

;; JS Editing
;; Use js2-mode for .js files
(setq-default indent-tabs-mode nil)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . js2-mode))
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

;; Context coloring
;; A string color that is neutral when context coloring
(set-face-foreground 'font-lock-string-face "color-246")
;; Hook for js2-mode
(add-hook 'js2-mode-hook 'context-coloring-mode)
;;(require 'context-coloring)

;; Custom colors
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(col-highlight ((t (:background "color-233"))))
 '(context-coloring-level-0-face ((t (:foreground "color-255"))))
 '(context-coloring-level-1-face ((t (:foreground "color-81"))))
 '(context-coloring-level-2-face ((t (:foreground "color-175"))))
 '(context-coloring-level-3-face ((t (:foreground "color-42"))))
 '(context-coloring-level-4-face ((t (:foreground "color-27"))))
 '(context-coloring-level-5-face ((t (:foreground "color-92"))))
 '(context-coloring-level-6-face ((t (:foreground "color-23"))))
 '(font-lock-comment-face ((t (:foreground "color-95"))))
 '(hl-line ((t (:background "color-233"))))
 '(lazy-highlight ((t (:background "black" :foreground "white" :underline t))))
 '(neo-dir-link-face ((t (:foreground "cyan"))))
 '(neo-file-link-face ((t (:foreground "white")))))

;; neo tree
(global-set-key [f8] 'neotree-toggle)
(global-set-key [f9] 'neotree-find)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(prettier-args (\` ("--single-quote=true" "--trailing-comma=false")))
 '(prettier-target-mode "js2-mode"))

;; neetree colors
;; defaults were too dark


;; osx clipboard integration
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)


(require 'prettier-js)
(add-hook 'js-mode-hook
          (lambda ()
                        (add-hook 'before-save-hook 'prettier-before-save)))
