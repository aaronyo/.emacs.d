;;; init-use-package.el --- Get started with use-package in emacs

;; Copyright (C) 2015 Gregory J Stein

;; Author: Gregory J Stein <gregory.j.stein@gmail.com>
;; Keywords: use-package
;; License: none, use this however you want without citation
;;

;; Code inspired by:
;;      http://stackoverflow.com/a/10093312/3672986
;;      http://www.lunaryorn.com/2015/01/06/my-emacs-configuration-with-use-package.html
;;      https://github.com/jwiegley/use-package


;;; Commentary:

;; As Sebastian Wiesner from http://www.lunaryorn.com/ points out, there is a "chicken
;; and egg" problem with use-package, which is capable of automatically downloading and
;; installing packages, but otherwise needs to be downloaded and installed manually.
;; I include the following code in my emacs initialization file so that this process
;; is automatic.


;;; Code:

;; Update package-archive lists
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

;; Install 'use-package' if necessary
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use-package
(eval-when-compile
  (require 'use-package))

;;; init-use-package.el ends here

(require 'use-package-ensure)
(setq use-package-always-ensure t)



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
(add-to-list 'load-path "~/.emacs.d/custom/")

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
;;(show-paren-mode 1)

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
;; (setq projectile-enable-caching t)
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

(load-library "setup-js-editing")

;; Custom colors
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(context-coloring-level-0-face ((t (:foreground "color-255"))))
 '(context-coloring-level-1-face ((t (:foreground "color-81"))))
 '(context-coloring-level-2-face ((t (:foreground "color-175"))))
 '(context-coloring-level-3-face ((t (:foreground "color-42"))))
 '(context-coloring-level-4-face ((t (:foreground "color-27"))))
 '(context-coloring-level-5-face ((t (:foreground "color-92"))))
 '(context-coloring-level-6-face ((t (:foreground "color-23")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (rainbow-delimiters elisp-slime-nav use-package prettier-js flycheck tide company flycheck-ghcmod haskell-mode smex rjsx-mode projectile neotree multiple-cursors markdown-mode json-mode ido-grid-mode git-gutter flx-ido fill-column-indicator context-coloring)))
 '(standard-indent 2))

(global-set-key [f8] 'neotree-toggle)
(global-set-key [f9] 'neotree-find)

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

(use-package elisp-slime-nav)

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)

  ;; align annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  ;; begin: fix fxi alignment conflict
  ;; https://github.com/company-mode/company-mode/issues/180
  (defvar-local company-fci-mode-on-p nil)

  (defun company-turn-off-fci (&rest ignore)
    (when (boundp 'fci-mode)
      (setq company-fci-mode-on-p fci-mode)
      (when fci-mode (fci-mode -1))))

  (defun company-maybe-turn-on-fci (&rest ignore)
    (when company-fci-mode-on-p (fci-mode 1)))

  (add-hook 'company-completion-started-hook 'company-turn-off-fci)
  (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
  (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)
  ;; end: fix fxi alignment conflict
  )

(use-package distinguished-theme
  :config
  (load-theme 'distinguished t))

(use-package rainbow-delimiters
  :init
    (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))
