;;; init.el --- Aaron Boyd's Emacs init file

(setq lexical-binding t)

;; Backup file setup
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
                `((".*" ,temporary-file-directory t)))

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
;; I include the following code in my Emacs initialization file so that this process
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

(let ((script-dir (file-name-directory load-file-name)))
  (load-library (concat script-dir "custom/setup-js-editing"))
  )

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; ido and flx-ido
(use-package ido
  :hook 'after-init-hook
  :config
  (ido-mode +1)
  (ido-everywhere +1)
  (flx-ido-mode +1)
  ;;disable ido faces to see flx highlights.
  (custom-set-default 'ido-use-faces nil)
  (custom-set-variables
   '(ido-use-filename-at-point 'guess)
   '(ido-file-extensions-order '(".ts" ".js")
                               ))
  )

(use-package ido-completing-read+
  :config
  (ido-ubiquitous-mode +1))

(use-package amx
  :config
  (amx-mode +1))

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode +1)
  (custom-set-variables
   '(ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
  ))

;; Auto Save setup
(custom-set-default 'backup-directory-alist
                    `((".*" . ,temporary-file-directory)))
(custom-set-default 'auto-save-file-name-transforms
                    `((".*" ,temporary-file-directory t)))

(use-package git-gutter
  :config
  (global-git-gutter-mode +1))

;; Show column numbers
(column-number-mode 1)

;; Highlight matching paren
(show-paren-mode 1)

(menu-bar-mode -1)

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
(use-package projectile
  :config
  (projectile-mode +1)
  :bind
  (("C-x t" . 'projectile-find-file)))

;; Use shift-<arrow> to navigate windows
(windmove-default-keybindings)

(use-package multiple-cursors
  :bind
  (("^[ >" . 'mc/mark-next-like-this)
   ("^[ <" . 'mc/mark-previous-like-this)
   ("C-c ^[ <" . 'mc/mark-all-like-this))
  )

;; Commenting regions
(global-set-key (kbd "C-x /") 'comment-region)
(global-set-key (kbd "C-x \\") 'uncomment-region)


(use-package neotree
  :bind
  (("<f8>" . 'neotree-toggle)
   ("<f9>" . 'neotree-find))
  :config
  (neotree-toggle)
  )

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

(use-package elisp-slime-nav
  :init
  (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode))

(use-package company
  ;; align annotation to the right hand side
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t)

  :init
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

(defun my/init-window-divider ()
  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│)))

(defun my/update-window-divider ()
  (let ((display-table (or buffer-display-table standard-display-table)))
    (set-display-table-slot display-table 'vertical-border (make-glyph-code ?│))
    (set-window-display-table (selected-window) display-table)))

(add-hook 'fci-mode-hook #'my/update-window-divider)

(use-package zenburn-theme
  :custom
  (fci-rule-character ?│)
  (fci-rule-color "#352028")
  :init
  (my/init-window-divider)
  (setq zenburn-override-colors-alist
        '(("zenburn-bg"    . "#111111")
          ("zenburn-bg-1"  . "#444444")
          ("zenburn-bg+1"  . "#222222")
          ("zenburn-bg-05" . "#111111")
          ("zenburn-red-1" . "#ff4444")
          ))
  :config
  (load-theme 'zenburn t)
  (custom-theme-set-faces
   'zenburn
   `(highlight ((t (:background , "#444444")))))
  (set-face-foreground 'vertical-border "black")
)


(use-package magit
  :bind
  (("C-x g" . 'magit-status))
)

(use-package rainbow-delimiters)
(use-package flycheck)

(defun my/setup-emacs-lisp-mode ()
  "Seteup 'emacs-lisp-mode'."
  (rainbow-delimiters-mode +1)
  (flycheck-mode +1))

(add-hook 'prog-mode-hook 'fci-mode)
(add-hook 'emacs-lisp-mode-hook #'my/setup-emacs-lisp-mode)

(custom-set-default 'checkdoc-force-docstrings-flag nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-begin-commands (quote (self-insert-command)))
 '(company-idle-delay 0.1)
 '(company-minimum-prefix-length 2)
 '(company-show-numbers t)
 '(company-tooltip-align-annotations t)
 '(dired-use-ls-dired nil)
 '(fci-rule-character 9474)
 '(fci-rule-color "#352028")
 '(global-company-mode t)
 '(ido-file-extensions-order (quote (".ts" ".js")))
 '(ido-use-filename-at-point (quote guess))
 '(package-selected-packages
   (quote
    (prettier-js tide company flycheck-ghcmod haskell-mode smex rjsx-mode projectile neotree multiple-cursors markdown-mode json-mode ido-grid-mode git-gutter flx-ido fill-column-indicator context-coloring)))
 '(standard-indent 2))

 (provide 'init)
;;; init.el ends here
