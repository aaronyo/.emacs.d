;;; init.el --- Aaron Boyd's Emacs init file
;;; Commentary:
;;


;;; Code:
(setq lexical-binding t)
(add-to-list 'load-path "~/.emacs.d/lisp")
(custom-set-default 'custom-file "~/.emacs.d/lisp/generated-customizations.el")

(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (toggle-scroll-bar -1)
      (fringe-mode -1)
      (custom-set-faces
       '(fringe ((t (:background "#181818")))))
      (toggle-frame-fullscreen)
      (set-window-margins nil 1)
      )
  nil)

(load "my/key-mappings")
(load "3p/init-use-package")
(require 'use-package-ensure)
(custom-set-default 'use-package-always-ensure t)


;; osx clipboard integration
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(cond
 ((string-equal system-type "darwin") ; Mac OS X
  (progn
    (setq interprogram-cut-function 'paste-to-osx)
    (setq interprogram-paste-function 'copy-from-osx))))


;; osx clipbard support for emacs in ssh terminal
(use-package clipetty
  :ensure t
  :bind ("M-w" . clipetty-kill-ring-save))


;; Backup file setup
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
                `((".*" ,temporary-file-directory t)))


(defvar-local window-background "#181818")

(use-package ivy
  :defer 0.1
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-x b" . ivy-switch-buffer)
         ("C-x B" . ivy-switch-buffer-other-window)
         )
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))

(use-package counsel
  :diminish
  :after (ivy)
  :config (counsel-mode))

(use-package ivy-rich
  :after (ivy)
  :init
  (setq ivy-rich-path-style 'abbrev
        ivy-virtual-abbreviate 'full)
  :config (ivy-rich-mode 1))


(use-package swiper
  :after (ivy)
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package amx
  :diminish
  :after (ivy)
  :config (amx-mode +1))

;; Auto Save setup
(custom-set-default 'backup-directory-alist
                    `((".*" . ,temporary-file-directory)))
(custom-set-default 'auto-save-file-name-transforms
                    `((".*" ,temporary-file-directory t)))

(use-package git-gutter
  :diminish " 𝙂"
  :bind
  (("C-x C-g" . git-gutter))
  :config
  (global-git-gutter-mode +1)
  )

;; Show column numbers
(column-number-mode 1)

;; Highlight matching paren
(show-paren-mode 1)

(menu-bar-mode -1)

;; Kill trailing white space
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; quiet, please! No dinging!
;; http://stuff-things.net/2015/10/05/emacs-visible-bell-work-around-on-os-x-el-capitan/
(setq visible-bell nil)
(setq ring-bell-function (lambda ()
			   (invert-face 'mode-line)
			   (run-with-timer 0.1 nil 'invert-face 'mode-line)))

;; Projectile
;; (setq projectile-enable-caching t)
(use-package projectile
  :diminish " 𝙋"
  :after (ivy)
  :custom
  (projectile-completion-system 'ivy)
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (projectile-mode +1))

;; Use shift-<arrow> to navigate windows
(windmove-default-keybindings)

(use-package multiple-cursors
  :bind
  (
   ("^[ [>" . 'mc/mark-next-like-this)
   ("^[ [<" . 'mc/mark-previous-like-this)
   ("C-c ^[ [<" . 'mc/mark-all-like-this)
   ("C->" . 'mc/mark-next-like-this)
   ("C-<" . 'mc/mark-previous-like-this)
   )
  )

;; Commenting regions
(global-set-key (kbd "C-x /") 'comment-region)
(global-set-key (kbd "C-x \\") 'uncomment-region)


(use-package neotree
  :config
  (setq neo-theme 'ascii)
  :bind
  (("<f8>" . 'neotree-toggle)
   ("<f9>" . 'neotree-find))
  )

(use-package elisp-slime-nav
  :diminish
  :init
  (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode))

(use-package company
  :diminish
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
  (defvar company-fci-mode-on-p nil)

  (defun company-turn-off-fci (&rest ignore)
    (when (boundp 'fci-mode)
      (setq-local company-fci-mode-on-p fci-mode)
      (when fci-mode (fci-mode -1))))

  (defun company-maybe-turn-on-fci (&rest ignore)
    (when company-fci-mode-on-p (fci-mode 1)))

  (add-hook 'company-completion-started-hook 'company-turn-off-fci)
  (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
  (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)
  ;; end: fix fxi alignment conflict
  )

(defun my/init-window-divider ()
  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?▐)))
(my/init-window-divider)

(defun my/update-window-divider ()
  (let ((display-table (or buffer-display-table standard-display-table)))
    (set-display-table-slot display-table 'vertical-border (make-glyph-code ?▐))
    (set-window-display-table (selected-window) display-table)))


(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))

(use-package fill-column-indicator
  :diminish fci-mode
  :custom
  (fci-rule-character ?│)
  (fci-rule-column 80)
  :init
  (add-hook 'fci-mode-hook #'my/update-window-divider))

(use-package flycheck
  :diminish " 𝙁"
  :custom
  (flycheck-idle-change-delay 2)
  (flycheck-check-syntax-automatically '(save mode-enabled idle-change))
  :config
  (defun my/flycheck-buffer-status ()
    (when (bound-and-true-p flycheck-mode)
      (let ((error-levels (mapcar 'flycheck-error-level flycheck-current-errors)))
        (cond
         ((seq-contains error-levels 'error) 'error)
         ((seq-contains error-levels 'warning) 'warning)
         ((seq-contains error-levels 'info) 'info)
         (t 'ok)))))
  )

(use-package zenburn-theme
  :after (git-gutter fill-column-indicator flycheck company)
  :custom
  (fci-rule-color "#352035")
  (git-gutter:modified-sign "▌")
  (git-gutter:added-sign "▌")
  (git-gutter:deleted-sign "▂")
  :init
  (setq zenburn-override-colors-alist
        '(("zenburn-bg"    . window-background)
          ("zenburn-bg-1"  . "#3c3c3c")
          ("zenburn-bg-05" . "#111111")
          ))
  :config
  (load-theme 'zenburn t)
  (custom-theme-set-faces
   'zenburn
   `(highlight ((t (:background , "#444444"))))
   `(highlight-indentation-face ((t (:background "#222")))))
  (set-face-inverse-video 'git-gutter:added nil)
  (set-face-inverse-video 'git-gutter:deleted nil)
  (set-face-inverse-video 'git-gutter:modified nil)
  (set-face-background 'git-gutter:added window-background)
  (set-face-background 'git-gutter:deleted window-background)
  (set-face-background 'git-gutter:modified window-background)
  (set-face-foreground 'git-gutter:added "#3f5f3f")
  (set-face-foreground 'git-gutter:deleted "#6f2f2f")
  (set-face-foreground 'git-gutter:modified "#3f3f7f")
  (set-face-foreground 'vertical-border "#000000")
  (set-face-background 'vertical-border "#111111")
  (set-face-foreground 'flycheck-error "#ff2222")
  (set-face-foreground 'flycheck-warning "#ffdd44")
  (set-face-foreground 'flycheck-info "#44ff44")
  (let ((item-bg "#333333")
        (item-selected-bg "#665a44"))
    (set-face-background 'company-tooltip item-bg)
    (set-face-background 'company-tooltip-selection item-selected-bg)
    (set-face-background 'company-tooltip-annotation item-bg)
    (set-face-background 'company-tooltip-annotation-selection item-selected-bg)
    (set-face-background 'company-scrollbar-bg "#dcdccc")
    (set-face-background 'company-scrollbar-fg "#878777"))
  )

(use-package dockerfile-mode
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package magit
  :custom
  (magit-log-margin '(t "%Y-%m-%d %I:%M %p" magit-log-margin-width t 18))
  :bind
  (("C-x g" . 'magit-status)))

(use-package rainbow-delimiters)

(defun my/setup-emacs-lisp-mode ()
  "Seteup 'emacs-lisp-mode'."
  (rainbow-delimiters-mode +1)
  (flycheck-mode +1))

(defun my/mode-line-buffer-id (fly-status)
  (let ((label "%b"))
    (cond
     ((eq fly-status 'error)
      (propertize label 'face 'flycheck-error))
     ((eq fly-status 'warning)
      (propertize label 'face 'flycheck-warning))
     ((eq fly-status 'info)
      (propertize label 'face 'flycheck-info))
     (t (propertize label 'face 'mode-line-buffer-id)))))

(custom-set-variables
 '(mode-line-buffer-identification
   '(:eval (my/mode-line-buffer-id (my/flycheck-buffer-status)))))

(setcdr (assq 'vc-mode mode-line-format)
        '((:eval (replace-regexp-in-string "^ Git"
                                           (format " %s "
                                                   (char-to-string #xe0a0))
                                           vc-mode))))

(add-hook 'prog-mode-hook 'fci-mode)
(add-hook 'prog-mode-hook 'flycheck-mode)
(add-hook 'prog-mode-hook 'git-gutter-mode)
;; global-auto-revert-mode does not seem to be working for me...
(add-hook 'prog-mode-hook 'auto-revert-mode)

(add-hook 'emacs-lisp-mode-hook #'my/setup-emacs-lisp-mode)

(custom-set-default 'checkdoc-force-docstrings-flag nil)

(global-set-key (kbd "C-x k") 'kill-this-buffer)

(use-package diminish
  :config
  (diminish 'eldoc-mode)
  (diminish 'auto-revert-mode " ↻"))


(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown")
  (setq markdown-open-command "/usr/local/bin/marked"))

(use-package web-mode
  :config
  (setq web-mode-markup-indent-offset 2))

(add-hook 'css-mode-hook
      (lambda ()
        (setq css-indent-offset 2)))

(load "my/performance")
(load "my/setup-js-editing")
(load "my/setup-python-editing")
(load "generated-customizations" t)


;;; init.el ends here
