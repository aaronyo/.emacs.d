;;; setup-python-editing -- configuration for JS and TypesScript editing
;;; Commentary:
;;

;;; Code:

(use-package jedi)

(use-package elpy
  :ensure t
  :init
  (add-hook 'elpy-mode-hook 'jedi:setup)
  (add-hook 'python-mode-hook (lambda () (auto-complete-mode -1)))
  (setq ac-modes (delq 'python-mode ac-modes))
  (elpy-enable)
  (setq elpy-rpc-virtualenv-path 'current)
  (add-to-list 'process-coding-system-alist '("python" . (utf-8 . utf-8)))
  )

;; Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(use-package blacken
  :init
  (setq blacken-line-length 79))

(use-package py-isort)

;;; setup-python-editing ends here
