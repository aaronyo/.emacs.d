;;; setup-python-editing -- configuration for JS and TypesScript editing
;;; Commentary:
;;

;;; Code:

(use-package elpy
  :ensure t
  :init
  (elpy-enable))


;; Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

;;; setup-python-editing ends here
