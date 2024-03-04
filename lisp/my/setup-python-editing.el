;;; setup-python-editing -- configuration for JS and TypesScript editing
;;; Commentary:
;;

;;; Code:


(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

(use-package python-black
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode))

;;(use-package jedi)

;; (use-package elpy
;;   :ensure t
;;   :init
;; ;;  (add-hook 'elpy-mode-hook 'jedi:setup)
;;   (add-hook 'python-mode-hook (lambda () (auto-complete-mode -1)))
;;   (elpy-enable)
;; ;;  (setq elpy-rpc-virtualenv-path 'current)
;;   (add-to-list 'process-coding-system-alist '("python3.12" . (utf-8 . utf-8)))
;;   )

;; ;; Enable Flycheck
;; (when (require 'flycheck nil t)
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode))

;; (use-package blacken
;;   :init
;;   (setq blacken-line-length 79))

;; (use-package py-isort)

;;; setup-python-editing ends here
