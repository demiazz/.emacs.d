;;--- Initialization

(use-package js2-mode
  :ensure t
  :pin "melpa-stable"
  :config
  (setq js2-mode-show-strict-warnings nil)
  (setq js2-basic-offset 2)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))
