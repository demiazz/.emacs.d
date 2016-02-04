;;----- Initialization

(use-package company
  :ensure t
  :pin    "melpa-stable"
  :config
  (add-hook 'after-init-hook 'global-company-mode))
