;;----- Initialization

(use-package go-mode
  :ensure t
  :pin    "melpa-stable"
  :config
  (add-hook 'before-save-hook 'gofmt-before-save))
