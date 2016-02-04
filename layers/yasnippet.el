;;----- Initialization

(use-package yasnippet
  :ensure t
  :pin    "melpa-stable"
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode))
