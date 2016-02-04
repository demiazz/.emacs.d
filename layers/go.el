;;----- Initialization

(use-package go-mode
  :ensure t
  :pin    "melpa-stable"
  :config
  (add-hook 'before-save-hook 'gofmt-before-save))

(when (demiazz/layer-used-p 'company)
  (use-package company-go
    :ensure t
    :pin    "melpa-stable"
    :config
    (add-hook 'go-mode-hook (lambda ()
                              (set
                               (make-local-variable 'company-backends)
                               '(company-go))))))
