;;----- Settings

(demiazz/default-settings helm
                          position  'bottom
                          resize    nil
                          no-header nil)

;;----- Initialization

;; helm

(use-package helm
  :ensure t
  :pin    "melpa-stable"
  :config
  
  (progn
    (when (and l-helm//resize
               (or (eq l-helm//position 'bottom))
               (or (eq l-helm//position 'top)))
      (setq helm-autoresize-min-height 10)
      (helm-autoresize-mode 1))
    
    (defvar helm-source-header-default-background
      (face-attribute 'helm-source-header :background))
    (defvar helm-source-header-default-foreground
      (face-attribute 'helm-source-header :foreground))
    (defvar helm-source-header-default-box
      (face-attribute 'helm-source-header :box))
    (defvar helm-source-header-default-height
      (face-attribute 'helm-source-header :height))

    (defun l-helm//toggle-header-line ()
      "Hide the `helm` header is there is only one source."
      (when l-helm//no-header
        (if (> (length helm-sources) 1)
            (set-face-attribute 'helm-source-header
                                nil
                                :foreground helm-source-header-default-foreground
                                :background helm-source-header-default-background
                                :box        helm-source-header-box
                                :height     helm-source-header-height)
          (set-face-attribute 'helm-source-header
                              nil
                              :foreground (face-attribute 'helm-selection :background)
                              :background (face-attribute 'helm-selection :background)
                              :box        nil
                              :height     0.1))))

    (add-hook 'helm-before-initialize-hook 'l-helm//toggle-header-line))
  
  :init
  
  (progn
    (setq helm-prevent-escaping-from-minibuffer    t
          helm-bookmark-show-location              t
          helm-display-header-line                 nil
          helm-split-window-in-side-p              t
          helm-always-two-windows                  t
          helm-echo-input-in-header-line           t
          helm-imenu-execute-action-at-once-if-one t)

    ;; fuzzy matching settings

    (setq helm-M-x-fuzzy-match        t
          helm-apropos-fuzzy-match    t
          helm-file-cache-fuzzy-match t
          helm-imenu-fuzzy-match      t
          helm-lisp-fuzzy-completion  t
          helm-recentf-fuzzy-match    t
          helm-semantic-fuzzy-match   t
          helm-buffers-fuzzy-matching t)

    (defun l-helm//hide-cursor-in-helm-buffer ()
      "Hide the cursor in helm buffers."
      (with-helm-buffer
        (setq cursor-in-non-selected-windows nil)))

    (add-hook 'helm-after-initialize-hook 'l-helm//hide-cursor-in-helm-buffer))

  :config

  (progn
    (helm-mode +1)

    (helm-locate-set-command)
    (setq helm-locate-fuzzy-match (string-match "locate" helm-locate-command))

    (defun l-helm//set-dotted-directory ()
      "Set the face of directories for `.' and `..'"
      (set-face-attribute 'helm-ff-dotted-directory
                          nil
                          :foreground nil
                          :background nil
                          :inherit    'helm-ff-directory))

    (add-hook 'helm-find-files-before-init-hook 'l-helm//set-dotted-directory)

    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
    (define-key helm-find-files-map (kbd "S-<tab>") 'helm-find-files-up-one-level)
    (define-key helm-find-files-map (kbd "<backtab>") 'helm-find-files-up-one-level)))

;; helm-flx

(use-package helm-flx
  :ensure t
  :pin    "melpa-stable"
  :config
  (progn
    (setq helm-flx-for-helm-find-files nil)

    (helm-flx-mode)))

;; helm-projectile

(when (demiazz/layer-used-p 'projectile)
  (use-package helm-projectile
    :ensure t
    :pin    "melpa-stable"
    :commands (helm-projectile-switch-to-buffer
               helm-projectile-find-dir
               helm-projectile-dired-find-dir
               helm-projectile-recentf
               helm-projectile-find-file
               helm-projectile-grep
               helm-projectile
               helm-projectile-switch-project)
    :init
    (setq projectile-switch-project-action 'helm-projectile)))

;;----- Keybindings

(bind-keys*
 ("C-x C-f" . helm-find-files)
 ("C-x C-b" . helm-mini))
