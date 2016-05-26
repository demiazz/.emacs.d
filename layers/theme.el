;;----- Settings

(demiazz/default-settings theme
                          themes nil
                          light nil
                          dark nil
                          default nil)

;;----- Utilities

(setq-default demiazz//theme//current-theme nil)

(defun demiazz//theme//not-installed-themes ()
  (-filter (lambda (theme) (not (package-installed-p theme)))
	   l-theme//themes))

(defun demiazz//theme//install-themes ()
  (let ((themes (demiazz//theme//not-installed-themes)))
    (when themes
      (message "Not all themes are installed")

      (package-refresh-contents)

      (dolist (theme themes)
        (package-install theme)))))

(defun demiazz//theme//validate-light-theme ()
  (if l-theme//light t
    (progn
      (demiazz//log//fatal "layer/theme" "light theme isn't set.") nil)))

(defun demiazz//theme//validate-dark-theme ()
  (if l-theme//dark t
    (progn
      (demiazz//log//fatal "layer/theme" "dark theme isn't set.") nil)))

(defun demiazz//theme//validate-default-theme ()
  (if (or (= l-theme//default 'light)
          (= l-theme//default 'dark)) t
    (progn
      (deimazz//log//fatal "layer/theme"
                           "default theme must be `light` or `dark`"))))

(defun demiazz//theme//is-valid ()
  (or (demiazz//theme//validate-light-theme)
      (demiazz//theme//validate-dark-theme)
      (demiazz//theme//validate-default-theme)))

(defun demiazz//theme//use-light-theme ()
  (setq-default demiazz//theme//current-theme 'light)
  (load-theme l-theme//light))

(defun demiazz//theme//use-dark-theme ()
  (setq-default demiazz//theme//current-theme 'dark)
  (load-theme l-theme//dark))

(defun demiazz//theme//use-default-theme ()
  (let ((default-theme l-theme//default))
    (cond
     ((eq default-theme 'light) (demiazz//theme//use-light-theme))
     ((eq default-theme 'dark) (demiazz//theme//use-dark-theme)))))

;;----- Public API

(defun demiazz/theme/toggle-theme ()
  (interactive)
  (if (eq demiazz//theme//current-theme 'light)
      (demiazz//theme//use-dark-theme)
    (demiazz//theme//use-light-theme)))

(defun demiazz/theme/light-theme ()
  (interactive)
  (demiazz//theme//use-light-theme))

(defun demiazz/theme/dark-theme ()
  (interactive)
  (demiazz//theme//use-dark-theme))

;;----- Bootstrap

(defun demiazz//theme//bootstrap ()
  (setq-default custom-safe-themes t)
  (demiazz//theme//install-themes)
  (demiazz//theme//use-default-theme))

;;----- Keybindings

(defun demiazz//theme//keybindings ()
  (bind-keys*
 ("C-c t t" . demiazz/theme/toggle-theme)
 ("C-c t l" . demiazz/theme/light-theme)
 ("C-c t d" . demiazz/theme/dark-theme)))

;;----- Initialization

(when (demiazz//theme//is-valid)
  (demiazz//theme//bootstrap)
  (demiazz//theme//keybindings))
