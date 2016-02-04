;;----- Settings

(demiazz/default-settings theme
                          themes  nil
                          default nil)

;;----- Initialization

(defun demiazz//theme//installed-p ()
  "Check if all themes from settings is installed."
  (loop for theme in l-theme//themes
        when (not (package-installed-p theme)) do (return nil)
        finally (return t)))

(setq-default custom-safe-themes t)

(unless (demiazz//theme//installed-p)
  (message "Not all themes are installed")
  
  (package-refresh-contents)
  
  (dolist (theme l-theme//themes)
    (when (not (package-installed-p theme))
      (package-install theme))))

(let ((theme l-theme//default))
  (if theme (load-theme theme)))
