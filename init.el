;;----- Packages

(require 'package)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(add-to-list 'package-archives
             '("melpa"     . "https://melpa.org/packages/"))

(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

;;----- use-package

(defconst demiazz//use-package-path
  (expand-file-name "use-package" user-emacs-directory)
  "Path to `use-package`.")

;; Add use-package path to load-path list

(add-to-list 'load-path demiazz//use-package-path)

;; Try to load use-package, and clone them from Git, if it's not exists

(unless (require 'use-package nil 'noerror)
  (let* ((log (get-buffer-create "*use-package-installation*"))
         (git (or (executable-find "git")
                  (error "Unable to find `git` executable")))
         (url "https://github.com/jwiegley/use-package")
         
         (process-connection-type nil)
         
         (status
          (call-process
           git nil `(,log t) t "--no-pager" "clone" "-v" url demiazz//use-package-path)))
    
    (if (zerop status)
        (progn
          (message "Package `use-package` successfully installed")
          (require 'use-package))
      (error "Could't clone `use-package` from the Git repository `%s`" url))))

;;----- Requirements

;; Useful functions library

(use-package dash
  :ensure t
  :pin    "melpa-stable")

;; Loading exec path from shell

(use-package exec-path-from-shell
  :ensure t
  :pin    "melpa-stable")

;;----- Environment

;; If Mac OS X, load exec path from shell, because apps launched from launchpad
;; don't load shell environment.

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;----- Config

(defconst demiazz//config-path
  (expand-file-name "config.el" user-emacs-directory)
  "Path to config file.")

(defconst demiazz//config-template-path
  (expand-file-name "config.template.el" user-emacs-directory)
  "Path to config's template file.")

(defun demiazz//load-config ()
  (unless (file-exists-p demiazz//config-path)
    (copy-file demiazz//config-template-path demiazz//config-path))
  (load demiazz//config-path))

;;----- Layers

(defvar demiazz//layers nil)

(defconst demiazz//layers-path
  (expand-file-name "layers" user-emacs-directory)
  "Path to layers.")

(defun demiazz//require-layers ()
  (dolist (layer demiazz//layers)
    (load (expand-file-name (symbol-name layer) demiazz//layers-path))))

(defun demiazz//load-layers ()
  (demiazz//require-layers))

;; Public API

(defmacro demiazz/use-layers (&rest layers)
  `(dolist (layer ',layers)
     (add-to-list 'demiazz//layers layer)))

(defun demiazz/layer-used-p (layer)
  (-contains? demiazz//layers layer))

;;----- Settings

(defun demiazz//list-to-assoc (source)
  (if (oddp (length source))
      (error "List must have even count of elements for converting to alist."))
  
  (let ((result '())
        (pair   '()))
    (dolist (e source)
      (setq pair (cons e pair))
      (when (= (length pair) 2)
        (setq result (cons (reverse pair) result))
        (setq pair '())))
    (reverse result)))

(defmacro demiazz//setting-parser (layer)
  `(lambda (setting)
     (let* ((prefix (concat "l-" (symbol-name ,layer) "//"))
            (name (intern (concat prefix (symbol-name (car setting)))))
            (value (eval (cadr setting))))
       (list name value))))

(defmacro demiazz//setting-processor (as-default)
  `(lambda (setting)
     (let ((name  (car setting))
     (value (cadr setting)))
       (if ,as-default
           (unless (boundp name)
             (set-default name value))
         (set-default name value)))))

(defun demiazz//process-settings (layer settings as-default)
  (let ((parser    (demiazz//setting-parser layer))
        (processor (demiazz//setting-processor as-default)))
    (->> (demiazz//list-to-assoc settings)
         (-map parser)
         (-map processor))))

;; Public API

(defmacro demiazz/default-settings (layer &rest settings)
  `(demiazz//process-settings ',layer ',settings t))

(defmacro demiazz/settings (layer &rest settings)
  `(demiazz//process-settings ',layer ',settings nil))

;;----- Logger

(setq-default demiazz//log-buffer (get-buffer-create "*Log*"))

(setq-default demiazz//debug-log-level 0)
(setq-default demiazz//info-log-level 1)
(setq-default demiazz//warning-log-level 2)
(setq-default demiazz//error-log-level 3)
(setq-default demiazz//fatal-log-level 4)

(setq-default demiazz//show-log-on demiazz//warning-log-level)
(setq-default demiazz//max-log-level demiazz//debug-log-level)

(defun demiazz//log (level subsystem message)
  (let ((message-format "%s :: %s :: %s\n")
        (formatted-log-level (demiazz//format-log-level level))
        (formatted-subsystem (upcase subsystem)))
    (with-current-buffer demiazz//log-buffer
      (when (> level demiazz//max-log-level)
        (setq-default demiazz//max-log-level level))
      (insert
       (format message-format
               formatted-log-level
               formatted-subsystem
               message)))))

(defun demiazz//format-log-level (level)
  (cond ((= level demiazz//debug-log-level) "DEBUG")
        ((= level demiazz//info-log-level) "INFO")
        ((= level demiazz//warning-log-level) "WARNING")
        ((= level demiazz//error-log-level) "ERROR")
        ((= level demiazz//fatal-log-level) "FATAL")))

(defun demiazz//show-log-on-startup ()
  (when (>= demiazz//max-log-level demiazz//show-log-on)
    (switch-to-buffer demiazz//log-buffer)))

(defun demiazz/log/debug (subsystem message)
  (demiazz//log demiazz//debug-log-level subsystem message))

(defun demiazz/log/info (subsystem message)
  (demiazz//log demiazz//info-log-level subsystem message))

(defun demiazz/log/warning (subsystem message)
  (demiazz//log demiazz//warning-log-level subsystem message))

(defun demiazz/log/error (subsystem message)
  (demiazz//log demiazz//error-log-level subsystem message))

(defun demiazz/log/fatal (subsystem message)
  (demiazz//log demiazz//fatal-log-level subsystem message))

;;----- Bootstrap

(demiazz//load-config)
(demiazz//load-layers)
(demiazz//show-log-on-startup)

;;----- Server

(require 'server)

(unless (server-running-p)
  (server-start))
