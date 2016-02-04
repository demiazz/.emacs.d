;;----- Settings

(demiazz/default-settings ui
                          scroll-bar      nil
                          tool-bar        nil
                          menu-bar        nil
                          
                          frame-mode      'maximized
                          
                          font-family     "Monaco"
                          font-size       14
                          
                          splash-screen   nil
                          
                          scratch-message nil
                          major-mode      'lisp-interaction-mode
                          
                          column-rules    '(80 100 120)
                          
                          line-number     t
                          column-number   t

                          linum-format    " %d ")

;;----- Initialization

;; Frame

(when window-system
  (let ((mode l-ui//frame-mode))
    (cond ((equal mode 'maximized)
           (add-hook 'window-setup-hook 'toggle-frame-maximized t))
          ((equal mode 'fullscreen)
           (add-hook 'window-setup-hook 'toggle-frame-fullscreen t)))))

;; Bars

(unless l-ui//scroll-bar (scroll-bar-mode -1))
(unless l-ui//tool-bar   (tool-bar-mode   -1))
(unless l-ui//menu-bar   (menu-bar-mode   -1))

;; Font

(when window-system
  (let ((family l-ui//font-family)
        (size   l-ui//font-size))
    (if (member family (font-family-list))
        (set-face-attribute 'default nil :font family))
    (set-face-attribute 'default nil :height (* size 10))))

;; Splash screen

(setq-default inhibit-splash-screen (not l-ui//splash-screen))

;; Scratch

(setq-default initial-scratch-messsage l-ui//scratch-message
              initial-major-mode       l-ui//major-mode)

;; Column markers

(use-package column-marker
  :ensure t
  :pin    "melpa-stable"
  :init
  (add-hook 'prog-mode-hook
            (lambda ()
              (set-face-attribute 'column-marker-1 nil :background "goldenrod1")
              (set-face-attribute 'column-marker-2 nil :background "orange")
              (set-face-attribute 'column-marker-3 nil :background "red")
              
              (let ((rule-1 (nth 0 l-ui//column-rules))
                    (rule-2 (nth 1 l-ui//column-rules))
                    (rule-3 (nth 2 l-ui//column-rules)))
                (if rule-1 (column-marker-1 rule-1))
                (if rule-2 (column-marker-2 rule-2))
                (if rule-3 (column-marker-3 rule-3))))))

;; Line and column numbers in status line

(line-number-mode   l-ui//line-number)
(column-number-mode l-ui//column-number)

;; Line numbers and empty lines

(toggle-indicate-empty-lines)

(use-package nlinum
  :ensure t
  :init
  (setq-default nlinum-format l-ui//linum-format)
  (global-linum-mode))

;; Paren mode

(show-paren-mode)

;; Miscellneous

(setq-default echo-keystrokes 0.1
              use-dialog-box  nil
              visible-bell    nil)
