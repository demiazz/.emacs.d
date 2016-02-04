;;----- Settings

(demiazz/default-settings backup
                          enabled   t
                          directory (expand-file-name "backups"
                                                      user-emacs-directory))

;;----- Initialization

(unless l-backup//enabled
  (setq-default make-backup-files nil))

(when l-backup//enabled
  (let ((directory l-backup//directory))
    (unless (file-exists-p directory)
      (make-directory directory))

    (setq-default backup-directory-alist `(("." . ,directory))))

  (setq-default make-backup-files         t
                backup-by-copying         t
                version-control           t
                delete-old-versions       t
                delete-by-moving-to-trash t
                keep-old-versions         3
                keep-new-versions         3
                auto-save-default         t
                auto-save-timeout         300
                auto-save-interval        300))
                
        
