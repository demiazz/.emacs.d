;;----- Settings

(demiazz/default-settings edit
                          delete-selection t
                          transient-mark   t
                          
                          tab-width        2
                          tabs           nil)

;;----- Initialization

;; Delete selection when start typing

(delete-selection-mode l-edit//delete-selection)

;; Transient mark like a classic editors

(transient-mark-mode l-edit//transient-mark)

;; Clipboard

(setq-default x-select-enable-clipboard t)

;; Indentation

(setq-default tab-width        l-edit//tab-width
              indent-tabs-mode l-edit//tabs)
