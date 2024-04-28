;; emacs-which-key.el --- which-key package config file -*- lexical-binding: t -*-
;;; Code:

(use-package which-key
  :ensure t
  :config
  (setq which-key-separator "  ")
  (setq which-key-prefix-prefix "-> ")
  (setq which-key-max-display-columns 3)
  
  (setq which-key-idle-secondary-delay 0.25)
  (setq which-key-add-column-padding 1)
  (setq which-key-max-description-length 40)
  
  (setq which-key-idle-delay 0.3)
  (setq which-key-popup-type 'side-window)

  ;; location of which-key window. valid values: top, bottom, left, right,
  ;; or a list of any of the two. If it's a list, which-key will always try
  ;; the first location first. It will go to the second location if there is
  ;; not enough room to display any keys in the first location
  (setq which-key-side-window-location 'bottom)

  ;; max width of which-key window, when displayed at left or right.
  ;; valid values: number of columns (integer), or percentage out of current
  ;; frame's width (float larger than 0 and smaller than 1)
  (setq which-key-side-window-max-width 0.33)

  ;; max height of which-key window, when displayed at top or bottom.
  ;; valid values: number of lines (integer), or percentage out of current
  ;; frame's height (float larger than 0 and smaller than 1)
  (setq which-key-side-window-max-height 0.25)
  (which-key-mode 1))

;;
;; better help
(use-package helpful
  :ensure t
  :bind (:map global-map
  ;; Note that the built-in `describe-function' includes both functions
  ;; and macros. `helpful-function' is functions only, so we provide
  ;; `helpful-callable' as a drop-in replacement.
        ([remap describe-function]  . helpful-callable)
        ([remap describe-command]  . helpful-command)
        ([remap describe-variable]  . helpful-variable)
        ([remap describe-key]  . helpful-key)))

(provide 'emacs-which-key)

;; emacs-which-key.el ends here
