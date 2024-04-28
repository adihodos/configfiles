;; emacs-icons.el --- icons config file -*- lexical-binding: t -*-
;;; Code:

(use-package nerd-icons
  :ensure t
  :config
  (setq nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package treemacs-nerd-icons
  :ensure t
  :config
  (treemacs-load-theme "nerd-icons"))

(use-package nerd-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :config
  ;; Use human readable file size in ibuffer.
  (setq  nerd-icons-ibuffer-human-readable-size t))

(use-package nerd-icons-completion
  :after marginalia
  :ensure t
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode))

(use-package all-the-icons :ensure t)

(provide 'emacs-icons)
;; emacs-icons.el ends here
