;; emacs-icons.el --- icons config file -*- lexical-binding: t -*-
;;; Code:

(prot-emacs-package nerd-icons
  (:install t)
  (:delay 5)
  (setq nerd-icons-font-family "Symbols Nerd Font Mono"))

(prot-emacs-package nerd-icons-dired
  (:install t)
  (:delay 5)
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode))

(prot-emacs-package treemacs-nerd-icons
  (:install t)
  (:delay 5)
  (treemacs-load-theme "nerd-icons"))

(prot-emacs-package nerd-icons-ibuffer
  (:install t)
  (:delay 5)
  (add-hook 'ibuffer-mode-hook #'nerd-icons-ibuffer-mode)
  ;; Use human readable file size in ibuffer.
  (setq  nerd-icons-ibuffer-human-readable-size t))

(with-eval-after-load 'marginalia
  (prot-emacs-package nerd-icons-completion
    (:install t)
	(:delay 5)
        (nerd-icons-completion-mode)
        (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)))

(prot-emacs-package all-the-icons (:install t) (:delay 5))

(provide 'emacs-icons)
;; emacs-icons.el ends here
