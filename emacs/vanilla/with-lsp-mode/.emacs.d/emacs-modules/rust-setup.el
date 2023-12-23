(use-package rustic
  :hook ((rustic-mode . flycheck-mode))
  :config
  ;; comment to disable rustfmt on save
  ;; format on save makes Emacs freeze on Windows
  (when (not (eq system-type 'windows-nt))
    (setq rustic-format-on-save t)))

(use-package rust-playground)
;;; TOML
(use-package toml-mode)

(provide 'rust-setup)
