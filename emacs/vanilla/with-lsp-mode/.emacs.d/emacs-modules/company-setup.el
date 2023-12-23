
(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :custom
  ;; Trigger completion immediately.
  (setq company-idle-delay 0)

  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (setq company-show-numbers t)
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  (company-tooltip-align-annotations 't)
  (global-company-mode t)
  :bind
  (:map company-active-map
	("C-n". company-select-next)
	("C-p". company-select-previous)
	("M-<". company-select-first)
	("M->". company-select-last))
  :config
  (global-set-key (kbd "<C-tab>") 'company-complete))

(use-package company-prescient
  :config (company-prescient-mode))

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode))

(use-package company-glsl
  :config
  (when (executable-find "glslangValidator")
    (add-to-list 'company-backends 'company-glsl)))

(provide 'company-setup)
