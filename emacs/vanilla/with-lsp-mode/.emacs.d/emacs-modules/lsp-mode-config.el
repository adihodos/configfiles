
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  ;; (lsp-rust-analyzer-cargo-watch-command "clippy")
  
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-rust-analyzer-server-display-inlay-hints nil)
  ;; (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints nil)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  (setq lsp-clients-clangd-args '(
				  "--background-index"
				  "--clang-tidy"
				  "--enable-config"
				  "--pch-storage=memory"
				  "--header-insertion=never"
				  "--header-insertion-decorators"
				  "--all-scopes-completion"
				  "--completion-style=detailed"
				  "-j=4"
				  "--log=verbose"))
  :hook (
	 (lsp-mode-hook . lsp-ui-mode)
	 (lsp-mode . (lambda ()
                       (let ((lsp-keymap-prefix "C-c l"))
                         (lsp-enable-which-key-integration)))))
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-eldoc-render-all t)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-lens-enable nil)
  (setq lsp-signature-auto-activate t) ;; you could manually request them via `lsp-signature-activate`
  (setq lsp-signature-render-documentation nil)

  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (global-set-key (kbd "<backtab>") 'lsp-signature-activate)
  (global-set-key [C-up] 'lsp-signature-previous)
  (global-set-key [C-down] 'lsp-signature-next)
  (global-set-key (kbd "M-j") 'lsp-ui-imenu))

(use-package lsp-ui
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-peek-always-show nil
		lsp-ui-sideline-show-diagnostics nil
		lsp-ui-sideline-show-hover nil
		lsp-ui-sideline-show-code-actions nil
		
		lsp-ui-peek-enable t
		lsp-ui-peek-show-directory t
		
		lsp-ui-doc-enable nil
		lsp-ui-doc-use-webkit t
		lsp-ui-doc-show-with-mouse nil
		lsp-ui-doc-show-with-cursor nil
		lsp-ui-doc-position 'at-point)

  (define-key lsp-mode-map [remap xref-find-definitions] #'lsp-find-definition)
  (define-key lsp-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  ;; (global-set-key (kbd "C-c C-h") 'lsp-describe-thing-at-point))
    (global-set-key (kbd "C-c C-h") 'lsp-ui-doc-glance))

(provide 'lsp-mode-config)
