(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package rustic
  :straight (
	     :config (
		      (setq rustic-format-on-save nil
			    rustic-lsp-format t
			    rustic-lsp-server 'rust-analyzer
;;			    lsp-rust-analyzer-server-command '("~/.cargo/bin/rust-analyzer")
			    lsp-rust-analyzer-server-display-inlay-hints t
			    lsp-rust-analyzer-display-chaining-hints t
			    lsp-rust-analyzer-display-parameter-hints t
			    rustic-format-display-method 'ignore
			    rustic-indent-method-chain t
			    rustic-format-on-save nil
			    rustic-lsp-format t
			    rustic-format-trigger 'on-save))))

(use-package lsp-mode
  :straight (
	     :hook (
		    (rustic-mode . lsp))
	     :config (
		      (setq lsp-prefer-flymake nil))
	     
	     :commands lsp))
(use-package lsp-ui :straight (
			       :config(
				       (setq lsp-ui-doc-enable t
					     lsp-ui-doc-use-webkit t
					     lsp-ui-doc-position 'top
					     lsp-ui-doc-include-signature t
					     lsp-ui-doc-header t
					     lsp-ui-sideline-enable nil
					     lsp-ui-flycheck-enable t
					     lsp-ui-flycheck-list-position 'right
					     lsp-ui-flycheck-live-reporting t
					     lsp-ui-peek-enable t
					     lsp-ui-peek-list-width 60
					     lsp-ui-peek-peek-height 25)
				       )
			       :hook (lsp-mode . lsp-ui)
			       :commands lsp-ui-mode))

(use-package company
  :straight (
	     :config (
		      (setq company-idle-delay 0.3)
		      (global-company-mode 1)
		      (global-set-key (kbd "C-<tab>") 'company-complete))))

(use-package company-lsp
  :straight (
	     :config (
		      (push 'company-lsp company-backends)
		      (setq company-transformers nil
			    company-lsp-async t
			    company-lsp-cache-candidates nil))))

(setq display-line-numbers-type t)
