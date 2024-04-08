;; emacs-lsp.el --- LSP config file -*- lexical-binding: t -*-
;;; Code:

;;; tree-sitter
(prot-emacs-package tree-sitter
  (:install t)
  (:delay 2)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(prot-emacs-package tree-sitter-langs
  (:install t)
  (:delay 2))

(setq lsp-keymap-prefix "C-c l")

(prot-emacs-package lsp-mode
  (:install t)
  (:delay 2)
  (lsp)
  (lsp-deferred)
  ;; :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  ;; (lsp-rust-analyzer-cargo-watch-command "clippy")

  (add-hook 'lsp-mode-hook #'lsp-ui-mode)
  (add-hook 'lsp-mode-hook #'company-mode)
  (add-hook 'c-mode-hook 'lsp)
  (add-hook 'c++-mode-hook 'lsp)

  (setq lsp-enable-which-key-integration t)
  (setq lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (setq lsp-rust-analyzer-server-display-inlay-hints nil)
  ;; (setq lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (setq lsp-rust-analyzer-display-chaining-hints nil)
  (setq lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (setq lsp-rust-analyzer-display-closure-return-type-hints t)
  (setq lsp-rust-analyzer-display-parameter-hints nil)
  (setq lsp-rust-analyzer-display-reborrow-hints nil)
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
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-eldoc-render-all t)
  (setq lsp-eldoc-enable-hover nil)
  (setq lsp-lens-enable nil)
  (setq lsp-signature-auto-activate t) ;; you could manually request them via `lsp-signature-activate`
  (setq lsp-signature-render-documentation nil)

  (prot-emacs-keybind global-map 
    "<backtab>" #'lsp-signature-activate
    "C-<up>" #'lsp-signature-previous
    "C-<down>" #'lsp-signature-next
    "M-?" #'lsp-ui-peek-find-references))

(prot-emacs-package lsp-ui
  (:install t)
  (:delay 2)
  (lsp-ui-mode 1)
  (add-hook 'lsp-mode #'lsp-ui-mode)
  (setq lsp-ui-peek-always-show nil)
  (setq lsp-ui-sideline-show-diagnostics nil)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-show-code-actions nil)

  (setq lsp-ui-peek-enable t)
  (setq lsp-ui-peek-show-directory t)

  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-use-webkit t)
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-position 'at-point)

  (prot-emacs-keybind lsp-mode-map
    [remap xref-find-definitions] #'lsp-find-definition
    [remap xref-find-references] #'lsp-ui-peek-find-references)
  ;; (global-set-key (kbd "C-c C-h") 'lsp-describe-thing-at-point))
  (prot-emacs-keybind global-map 
    "C-c C-h" #'lsp-ui-doc-glance))

(with-eval-after-load 'lsp-mode
  (prot-emacs-package lsp-treemacs
    (:install t)
	(:delay 2)))

;; (use-package flycheck
;;   :defer t
;;   :init (global-flycheck-mode))

;; (use-package company-glsl
;;   :config
;;   (when (executable-find "glslangValidator")
;;     (add-to-list 'company-backends 'company-glsl)))



(provide 'emacs-lsp)
