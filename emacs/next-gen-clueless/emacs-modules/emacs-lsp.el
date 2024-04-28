;; emacs-lsp.el --- LSP config file -*- lexical-binding: t -*-
;;; Code:

;;; tree-sitter
(use-package tree-sitter
  :ensure t
  :hook (tree-sitter-after-on . tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t)

(use-package eglot
  :ensure t
  :hook (((c++-mode c-mode) . eglot-ensure)
         (eglot-managed-mode-hook . company-mode))
  :bind (:map eglot-mode-map
	("C-c l =" . eglot-format-buffer)
	("C-c l a" . eglot-code-actions)
	("C-c l h" . eldoc)
	("C-c l i" . eglot-code-action-organize-imports)
	("C-c l q" . eglot-code-action-quickfix)
	("C-c l e" . eglot-code-action-extract)
	("C-c l I" . eglot-code-action-inline)
	("C-c l R" . eglot-code-action-rewrite)
	("C-c l r" . eglot-rename))
	:config
    (setq eglot-autoshutdown t)
    (with-eval-after-load 'eglot
;;       (add-to-list 'eglot-ignored-server-capabilites :hoverProvider)
      (add-to-list 'eglot-server-programs
  '(c++-mode . ("clangd"
                "--background-index"
                "--clang-tidy"
                "--pch-storage=memory"
                "--header-insertion=never"
                "--header-insertion-decorators"
                "--all-scopes-completion"
                "--completion-style=detailed"
                "-j=4"
                "--log=verbose")))))

(use-package eldoc
  :preface
  (defun mp-eglot-eldoc ()
    (setq eldoc-documentation-strategy
            'eldoc-documentation-compose-eagerly))
  :ensure t
  :hook ((eglot-managed-mode emacs-lisp-mode lisp-interaction-mode) . mp-eglot-eldoc)
  :config
  (setq eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit))

;; (use-package eldoc-box
;;   :after eldoc
;;   :ensure t
;;   :hook (eglot-managed-mode . eldoc-box-hover-at-point-mode))

;; (use-package rust-mode
;;   :ensure t)

;; (prot-emacs-package rust-ts-mode
;;   (:install t)
;;   (:delay 1)
;;   (:after eglot))

  ;; :hook ((rust-ts-mode . eglot-ensure)
  ;; 	 (rust-ts-mode . company-tng-mode)
  ;; 	 (rust-ts-mode . (lambda ()
  ;; 			   (eglot-inlay-hints-mode -1))))
  ;; :config
  ;; (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))
  ;; (add-to-list 'eglot-server-programs '(rust-ts-mode . ("rust-analyzer"))))  

;; (setq lsp-keymap-prefix "C-c l")
;; 
;; (prot-emacs-package lsp-mode
;;   (:install t)
;;   (:delay 1)
;;   (lsp)
;;   (lsp-deferred)
;;   ;; :custom
;;   ;; what to use when checking on-save. "check" is default, I prefer clippy
;;   ;; (lsp-rust-analyzer-cargo-watch-command "clippy")
;; 
;;   (add-hook 'lsp-mode-hook #'lsp-ui-mode)
;;   (add-hook 'lsp-mode-hook #'company-mode)
;;   (add-hook 'c-mode-hook 'lsp)
;;   (add-hook 'c++-mode-hook 'lsp)
;; 
;;   (setq lsp-enable-which-key-integration t)
;;   (setq lsp-idle-delay 0.6)
;;   ;; enable / disable the hints as you prefer:
;;   (setq lsp-rust-analyzer-server-display-inlay-hints nil)
;;   ;; (setq lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
;;   (setq lsp-rust-analyzer-display-chaining-hints nil)
;;   (setq lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
;;   (setq lsp-rust-analyzer-display-closure-return-type-hints t)
;;   (setq lsp-rust-analyzer-display-parameter-hints nil)
;;   (setq lsp-rust-analyzer-display-reborrow-hints nil)
;;   (setq lsp-clients-clangd-args '(
;; 								  "--background-index"
;; 								  "--clang-tidy"
;; 								  "--enable-config"
;; 								  "--pch-storage=memory"
;; 								  "--header-insertion=never"
;; 								  "--header-insertion-decorators"
;; 								  "--all-scopes-completion"
;; 								  "--completion-style=detailed"
;; 								  "-j=4"
;; 								  "--log=verbose"))
;;   (setq lsp-headerline-breadcrumb-enable nil)
;;   (setq lsp-eldoc-render-all t)
;;   (setq lsp-eldoc-enable-hover nil)
;;   (setq lsp-lens-enable nil)
;;   (setq lsp-signature-auto-activate t) ;; you could manually request them via `lsp-signature-activate`
;;   (setq lsp-signature-render-documentation nil)
;; 
;;   (prot-emacs-keybind global-map 
;;     "<backtab>" #'lsp-signature-activate
;;     "C-<up>" #'lsp-signature-previous
;;     "C-<down>" #'lsp-signature-next
;;     "M-?" #'lsp-ui-peek-find-references))
;; 
;; (prot-emacs-package lsp-ui
;;   (:install t)
;;   (:delay 1)
;;   (lsp-ui-mode 1)
;;   (add-hook 'lsp-mode #'lsp-ui-mode)
;;   (setq lsp-ui-peek-always-show nil)
;;   (setq lsp-ui-sideline-show-diagnostics nil)
;;   (setq lsp-ui-sideline-show-hover nil)
;;   (setq lsp-ui-sideline-show-code-actions nil)
;; 
;;   (setq lsp-ui-peek-enable t)
;;   (setq lsp-ui-peek-show-directory t)
;; 
;;   (setq lsp-ui-doc-enable nil)
;;   (setq lsp-ui-doc-use-webkit t)
;;   (setq lsp-ui-doc-show-with-mouse nil)
;;   (setq lsp-ui-doc-show-with-cursor nil)
;;   (setq lsp-ui-doc-position 'at-point)
;; 
;;   (prot-emacs-keybind lsp-mode-map
;;     [remap xref-find-definitions] #'lsp-find-definition
;;     [remap xref-find-references] #'lsp-ui-peek-find-references)
;;   ;; (global-set-key (kbd "C-c C-h") 'lsp-describe-thing-at-point))
;;   (prot-emacs-keybind global-map 
;;     "C-c C-h" #'lsp-ui-doc-glance))
;; 
;; (with-eval-after-load 'lsp-mode
;;   (prot-emacs-package lsp-treemacs
;;     (:install t)
;; 	(:delay 1)))
;; 
;; (prot-emacs-package meson-mode
;;   (:install t)
;;   (:delay 1)
;;   (add-hook 'meson-mode-hook 'company-mode))
;; (use-package flycheck
;;   :defer t
;;   :init (global-flycheck-mode))

(use-package company-glsl
  :ensure t
  :config
  (when (executable-find "glslangValidator")
    (add-to-list 'company-backends 'company-glsl)))

(provide 'emacs-lsp)
