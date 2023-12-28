;;; tree-sitter
(use-package tree-sitter)
(use-package tree-sitter-langs)
;; (global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

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

(use-package lsp-treemacs
  :after lsp)

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
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-icons-alist 'company-box-icons-all-the-icons
	company-box-backends-colors nil
	company-box-backends-colors nil

        ;; These are the Doom Emacs defaults
        company-box-icons-all-the-icons
        `((Unknown       . ,(all-the-icons-material "find_in_page"             :face 'all-the-icons-purple))
          (Text          . ,(all-the-icons-material "text_fields"              :face 'all-the-icons-green))
          (Method        . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
          (Function      . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
          (Constructor   . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
          (Field         . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
          (Variable      . ,(all-the-icons-material "adjust"                   :face 'all-the-icons-blue))
          (Class         . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
          (Interface     . ,(all-the-icons-material "settings_input_component" :face 'all-the-icons-red))
          (Module        . ,(all-the-icons-material "view_module"              :face 'all-the-icons-red))
          (Property      . ,(all-the-icons-material "settings"                 :face 'all-the-icons-red))
          (Unit          . ,(all-the-icons-material "straighten"               :face 'all-the-icons-red))
          (Value         . ,(all-the-icons-material "filter_1"                 :face 'all-the-icons-red))
          (Enum          . ,(all-the-icons-material "plus_one"                 :face 'all-the-icons-red))
          (Keyword       . ,(all-the-icons-material "filter_center_focus"      :face 'all-the-icons-red))
          (Snippet       . ,(all-the-icons-material "short_text"               :face 'all-the-icons-red))
          (Color         . ,(all-the-icons-material "color_lens"               :face 'all-the-icons-red))
          (File          . ,(all-the-icons-material "insert_drive_file"        :face 'all-the-icons-red))
          (Reference     . ,(all-the-icons-material "collections_bookmark"     :face 'all-the-icons-red))
          (Folder        . ,(all-the-icons-material "folder"                   :face 'all-the-icons-red))
          (EnumMember    . ,(all-the-icons-material "people"                   :face 'all-the-icons-red))
          (Constant      . ,(all-the-icons-material "pause_circle_filled"      :face 'all-the-icons-red))
          (Struct        . ,(all-the-icons-material "streetview"               :face 'all-the-icons-red))
          (Event         . ,(all-the-icons-material "event"                    :face 'all-the-icons-red))
          (Operator      . ,(all-the-icons-material "control_point"            :face 'all-the-icons-red))
          (TypeParameter . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
          (Template      . ,(all-the-icons-material "short_text"               :face 'all-the-icons-green))))
  ;; Add a space after the icon
  (dolist (elt company-box-icons-all-the-icons)
    (setcdr elt (concat (cdr elt) " "))))

(use-package flycheck
  :defer t
  :init (global-flycheck-mode))

(use-package company-glsl
  :config
  (when (executable-find "glslangValidator")
    (add-to-list 'company-backends 'company-glsl)))

(use-package rustic
  :hook ((rustic-mode . flycheck-mode))
  :config
  ;; comment to disable rustfmt on save
  ;; format on save makes Emacs freeze on Windows
  (when (not (eq system-type 'windows-nt))
    (setq rustic-format-on-save t)))

(provide 'lsp)
