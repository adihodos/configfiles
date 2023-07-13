(setq inhibit-startup-message t)

(require 'package)
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(setq package-user-dir (expand-file-name "elpa/" user-emacs-directory))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package no-littering :ensure t)

(recentf-mode 1)
(setq recentf-max-saved-items 100
      inhibit-startup-message t
      ring-bell-function 'ignore)

(tool-bar-mode -1)
(menu-bar-mode -1)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(global-hl-line-mode 1)
(global-display-line-numbers-mode 1)

(use-package gruber-darker-theme :ensure)
(load-theme 'gruber-darker t)

(use-package command-log-mode :ensure t)
(use-package all-the-icons :ensure t)

(use-package rainbow-delimiters :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :ensure t
  :init (which-key-mode)
  ;; :diminish which-key-mode
  :config
  (which-key-setup-side-window-right)
  (setq which-key-idle-delay 0.3))

(set-face-attribute 'default nil :font "Iosevka SS03" :weight 'normal :height 150)
(set-face-attribute 'fixed-pitch nil :font "Iosevka SS03" :weight 'light :height 150)
(set-face-attribute 'variable-pitch nil :font "Iosevka SS03" :weight 'light :height 150)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; (when (file-directory-p "~/Projects/Code")
  ;;   (setq projectile-project-search-path '("~/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :ensure t
 :after projectile
 :config
 (counsel-projectile-mode 1))

(use-package rustic
  :ensure t
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  ;; (setq rustic-format-on-save t)
  ;; (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook)
  )

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  ;; (add-hook 'before-save-hook 'lsp-format-buffer nil t)
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(company-tabnine cmake-mode ack rg multiple-cursors yasnippet winum use-package toml-mode selectrum rustic rainbow-delimiters no-littering lsp-ui gruber-darker-theme glsl-mode flycheck doom-themes counsel-projectile company command-log-mode all-the-icons-dired))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package lsp-mode
  :ensure t
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  ;; (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-rust-analyzer-server-display-inlay-hints nil)
  ;; (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints nil)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :hook (lsp-mode . (lambda ()
                      (let ((lsp-keymap-prefix "C-c l"))
                        (lsp-enable-which-key-integration))))
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
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
  )
  

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-use-webkit t)
  (lsp-ui-doc-position 'at-point)
  )

(use-package company
  :ensure
  :custom
  ;; Trigger completion immediately.
  (setq company-idle-delay 0)

  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (setq company-show-numbers t)
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  :bind
  (:map company-active-map
	      ("C-n". company-select-next)
	      ("C-p". company-select-previous)
	      ("M-<". company-select-first)
	      ("M->". company-select-last)))

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package flycheck :ensure)
(use-package toml-mode :ensure)


(use-package selectrum
  :ensure
  :init
  (selectrum-mode)
  :custom
  (completion-styles '(flex substring partial-completion)))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :config)

(use-package all-the-icons-dired :ensure
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;;(load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package glsl-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode)))

;; ;; Some common sense settings

;; start every frame maximized
(add-to-list 'default-frame-alist '(fullscreen . fullboth))

(use-package winum :ensure t)
(winum-mode)

(use-package ibuffer :ensure t
  :config (global-set-key (kbd "C-x C-b") 'ibuffer)
  )

(use-package multiple-cursors
  :ensure t
  :config
  ;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(setq backup-directory-alist            '((".*" . "~/.Trash")))

(use-package rg
  :ensure t
  :config (global-set-key (kbd "C-c s") 'rg-project)
  (setq rg-group-result t)
  (setq rg-align-position-numbers t)
  (setq rg-align-line-number-field-length 3)
  (setq rg-align-column-number-field-length 3)
  (setq rg-align-line-column-separator "#")
  (setq rg-align-position-content-separator "|")
  )
  
;; (use-package ack
;;   :ensure t
;;   :config
;;   (global-set-key (kbd "C-c s") 'ack)
;;   (setq ack-defaults-function 'ack-legacy-defaults)
;;   )

(use-package cmake-mode :ensure t)
(when (executable-find "clangd")
  (add-hook 'c++-mode-hook #'lsp))

;; (use-package company-tabnine :ensure t)
;; (add-to-list 'company-backends #'company-tabnine)
(with-eval-after-load 'c++-mode
  :bind (:map c++-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-clangd-status)))

