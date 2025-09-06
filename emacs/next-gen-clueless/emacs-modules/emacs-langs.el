;; emacs-langs.el --- langs config file -*- lexical-binding: t -*-
;;; Code:

(use-package emacs
;;;; Indentation
  :ensure nil
  :hook ((prog-mode . electric-indent-local-mode)
        (after-init . show-paren-mode)
        (prog-mode . eldoc-mode))

  :config
  (setq-default indent-tabs-mode t)
  (setq-default tab-width 4)
  (setq c-basic-offset 4)
  (defvaralias 'c-basic-offset 'tab-width)
;;;; Disable "electric" behaviour
  (electric-pair-mode 1)
  (electric-quote-mode 1)
  ;; I don't like auto indents in Org and related.  They are okay for
  ;; programming.
  (electric-indent-mode -1)
  ;;;; Parentheses (show-paren-mode)
  (setq show-paren-style 'parenthesis)
  (setq show-paren-when-point-in-periphery nil)
  (setq show-paren-when-point-inside-paren nil)
  (setq show-paren-context-when-offscreen 'overlay) ; Emacs 29
  ;;;; Eldoc (Emacs live documentation feedback)
  (setq eldoc-message-function #'message) ; don't use mode line for M-x eval-expression, etc.
  ;;;; Handle performance for very long lines (so-long.el)
  (global-so-long-mode 1))

(use-package caser
  :ensure t
  :bind (:map global-map
    ("C-c s" . caser-snakecase-dwim)
    ("C-c c" . caser-camelcase-dwim)
    ("C-c d" . caser-dashcase-dwim)))

;;; markdown mode
;;; Markdown (markdown-mode)
(use-package markdown-mode
  :ensure t
  :config
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-command "pandoc"))

;;; Flymake
(use-package flymake
  :ensure t
  :hook (emacs-lisp-mode . prot/flymake-mode-in-my-projects)
  :config
  (setq flymake-fringe-indicator-position 'left-fringe)
  (setq flymake-suppress-zero-counters t)
  (setq flymake-no-changes-timeout nil)
  (setq flymake-start-on-flymake-mode t)
  (setq flymake-start-on-save-buffer t)
  (setq flymake-proc-compilation-prevents-syntax-check t)
  (setq flymake-wrap-around nil)
  (setq flymake-mode-line-format
        '("" flymake-mode-line-exception flymake-mode-line-counters))
  ;; NOTE 2023-07-03: `prot-modeline.el' actually defines the counters
  ;; itself and ignores this.
  (setq flymake-mode-line-counter-format
        '("" flymake-mode-line-error-counter
          flymake-mode-line-warning-counter
          flymake-mode-line-note-counter ""))
  (setq flymake-show-diagnostics-at-end-of-line nil) ; Emacs 30
  (define-key ctl-x-x-map "m" #'flymake-mode) ; C-x x m

  (defvar prot/flymake-mode-projects-path
    (file-name-as-directory (expand-file-name "Projects" "~/Git/"))
    "Path to my Git projects.")

  (defun prot/flymake-mode-lexical-binding ()
    (when lexical-binding
      (flymake-mode 1)))

  (defun prot/flymake-mode-in-my-projects ()
    (when-let ((file (buffer-file-name))
               ((string-prefix-p prot/flymake-mode-projects-path
                                 (expand-file-name file)))
               ((not (file-directory-p file)))
               ((file-regular-p file)))
      (add-hook 'find-file-hook #'prot/flymake-mode-lexical-binding nil t)))

      :bind (:map flymake-mode-map
    ("C-c ! s" . flymake-start)
    ("C-c ! d" . flymake-show-buffer-diagnostics) ; Emacs28
    ("C-c ! D" . flymake-show-project-diagnostics) ; Emacs28
    ("C-c ! n" . flymake-goto-next-error)
    ("C-c ! p" . flymake-goto-prev-error)))

;;; Elisp packaging requirements
(use-package package-lint-flymake
  :ensure t
  :hook (flymake-diagnostic-functions . package-lint-flymake))

;;; Custom extensions for "focus mode" (logos.el)
;; Read the manual: <https://protesilaos.com/emacs/logos>.
(use-package olivetti
  :ensure t
  :config
  (setq olivetti-body-width 0.7)
  (setq olivetti-minimum-body-width 80)
  (setq olivetti-recall-visual-line-mode-entry-state t)
  (setq olivetti-style 'fancy))

(use-package nix-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode)))

(use-package glsl-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.comp\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode)))

(use-package toml-mode :ensure t)
;; RON - Rusty Object Notation
(use-package ron-mode :ensure t)
(use-package cmake-mode :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :hook (
  ('emacs-lisp-mode . rainbow-delimiters-mode)
  ('clojure-mode . rainbow-delimiters-mode)
  ('rust-mode . rainbow-delimiters-mode)
  ('rustic-mode . rainbow-delimiters-mode)
  ('c++-mode . rainbow-delimiters-mode)))

(use-package rust-mode
  :ensure t
  :hook ('rust-mode-hook .
						 (lambda () (setq indent-tabs-mode nil)))
  :config
  (setq rust-format-on-save t)
  )
;; (use-package rustic
;;   (:install t)
;;   (:delay 5)
;;   ;; (add-hook 'rustic-mode-hook #'flycheck-mode)
;;   ;; comment to disable rustfmt on save
;;   ;; format on save makes Emacs freeze on Windows
;;   (when (not (eq system-type 'windows-nt))
;;     (setq rustic-format-on-save t)))

(provide 'emacs-langs)
;; emacs-langs.el ends here
