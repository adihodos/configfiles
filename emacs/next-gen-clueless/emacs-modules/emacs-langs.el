;; emacs-langs.el --- langs config file -*- lexical-binding: t -*-
;;; Code:

(prot-emacs-configure
;;;; Indentation
  (:delay 1)
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
  (add-hook 'prog-mode-hook #'electric-indent-local-mode)

  ;;;; Parentheses (show-paren-mode)
  (setq show-paren-style 'parenthesis)
  (setq show-paren-when-point-in-periphery nil)
  (setq show-paren-when-point-inside-paren nil)
  (setq show-paren-context-when-offscreen 'overlay) ; Emacs 29
  (add-hook 'after-init-hook #'show-paren-mode)

  ;;;; Eldoc (Emacs live documentation feedback)
  (setq eldoc-message-function #'message) ; don't use mode line for M-x eval-expression, etc.
  (add-hook 'prog-mode-hook #'eldoc-mode)

  ;;;; Handle performance for very long lines (so-long.el)
  (global-so-long-mode 1))

(prot-emacs-package caser
  (:install t)
  (:delay 8)
  (prot-emacs-keybind global-map
    "C-c s" #'caser-snakecase-dwim
    "C-c c" #'caser-camelcase-dwim
    "C-c d" #'caser-dashcase-dwim))

;;; markdown mode
;;; Markdown (markdown-mode)
(prot-emacs-package markdown-mode
  (:install t)
  (:delay 8)
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-command "pandoc"))

;;; Flymake
(prot-emacs-package flymake
  (:delay 30)
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

  (add-hook 'emacs-lisp-mode-hook #'prot/flymake-mode-in-my-projects)

  (define-key ctl-x-x-map "m" #'flymake-mode) ; C-x x m
  (prot-emacs-keybind flymake-mode-map
    "C-c ! s" #'flymake-start
    "C-c ! d" #'flymake-show-buffer-diagnostics ; Emacs28
    "C-c ! D" #'flymake-show-project-diagnostics ; Emacs28
    "C-c ! n" #'flymake-goto-next-error
    "C-c ! p" #'flymake-goto-prev-error))

;;; Elisp packaging requirements
(prot-emacs-package package-lint-flymake
  (:install t)
  (:delay 30)
  (add-hook 'flymake-diagnostic-functions #'package-lint-flymake))

;;; Custom extensions for "focus mode" (logos.el)
;; Read the manual: <https://protesilaos.com/emacs/logos>.
(prot-emacs-package olivetti
  (:install t)
  (:delay 10)
  (setq olivetti-body-width 0.7)
  (setq olivetti-minimum-body-width 80)
  (setq olivetti-recall-visual-line-mode-entry-state t)
  (setq olivetti-style 'fancy))

(prot-emacs-package nix-mode
  (:install t)
  (:delay 10)
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode)))

(prot-emacs-package glsl-mode
  (:install t)
  (:delay 10)
  (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.comp\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode)))

(prot-emacs-package toml-mode (:install t) (:delay 10))
;; RON - Rusty Object Notation
(prot-emacs-package ron-mode (:install t) (:delay 10))
(prot-emacs-package cmake-mode
  (:install t)
  (:delay 10))
;; (cmake-ide-setup))

(prot-emacs-package rainbow-delimiters
  (:install t)
  (:delay 10)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'rust-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'rustic-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'c++-mode-hook #'rainbow-delimiters-mode))

(prot-emacs-package rustic
  (:install t)
  (:delay 5)
  ;; (add-hook 'rustic-mode-hook #'flycheck-mode)
  ;; comment to disable rustfmt on save
  ;; format on save makes Emacs freeze on Windows
  (when (not (eq system-type 'windows-nt))
    (setq rustic-format-on-save t)))

(provide 'emacs-langs)
;; emacs-langs.el ends here
