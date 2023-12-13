;;
;; My epic EMACS init file :)
;;

;;; Code:

(setq gc-cons-threshold (* 128 1024 1024)
      read-process-output-max (* 1024 1024)
      make-backup-files nil
      create-lockfiles nil)

(add-hook 'emacs-startup-hook
	  #'(lambda ()
	      (message "Startup in %s seconds with %d garbage collection"
		       (emacs-init-time "%.2f")
		       gcs-done)))

(require 'server)
(unless (server-running-p)
  (server-start))

(setq-default shell-file-name "/bin/bash")
(setq backup-directory-alist            '((".*" . "~/.Trash")))
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file t)

(defmacro prot-emacs-keybind (keymap &rest definitions)
  "Expand key binding DEFINITIONS for the given KEYMAP.
DEFINITIONS is a sequence of string and command pairs."
  (declare (indent 1))
  (unless (zerop (% (length definitions) 2))
    (error "Uneven number of key+command pairs"))
  (let ((keys (seq-filter #'stringp definitions))
        ;; We do accept nil as a definition: it unsets the given key.
        (commands (seq-remove #'stringp definitions)))
    `(when-let (((keymapp ,keymap))
                (map ,keymap))
       ,@(mapcar
          (lambda (pair)
            (let* ((key (car pair))
                   (command (cdr pair)))
              (unless (and (null key) (null command))
                `(define-key map (kbd ,key) ,command))))
          (cl-mapcar #'cons keys commands)))))

;; Sample of `prot-emacs-keybind'
;; (prot-emacs-keybind global-map
;;   "C-z" nil
;;   "C-x b" #'switch-to-buffer
;;   "C-x C-c" nil
;;   "C-x k" #'kill-buffer)

;;
;; Package management
(require 'package)
(setq package-archives '(
  ("gnu" . "https://elpa.gnu.org/packages/")
  ("gnu-devel" . "https://elpa.gnu.org/devel/")
  ("nongnu" . "https://elpa.nongnu.org/nongnu/")
  ("tromey" . "http://tromey.com/elpa/")
  ("melpa" . "https://melpa.org/packages/")))

(setq package-user-dir (expand-file-name "elpa/" user-emacs-directory))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-verbose nil)

;;
;; End package management

;; start every frame maximized
(add-to-list 'default-frame-alist '(fullscreen . fullboth))
;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;; undo-tree
(use-package undo-tree
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-auto-save-history nil))

;;; completion / vertico
(use-package vertico
  :config
  (vertico-mode))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
	read-buffer-completion-ignore-case t
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :config)

(use-package embark
  :config
  (keymap-set minibuffer-mode-map "M-." #'embark-collect)
  (setq embark-indicators
	'(embark-highlight-indicator
	  embark-isearch-highlight-indicator)))

(use-package embark-consult)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-separator "  ")
  (setq which-key-prefix-prefix "... ")
  (setq which-key-max-display-columns 3)
  
  (setq which-key-idle-secondary-delay 0.25)
  (setq which-key-add-column-padding 1)
  (setq which-key-max-description-length 40)
  
  (setq which-key-idle-delay 0.3)
  (setq which-key-popup-type 'side-window)

  ;; location of which-key window. valid values: top, bottom, left, right,
  ;; or a list of any of the two. If it's a list, which-key will always try
  ;; the first location first. It will go to the second location if there is
  ;; not enough room to display any keys in the first location
  (setq which-key-side-window-location 'bottom)

  ;; max width of which-key window, when displayed at left or right.
  ;; valid values: number of columns (integer), or percentage out of current
  ;; frame's width (float larger than 0 and smaller than 1)
  (setq which-key-side-window-max-width 0.33)

  ;; max height of which-key window, when displayed at top or bottom.
  ;; valid values: number of lines (integer), or percentage out of current
  ;; frame's height (float larger than 0 and smaller than 1)
  (setq which-key-side-window-max-height 0.25))

;;; TOML
(use-package toml-mode)

(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (clojure-mode . rainbow-delimiters-mode)
	 (rust-mode . rainbow-delimiters-mode)
	 (rustic-mode . rainbow-delimiters-mode)
	 (c++-mode . rainbow-delimiters-mode))
  :config)

;;; tree-sitter
(use-package tree-sitter)
(use-package tree-sitter-langs)
;; (global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(use-package xref)
  ;; :config
  ;; (setq xref-search-program 'rg))

(use-package no-littering)
(use-package nerd-icons
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :config
  ;; Use human readable file size in ibuffer.
  (setq  nerd-icons-ibuffer-human-readable-size t))

;;
;; Nice flat modeline
;; https://www.youtube.com/watch?v=E1u6DcHis9M
(defvar mode-line-height 1
  "Modeline height.")

(defun flat-style (theme &rest args)
  (custom-set-faces
   `(mode-line
	 ((t (:inherit mode-line
				   :box (:line-width, mode-line-height :style flat-button)))) t)
   `(mode-line-inactive
	 ((t (:inherit mode-line-inactive
				   :box (:line-width, mode-line-height :style flat-button)))) t)
   ))
(advice-add 'load-theme :after #'flat-style)

(use-package gruvbox-theme)
(use-package gruber-darker-theme)
(use-package flatland-theme)
(use-package zenburn-theme)
(use-package dream-theme)
(use-package modus-themes)
;; (load-theme 'gruber-darker t)
;; (load-theme 'doom-one t)
;; (load-theme 'zenburn t)
(load-theme 'modus-vivendi-tinted t)

(use-package command-log-mode)

(use-package all-the-icons)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package rainbow-mode
  :diminish
  :hook org-mode prog-mode)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :custom
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode 1))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :after ivy
  :ensure t
  :init (ivy-rich-mode 1) ;; this gets us descriptions in M-x.
  :custom
  (ivy-virtual-abbreviate 'full
   ivy-rich-switch-buffer-align-virtual-buffer t
   ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 35      ;; sets modeline height
        doom-modeline-bar-width 5    ;; sets right bar width
        doom-modeline-persp-name t   ;; adds perspective name to modeline
        doom-modeline-persp-icon t)) ;; adds folder icon next to persp name

(use-package neotree
  :config
  (global-set-key (kbd "C-c C-t") 'neotree-toggle)
  (setq neo-smart-open t
        neo-show-hidden-files t
        neo-window-width 55
        neo-window-fixed-size nil
        inhibit-compacting-font-caches t
        projectile-switch-project-action 'neotree-projectile-action) 
  ;; truncate long file names in neotree
  (add-hook 'neo-after-create-hook
            #'(lambda (_)
		(with-current-buffer (get-buffer neo-buffer-name)
                  (setq truncate-lines t)
                  (setq word-wrap nil)
                  (make-local-variable 'auto-hscroll-mode)
                  (setq auto-hscroll-mode nil)))))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ;; ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile)
  ;; :config
  ;; (counsel-projectile-mode 1))

(use-package flycheck-projectile)

;;; perspective
(use-package perspective
  :bind
  (("C-x b" . persp-switch-to-buffer*))
  :custom
  (persp-mode-prefix-key (kbd "C-c C-j")))

(use-package posframe)

(use-package rustic
  :hook ((rustic-mode . flycheck-mode))
  :config
  ;; comment to disable rustfmt on save
  ;; (setq rustic-format-on-save t)
  )

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
  (global-set-key (kbd "C-c C-h") 'lsp-describe-thing-at-point))

(use-package lsp-treemacs
  :after lsp)

(use-package lsp-ivy)

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

(use-package terminal-here
  :config
  (setq terminal-here-windows-terminal-command 'alacritty))
  
(use-package dashboard
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Emacs Is ... Everything ??!!!!")
  (setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  ;; (setq dashboard-startup-banner "~/.config/emacs/images/dtmacs-logo.png")  ;; use custom image as banner
  (setq dashboard-center-content t) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)
                          (agenda . 5 )
                          (bookmarks . 3)
                          (projects . 3)
                          (registers . 3)))
  :custom 
  (dashboard-modify-heading-icons '((recents . "file-text")
				    (bookmarks . "book")))
  :config
  (dashboard-setup-startup-hook))

;;; yasnippet
(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (yas-global-mode 1)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

;;; markdown mode
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

;;; flycheck
(use-package flycheck
  :defer t
  :init (global-flycheck-mode))

(use-package selectrum
  :init
  (selectrum-mode)
  :custom
  (completion-styles '(flex substring partial-completion)))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first")))

(use-package dired-single)

(use-package glsl-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.comp\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode)))

(use-package company-glsl
  :config
  (when (executable-find "glslangValidator")
    (add-to-list 'company-backends 'company-glsl)))

;; ;; Some common sense settings

;;; winum
(use-package winum
  :config
  (setq winum-scope 'frame-local)
  (global-set-key (kbd "M-0") 'neotree-toggle)
  (global-set-key (kbd "M-1") 'winum-select-window-1)
  (global-set-key (kbd "M-2") 'winum-select-window-2)
  (global-set-key (kbd "M-3") 'winum-select-window-3)
  (global-set-key (kbd "M-4") 'winum-select-window-4)
  (global-set-key (kbd "M-5") 'winum-select-window-5)
  (global-set-key (kbd "M-6") 'winum-select-window-6)
  (global-set-key (kbd "M-7") 'winum-select-window-7)
  (global-set-key (kbd "M-8") 'winum-select-window-8)
  (global-set-key (kbd "M-9") 'winum-select-window-9)
  (winum-mode))

(use-package ibuffer
  :config (global-set-key (kbd "C-x C-b") 'ibuffer))

(use-package multiple-cursors  
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C->") 'mc/mark-all-like-this))

(use-package move-text
  :config (move-text-default-bindings))

(use-package olivetti
  :config (setq olivetti-style 'fancy))

(use-package gitignore-templates
  :defer t)

(use-package cmake-mode)

(add-hook 'c-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)

(use-package pdf-tools)
(pdf-loader-install)

;;; misc settings
(set-face-attribute 'default nil :font "Iosevka Nerd Font" :weight 'regular :height 160)
(set-face-attribute 'fixed-pitch nil :font "Iosevka NFM" :weight 'light :height 160)
(set-face-attribute 'variable-pitch nil :font "Iosevka NFM" :weight 'light :height 160)
(global-set-key (kbd "C-x k") 'adi/kill-buffer-current)
(tooltip-mode nil)

;;
;; eldoc
;; (use-package eldoc-box
;;   :hook ((eldoc-mode-hook . eldoc-box-hover-mode))
;;   :config (global-set-key (kbd "C-h D") #'eldoc-box-help-at-point))

;;; doom themes
(use-package doom-themes  
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

(use-package eat)
(use-package vterm)

;;
;; Ripgrep
;; https://www.youtube.com/watch?v=4qLD4oHOrlc
(use-package rg
  :config
  (setq rg-group-result t)
  (setq rg-hide-command t)
  (setq rg-show-columns t)
  (setq rg-ignore-case nil)
  (setq rg-show-header t)
  (setq rg-custom-type-aliases nil)
  (setq rg-default-alias-fallback "all")

  (rg-define-search adi/grep-project-or-current-dir
    "Execute ripgrep in the project root/current directory"
	:query ask
	:format regexp
	:files "everything"
	:dir (let ((vc (vc-root-dir)))
		   (if vc
			   vc
			 default-directory))
	:confirm prefix
	:flags ("--hidden -g !.git"))

  :bind (("M-s g" . adi/grep-project-or-current-dir)
		 :map rg-mode-map
		 ("C-n" . next-line)
		 ("C-p" . previous-line)
		 ("M-n" . rg-next-file)
		 ("M-p" . rg-prev-file)))

(use-package emacs
  :config
  (defvar adi/window-configuration nil
	"Current window configuration")

  (define-minor-mode adi/window-single-toggle
	"Toggle between multiple windows and single window"
	:lighter " [M]"
	:global nil
	(if (one-window-p)
		(when adi/window-configuration
		  (set-window-configuration adi/window-configuration))
	  (setq adi/window-configuration (current-window-configuration))
	  (delete-other-windows)))

  (defun adi/kill-buffer-current (&optional arg)
	"Kill current buffer or abort recursion when in minibuffer."
	(interactive "P")
	(if (minibufferp)
		(abort-recursive-edit)
	  (kill-buffer (current-buffer)))
	(when (and arg
			   (not (one-window-p)))
	  (delete-window)))

  :bind (("s-m" . adi/window-single-toggle)
		 ("s-k" . adi/kill-buffer-current)))  


;;; Directional window motions (windmove)
;; (setq windmove-create-window nil)     ; Emacs 27.1
(global-set-key (kbd "C-M-<up>") #'windmove-up)
(global-set-key (kbd "C-M-<right>") #'windmove-right)
(global-set-key (kbd "C-M-<down>") #'windmove-down)
(global-set-key (kbd "C-M-<left>") #'windmove-left)
(global-set-key (kbd "C-M-S-<up>") #'windmove-swap-states-up)
(global-set-key (kbd "C-M-S-<right>") #'windmove-swap-states-right)
(global-set-key (kbd "C-M-S-<down>") #'windmove-swap-states-down)
(global-set-key (kbd "C-M-S-<left>") #'windmove-swap-states-left)

;;
;; Window size
(global-set-key (kbd "C-x _") #'balance-windows)      ; underscore
(global-set-key (kbd "C-x -") #'fit-window-to-buffer) ; hyphen
(global-set-key (kbd "C-x +")  #'balance-windows-area)
(global-set-key (kbd "C-x }")  #'enlarge-window)
(global-set-key (kbd "C-x {")  #'shrink-window)
(global-set-key (kbd "C-x >")  #'enlarge-window-horizontally) ; override `scroll-right'
(global-set-key (kbd "C-x <")  #'shrink-window-horizontally) ; override `scroll-left'

;;
;; Window resizing
(global-set-key (kbd "C-x _") #'balance-windows)
(global-set-key (kbd "C-x -") #'fit-window-to-buffer)
(global-set-key (kbd "C-x +") #'balance-windows-area)
(global-set-key (kbd "C-x }") #'enlarge-window)
(global-set-key (kbd "C-x {") #'shrink-window)
(global-set-key (kbd "C-x >") #'enlarge-window-horizontally)
(global-set-key (kbd "C-x <") #'shrink-window-horizontally)
(prot-emacs-keybind resize-window-repeat-map
					">" #'enlarge-window-horizontally
					"<" #'shrink-window-horizontally)

(setq-default fill-column 80)
(setq column-number-mode t)
(setq tab-width 4)

(recentf-mode 1)
(setq recentf-max-saved-items 100
      inhibit-startup-message t
      ring-bell-function 'ignore)

(tool-bar-mode -1)
(menu-bar-mode -1)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; (global-hl-line-mode 1)
(global-visual-line-mode 1)
;; (global-display-line-numbers-mode 1)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)

(setq scroll-step 1
	  scroll-conservatively 10000
	  mouse-wheel-progressive-speed 1
	  mouse-wheel-scroll-amount '(1 ((shift) . 1)))

(provide 'init)
;;; init.el ends here
