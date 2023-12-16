;;
;; My epic EMACS init file :)
;;

;;; Code:


;; (setq-default shell-file-name "/bin/bash")


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
;; straight package manager
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
(setq straight-use-package-by-default t)

;;
;; DASHBOARD
(use-package dashboard
  :config
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-banner-logo-title "Emacs Is ... Everything ??!!!!")
  (setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  (setq dashboard-center-content t) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)
                          (agenda . 5 )
                          (bookmarks . 3)
                          ;; (projects . 3)
                          (registers . 3)))
  (setq dashboard-display-icons-p t) ;; display icons on both GUI and terminal
  (setq dashboard-icon-type 'all-the-icons) ;; use `nerd-icons' package
  ;; (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (dashboard-modify-heading-icons '((recents . "file-text")
									(bookmarks . "book")))
  (dashboard-setup-startup-hook))

(use-package page-break-lines
  :config (global-page-break-lines-mode))

;;
;; which-key
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


;;
;; Configuration
;; start every frame maximized
(add-to-list 'default-frame-alist '(fullscreen . fullboth))
;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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

;;
;;
(use-package rust-mode)
;;; TOML
(use-package toml-mode)

(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (clojure-mode . rainbow-delimiters-mode)
		 (rust-mode . rainbow-delimiters-mode)
		 (rustic-mode . rainbow-delimiters-mode)
		 (c++-mode . rainbow-delimiters-mode)))


;;
;; DOOM modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 35      ;; sets modeline height
        doom-modeline-bar-width 5    ;; sets right bar width
        doom-modeline-persp-name t   ;; adds perspective name to modeline
        doom-modeline-persp-icon t)) ;; adds folder icon next to persp name

;;
;; better help
(use-package helpful
  :bind
  ;; Note that the built-in `describe-function' includes both functions
  ;; and macros. `helpful-function' is functions only, so we provide
  ;; `helpful-callable' as a drop-in replacement.

  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

;;undo-tree
(use-package undo-tree
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-auto-save-history nil))

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
;; Look & feel
(use-package modus-themes)
;; (load-theme 'gruber-darker t)
;; (load-theme 'doom-one t)
;; (load-theme 'zenburn t)
(load-theme 'modus-vivendi-tinted t)

(use-package all-the-icons)
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;;
;; icons
(use-package no-littering)
(use-package nerd-icons
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode)
  :config
  ;; Use human readable file size in ibuffer.
  (setq  nerd-icons-ibuffer-human-readable-size t))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package neotree
  :config
  (global-set-key (kbd "C-c C-t") 'neotree-toggle)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
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

(when (featurep 'dired)
	  (progn
		 (setq dired-recursive-copies 'always)
		 (setq dired-recursive-deletes 'always)
		 (setq delete-by-moving-to-trash t)
		 (setq dired-listing-switches
			   "-AGFhlv --group-directories-first --time-style=long-iso")
		 (setq dired-dwim-target t)
		 (setq dired-auto-revert-buffer #'dired-directory-changed-p) ; also see `dired-do-revert-buffer'
		 (setq dired-make-directory-clickable t) ; Emacs 29.1
		 (setq dired-free-space nil) ; Emacs 29.1
		 (setq dired-mouse-drag-files t) ; Emacs 29.1
		 (setq dired-guess-shell-alist-user ; those are the suggestions for ! and & in Dired
			   '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open")
				 ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open")
				 (".*" "xdg-open")))

		 ;; (add-hook 'dired-mode-hook #'dired-hide-details-mode)
		 (add-hook 'dired-mode-hook #'hl-line-mode)))

(when (featurep 'dired-aux)
  (setq dired-isearch-filenames 'dwim)
  (setq dired-create-destination-dirs 'ask) ; Emacs 27
  (setq dired-vc-rename-file t)             ; Emacs 27
  (setq dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir)))) ; Emacs 28
  (setq dired-create-destination-dirs-on-trailing-dirsep t))

(when (featurep 'dired-x)
  (setq dired-clean-up-buffers-too t)
  (setq dired-clean-confirm-killing-deleted-buffers t)
  (setq dired-x-hands-off-my-keys t)    ; easier to show the keys I use
  (setq dired-bind-man nil)
  (setq dired-bind-info nil)
  (define-key dired-mode-map (kbd "I") #'dired-info))

(when (featurep 'wdired)
  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))

(use-package dired-single)

(use-package glsl-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.comp\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode)))

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

(straight-use-package '( vertico :files (:defaults "extensions/*")
                         :includes (vertico-buffer
                                    vertico-directory
                                    vertico-flat
                                    vertico-indexed
                                    vertico-mouse
                                    vertico-quick
                                    vertico-repeat
                                    vertico-reverse)))

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  (setq vertico-scroll-margin 0)

  ;; Show more candidates
  (setq vertico-count 4)

  ;; Grow and shrink the Vertico minibuffer
  (setq vertico-resize nil)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t)
  :hook
  ;; This works with `file-name-shadow-mode' enabled.  When you are in
;; a sub-directory and use, say, `find-file' to go to your home '~/'
;; or root '/' directory, Vertico will clear the old path to keep
;; only your current input.
  (rfn-eshadow-update-overlay-hook . vertico-directory-tidy))

;; Configure directory extension.
(use-package vertico-directory
  :after vertico
  :ensure nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package vertico-flat
  :after vertico
  :ensure nil)

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package marginalia
  :config
  (marginalia-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))


(use-package pulsar
  :config
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.055)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'pulsar-magenta)
  (setq pulsar-highlight-face 'pulsar-yellow)
  (pulsar-global-mode 1))

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

(setq-default fill-column 80)
(setq column-number-mode t)
(setq tab-width 4)

(recentf-mode 1)
(setq recentf-max-saved-items 100
      inhibit-startup-message t
      ring-bell-function 'ignore)

(global-hl-line-mode 1)
(global-visual-line-mode 1)
(global-display-line-numbers-mode 1)
;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)
;; (add-hook 'prog-mode-hook 'hl-line-mode)

;; (setq scroll-step 1
;;       scroll-conservatively 10000
;;       mouse-wheel-progressive-speed 1
;;       mouse-wheel-scroll-amount '(1 ((shift) . 1)))

(defvar adi/default-font "Iosevka Nerd Font" "Font to use")
;; (defvar adi/default-font "JetBrainsMono Nerd Font" "Font to use")
(defvar adi/default-font-size 160 "Size of the default font")
(set-face-attribute 'default nil :font adi/default-font :weight 'regular :height adi/default-font-size)
(set-face-attribute 'fixed-pitch nil :font adi/default-font :weight 'light :height adi/default-font-size)
(set-face-attribute 'variable-pitch nil :font adi/default-font :weight 'light :height adi/default-font-size)
(global-set-key (kbd "C-x k") 'adi/kill-buffer-current)

;; (tooltip-mode nil)

(dolist (path '("emacs-modules"))
  (add-to-list 'load-path (locate-user-emacs-file path)))

;; (add-to-list 'load-path #'locate-user-emacs-file)
(require 'essentials)
(require 'lsp-bridge-cfg)
(provide 'init)
;;; init.el ends here
