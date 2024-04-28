;; emacs-window.el --- window config file -*- lexical-binding: t -*-
;;; Code:

;; ;; start every frame maximized
(use-package emacs
  :ensure nil
  :config
  (add-to-list 'default-frame-alist '(fullscreen . fullboth))
  ;; Make ESC quit prompts
  :bind ("<escape>" . keyboard-escape-quit))

;;; Code:
(use-package uniquify
  :ensure nil
  :config
  (global-hl-line-mode 1)
  (global-visual-line-mode 1)
 ;;;; `uniquify' (unique names for buffers)
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-strip-common-suffix t)
  (setq uniquify-after-kill-buffer-p t))

;;;; Negative space highlight
(use-package whitespace
  :ensure nil
  :bind
  (("<f6>" . whitespace-mode)
   ("C-c z" . delete-trailing-whitespace))
  :config
  ;; NOTE 2023-08-14: This is experimental.  I am not sure I like it.
  (setq whitespace-style
        '(face
          tabs
          spaces
          tab-mark
          space-mark
          trailing
          missing-newline-at-eof
          space-after-tab::tab
          space-after-tab::space
          space-before-tab::tab
          space-before-tab::space)))

;;; Line numbers on the side of the window
(use-package display-line-numbers
  :ensure nil
  :config
  (global-display-line-numbers-mode t)
  (setq-default display-line-numbers-type t)
  (setq-default display-line-numbers-type 'relative)
  ;; Those two variables were introduced in Emacs 27.1
  (setq display-line-numbers-major-tick 0)
  (setq display-line-numbers-minor-tick 0)
  ;; Use absolute numbers in narrowed buffers
  (setq-default display-line-numbers-widen t))

(use-package emacs
  :ensure nil
  :config
  ;;
  ;; Some window settings
  (setq window-combination-resize t)
  (setq even-window-sizes 'height-only)
  (setq window-sides-vertical nil)
  (setq switch-to-buffer-in-dedicated-window 'pop)
  (setq split-height-threshold 80)
  (setq split-width-threshold 125)
  (setq window-min-height 3)
  (setq window-min-width 30)

  ;;
  ;; Window related keybindings
  :bind
  (:map global-map
	("C-x <down>" .  next-buffer)
	("C-x <up>" .  previous-buffer)
	("C-x C-n" .  next-buffer)     ; override `set-goal-column'
	("C-x C-p" .  previous-buffer) ; override `mark-page'
	("C-x !" .  delete-other-windows-vertically)
	("C-x _" .  balance-windows)      ; underscore
	("C-x -" .  fit-window-to-buffer) ; hyphen
	("C-x +" .  balance-windows-area)
	("C-x }" .  enlarge-window)
	("C-x {" .  shrink-window)
	("C-x >" .  enlarge-window-horizontally) ; override `scroll-right'
	("C-x <" .  shrink-window-horizontally) ; override `scroll-left'
	:map resize-window-repeat-map
	(">" . enlarge-window-horizontally)
	("<" . shrink-window-horizontally)))

;;; Frame-isolated buffers
;; <https://protesilaos.com/emacs/beframe>.
(use-package beframe
  :ensure t
  :hook (after-init . beframe-mode)
  :config
  (setq beframe-functions-in-frames '(project-prompt-project-dir))

  ;; I use this instead of :bind because I am binding a keymap and the
  ;; way `use-package' does it is by wrapping a lambda around it that
  ;; then breaks `describe-key' for those keys.
  (prot-emacs-keybind global-map
    ;; Override the `set-fill-column' that I have no use for.
    "C-x f" #'other-frame-prefix
    ;; Bind Beframe commands to a prefix key. Notice the -map as I am
    ;; binding keymap here, not a command.
    "C-c b" #'beframe-prefix-map
    ;; Replace the generic `buffer-menu'.  With a prefix argument, this
    ;; commands prompts for a frame.  Call the `buffer-menu' via M-x if
    ;; you absolutely need the global list of buffers.
    "C-x C-b" #'beframe-buffer-menu
    ;; Not specific to Beframe, but since it renames frames (by means
    ;; of `beframe-mode') it is appropriate to have this here:
    "C-x B" #'select-frame-by-name))

;;; Frame history (undelete-frame-mode)
(use-package frame
  :ensure nil
  :bind ("C-x u" . undelete-frame) ; I use only C-/ for `undo'
  :hook (after-init . undelete-frame-mode))

;;; Window history (winner-mode)
(use-package winner
  :ensure nil
  :hook (after-init . winner-mode)
  :bind
  (("C-x <right>" . winner-redo)
   ("C-x <left>" . winner-undo)))

;;; Directional window motions (windmove)
(use-package windmove
  :ensure nil
  :bind
  ;; Those override some commands that are already available with
  ;; C-M-u, C-M-f, C-M-b.
  (("C-M-<up>" . windmove-up)
   ("C-M-<right>" . windmove-right)
   ("C-M-<down>" . windmove-down)
   ("C-M-<left>" . windmove-left)
   ("C-M-S-<up>" . windmove-swap-states-up)
   ("C-M-S-<right>" . windmove-swap-states-right) ; conflicts with `org-increase-number-at-point'
   ("C-M-S-<down>" . windmove-swap-states-down)
   ("C-M-S-<left>" . windmove-swap-states-left))
  :config
  (setq windmove-create-window nil)) ; Emacs 27.1

;; Header line context of symbol/heading (breadcrumb.el)
(use-package breadcrumb
  :ensure t
  ;; :hook (after-init . breadcrumb-mode)
  :bind ("<f7>" . breadcrumb-mode)
  :config
  (setq breadcrumb-project-max-length 0.5)
  (setq breadcrumb-project-crumb-separator "/")
  (setq breadcrumb-imenu-max-length 1.0)
  (setq breadcrumb-imenu-crumb-separator " > "))

(use-package emacs
  :ensure nil
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

  :bind (:map global-map
	      ("s-m" . adi/window-single-toggle)
	      ("s-k" . adi/kill-buffer-current)))

(use-package emacs
  :ensure nil
  :if (eq system-type 'windows-nt)
  :config
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super) ; Left Windows key
  (w32-register-hot-key [s-]) ; disable all Windows shortcuts while Emacs has focus
  (setq w32-pass-rwindow-to-system nil)
  (setq w32-rwindow-modifier 'super) ; Right Windows key
  (setq w32-apps-modifier 'hyper)) ; Menu/App key

(provide 'emacs-window)
;; emacs-window.el ends here
