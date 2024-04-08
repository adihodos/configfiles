;; emacs-window.el --- window config file -*- lexical-binding: t -*-
;;; Code:

;; ;; start every frame maximized
(prot-emacs-configure
  (add-to-list 'default-frame-alist '(fullscreen . fullboth))
  ;; Make ESC quit prompts
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit))

;;; Code:
(prot-emacs-configure
 ;;;; `uniquify' (unique names for buffers)
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-strip-common-suffix t)
  (setq uniquify-after-kill-buffer-p t)

  (require 'hl-line)
  (global-hl-line-mode 1)
  (global-visual-line-mode 1)

  (require 'display-line-numbers)
  (global-display-line-numbers-mode 1)
  (setq-default display-line-numbers-type 'relative)
  ;; Those two variables were introduced in Emacs 27.1
  (setq display-line-numbers-major-tick 0)
  (setq display-line-numbers-minor-tick 0)
  ;; Use absolute numbers in narrowed buffers
  (setq-default display-line-numbers-widen t)
  ;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  ;; (add-hook 'prog-mode-hook 'hl-line-mode)
  (prot-emacs-keybind global-map
	"<f6>" #'whitespace-mode
	"<f7>" #'display-line-numbers-mode
	"C-c z" #'delete-trailing-whitespace)

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
  (prot-emacs-keybind global-map
	"C-x <down>" #'next-buffer
	"C-x <up>" #'previous-buffer
	"C-x C-n" #'next-buffer     ; override `set-goal-column'
	"C-x C-p" #'previous-buffer ; override `mark-page'
	"C-x !" #'delete-other-windows-vertically
	"C-x _" #'balance-windows      ; underscore
	"C-x -" #'fit-window-to-buffer ; hyphen
	"C-x +" #'balance-windows-area
	"C-x }" #'enlarge-window
	"C-x {" #'shrink-window
	"C-x >" #'enlarge-window-horizontally ; override `scroll-right'
	"C-x <" #'shrink-window-horizontally) ; override `scroll-left'
  (prot-emacs-keybind resize-window-repeat-map
	">" #'enlarge-window-horizontally
	"<" #'shrink-window-horizontally)

 ;;; Directional window motions (windmove)
  ;; (setq windmove-create-window nil)     ; Emacs 27.1
  (prot-emacs-keybind global-map
	"C-M-<up>" #'windmove-up
	"C-M-<right>" #'windmove-right
	"C-M-<down>" #'windmove-down
	"C-M-<left>" #'windmove-left
	"C-M-S-<up>" #'windmove-swap-states-up
	"C-M-S-<right>" #'windmove-swap-states-right
	"C-M-S-<down>" #'windmove-swap-states-down
	"C-M-S-<left>" #'windmove-swap-states-left))

(prot-emacs-configure
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

  (prot-emacs-keybind global-map
    "s-m" #'adi/window-single-toggle
	"s-k" #'adi/kill-buffer-current))

;; winum
(prot-emacs-package winum
  (:install t)
  
  (setq winum-scope 'frame-local)
  (prot-emacs-keybind global-map
    "M-0" #'neotree-toggle
    "M-1" #'winum-select-window-1
    "M-2" #'winum-select-window-2
    "M-3" #'winum-select-window-3
    "M-4" #'winum-select-window-4
    "M-5" #'winum-select-window-5
    "M-6" #'winum-select-window-6
    "M-7" #'winum-select-window-7
    "M-8" #'winum-select-window-8
    "M-9" #'winum-select-window-9)
  (winum-mode 1))

(provide 'emacs-window)
;; emacs-window.el ends here
