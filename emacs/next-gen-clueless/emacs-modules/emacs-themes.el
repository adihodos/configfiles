;; emacs-themes.el --- themes config file -*- lexical-binding: t -*-
;;; Code:


;;;; Pulsar
;; Read the pulsar manual: <https://protesilaos.com/emacs/pulsar>.
(use-package pulsar
  :ensure t
  :config
  (setopt pulsar-pulse t
          pulsar-delay 0.055
          pulsar-iterations 10
          pulsar-face 'pulsar-magenta
          pulsar-highlight-face 'pulsar-cyan)
  (pulsar-global-mode 1)
  :hook

  ;; There are convenience functions/commands which pulse the line using
  ;; a specific colour: `pulsar-pulse-line-red' is one of them.
  ((next-error . (pulsar-pulse-line-red pulsar-recenter-top pulsar-reveal-entry))
   (minibuffer-setup-hook pulsar-pulse-line-red))
  :bind
  ;; pulsar does not define any key bindings.  This is just my personal
  ;; preference.  Remember to read the manual on the matter.  Evaluate:
  ;;
  ;; (info "(elisp) Key Binding Conventions")
  (
    ("C-x l" . pulsar-pulse-line) ; override `count-lines-page'
    ("C-x L" . pulsar-highlight-dwim)) ; or use `pulsar-highlight-line'
  )

;;;; Lin
;; Read the lin manual: <https://protesilaos.com/emacs/lin>.
(use-package lin
  :ensure t
  :hook (after-init . lin-global-mode)
  :config
  ;; You can use this to live update the face:
  ;;
  ;; (customize-set-variable 'lin-face 'lin-green)
  ;;
  ;; Or `setopt' on Emacs 29: (setopt lin-face 'lin-yellow)
  ;;
  ;; I still prefer `setq' for consistency.
  (setq lin-face 'lin-magenta))

 ;;  appearance (cursory)
;; Read the manual: <https://protesilaos.com/emacs/cursory>.
(use-package cursory
  :ensure t
  :demand t
  :if (display-graphic-p)
  :config
  (setq cursory-presets
        '((box
           :blink-cursor-interval 1.2)
          (box-no-blink
           :blink-cursor-mode -1)
          (bar
           :cursor-type (bar . 2)
           :blink-cursor-interval 0.5)
          (bar-no-other-window
           :inherit bar
           :cursor-in-non-selected-windows nil)
          (underscore
           :cursor-type (hbar . 3)
           :blink-cursor-blinks 50)
          (underscore-thin-other-window
           :inherit underscore
           :cursor-in-non-selected-windows (hbar . 1))
          (underscore-thick
           :cursor-type (hbar . 8)
           :blink-cursor-interval 0.3
           :blink-cursor-blinks 50
           :cursor-in-non-selected-windows (hbar . 3))
          (underscore-thick-no-blink
           :blink-cursor-mode -1
           :cursor-type (hbar . 8)
           :cursor-in-non-selected-windows (hbar . 3))
          (t ; the default values
           :cursor-type box
           :cursor-in-non-selected-windows hollow
           :blink-cursor-mode 1
           :blink-cursor-blinks 10
           :blink-cursor-interval 0.2
           :blink-cursor-delay 0.2)))

  ;; I am using the default values of `cursory-latest-state-file'.

  ;; Set last preset or fall back to desired style from `cursory-presets'.
  (cursory-set-preset (or (cursory-restore-latest-preset) 'box))
  :hook
  ;; The other side of `cursory-restore-latest-preset'.
  (kill-emacs . cursory-store-latest-preset)
  :bind
  ;; We have to use the "point" mnemonic, because C-c c is often the
  ;; suggested binding for `org-capture' and is the one I use as well.
  ("C-c p" . cursory-set-preset))

;;
;; Look & feel
;; (use-package modus-themes
;;   :config
;;   (load-theme 'modus-vivendi-tinted t))

(use-package gruber-darker-theme
  :ensure t)

  ;; :config
  ;; (load-theme 'gruber-darker t))

(use-package catppuccin-theme
  :ensure t
  :config
  (setq catppuccin-height-title1 1.5))
  
  ;; (load-theme 'catppuccin t))
;; (load-theme 'gruber-darker t)
;; (load-theme 'doom-one t)
;; (load-theme 'zenburn t)
;; (load-theme 'modus-vivendi-tinted t)

(use-package emacs
  :ensure nil
  :config
  ;; (load-theme 'modus-operandi-tinted t)
  (load-theme 'gruber-darker)
  (defvar adi/default-font "Iosevka NFM")
  (defvar adi/font-variable-pitch "Iosevka NFP")
  (defvar adi/default-font-size 160 "Size of the default font.")
  (set-face-attribute 'default nil :font adi/default-font :weight 'regular :height adi/default-font-size)
  (set-face-attribute 'fixed-pitch nil :font adi/default-font :weight 'regular :height adi/default-font-size)
  (set-face-attribute 'variable-pitch nil :font adi/font-variable-pitch :weight 'regular :height adi/default-font-size))

(provide 'emacs-themes)
;; emacs-themes.el ends here
