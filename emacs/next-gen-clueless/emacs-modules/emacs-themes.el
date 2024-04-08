;; emacs-themes.el --- themes config file -*- lexical-binding: t -*-
;;; Code:

;; (prot-emacs-package pulsar
;;     (:install t)
;;     (setq pulsar-pulse t)
;;     (setq pulsar-delay 0.055)
;;     (setq pulsar-iterations 10)
;;     (setq pulsar-face 'pulsar-magenta)
;;     (setq pulsar-highlight-face 'pulsar-yellow)
;;     (pulsar-global-mode 1))

;;;; Pulsar
;; Read the pulsar manual: <https://protesilaos.com/emacs/pulsar>.
(prot-emacs-package pulsar
  (:install t)
  (:delay 1)
  (setopt pulsar-pulse t
          pulsar-delay 0.055
          pulsar-iterations 10
          pulsar-face 'pulsar-magenta
          pulsar-highlight-face 'pulsar-cyan)

  (pulsar-global-mode 1)

  ;; There are convenience functions/commands which pulse the line using
  ;; a specific colour: `pulsar-pulse-line-red' is one of them.
  (add-hook 'next-error-hook #'pulsar-pulse-line-red)
  (add-hook 'next-error-hook #'pulsar-recenter-top)
  (add-hook 'next-error-hook #'pulsar-reveal-entry)

  (add-hook 'minibuffer-setup-hook #'pulsar-pulse-line-red)

  ;; pulsar does not define any key bindings.  This is just my personal
  ;; preference.  Remember to read the manual on the matter.  Evaluate:
  ;;
  ;; (info "(elisp) Key Binding Conventions")
  (prot-emacs-keybind global-map
    "C-x l" #'pulsar-pulse-line ; override `count-lines-page'
    "C-x L" #'pulsar-highlight-dwim)) ; or use `pulsar-highlight-line'

;;;; Lin
;; Read the lin manual: <https://protesilaos.com/emacs/lin>.
(prot-emacs-package lin
  (:install t)
  (:delay 1)
  ;; You can use this to live update the face:
  ;;
  ;; (customize-set-variable 'lin-face 'lin-green)
  ;;
  ;; Or `setopt' on Emacs 29: (setopt lin-face 'lin-yellow)
  ;;
  ;; I still prefer `setq' for consistency.
  (setq lin-face 'lin-magenta)

  (lin-global-mode 1)) ; applies to all `lin-mode-hooks'

;;; Cursor appearance (cursory)
;; Read the manual: <https://protesilaos.com/emacs/cursory>.
(prot-emacs-package cursory
  (:install t)
  (:delay 1)
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

  ;; The other side of `cursory-restore-latest-preset'.
  (add-hook 'kill-emacs-hook #'cursory-store-latest-preset)

  ;; We have to use the "point" mnemonic, because C-c c is often the
  ;; suggested binding for `org-capture' and is the one I use as well.
  (define-key global-map (kbd "C-c p") #'cursory-set-preset))


;;
;; Look & feel
;; (use-package modus-themes
;;   :config
;;   (load-theme 'modus-vivendi-tinted t))

(prot-emacs-package gruber-darker-theme
  (:install t)
  (:delay 1)
  (load-theme 'gruber-darker t))

;; (use-package catppuccin-theme
;;   :config
;;   (setq catppuccin-height-title1 1.5)
;;   (load-theme 'catppuccin t))
;; (load-theme 'gruber-darker t)
;; (load-theme 'doom-one t)
;; (load-theme 'zenburn t)
;; (load-theme 'modus-vivendi-tinted t)

;;
;; Nice Flat Modeline
;; Https://www.youtube.com/watch?v=E1u6DcHis9M
(defvar mode-line-height 1 "Modeline height.")

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

(defvar adi/default-font-size 180 "Size of the default font.")
(set-face-attribute 'default nil :font adi/default-font :weight 'regular :height adi/default-font-size)
(set-face-attribute 'fixed-pitch nil :font adi/default-font :weight 'regular :height adi/default-font-size)
(set-face-attribute 'variable-pitch nil :font adi/font-variable-pitch :weight 'regular :height adi/default-font-size)

(provide 'emacs-themes)
;; emacs-themes.el ends here
