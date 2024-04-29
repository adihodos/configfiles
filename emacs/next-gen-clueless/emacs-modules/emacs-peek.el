
(use-package peek
  :ensure t
  ;; :straight (:type git :host sourcehut :repo "meow_king/peek")
  :after (eldoc)
  :vc (peek :url "https://git.sr.ht/~meow_king/peek" :branch "main")
  :custom
  ;; only list some settings that are wanted to be chaned by most people
  (peek-overlay-window-size 11)  ; lines
  ;; you can also set `peek-overlay-border-character' to nil to achieve a similar
  ;; looking as `make-separator-line', which is useful when you find there is a wrong
  ;; number of border characters when using default settings. However, in this case,
  ;; please consider report a bug.
  (peek-overlay-border-character ?\N{BOX DRAWINGS LIGHT HORIZONTAL})

  (peek-overlay-position 'above)  ; or below
  (peek-overlay-distance 4)  ; the distance between peek view and the cursor point

  ;; one line before the place found by `peek-definition' will also appear
  ;; in peek window. Note `peek-definition' is the underlying function of
  ;; `peek-xref-definition'
  (peek-definition-surrounding-above-lines 1)

  (peek-live-update t)  ; live update peek view of a marked region

  (peek-enable-eldoc-message-integration t)  ; enable `eldoc-message-function' integration
  ;; eldoc message overlay at two lines below the point
  ;; It's recommended to set the eldoc message overlay below the point since the pop up of
  ;; the peek overlay may cause visual shaking
  (peek-eldoc-message-overlay-position 2)

  ;; enable `eldoc-display-functons'  integration
  ;; note: you need Emacs version >= 28.1
  (peek-enable-eldoc-display-integration t)

  ;; Eldoc display setting
  ;; Besides making `peek-enable-eldoc-display-integration' to t, you may want to remove
  ;;   other eldoc display functions.
  (remove-hook 'eldoc-display-functions 'eldoc-display-in-buffer)
  :config
  (global-peek-mode 1)
  ;; you may also want to set scroll margin (see its docs)
  (setq-default scroll-margin 5)

  :bind (
		 ;; Keybindings
		 ;; default keybindings in peek-mode-keymap
		 :map peek-mode-keymap
		 ( "M-n" . peek-next-line)
		 ( "M-p" . peek-prev-line)

		 :map global-map
		 ;; or you can use `keymap-global-set', which is introduced in emacs 29
		 ("C-x P p" . peek-overlay-dwim)
		 ("C-x P d" . peek-xref-definition)
		 ("C-x P m" . peek-overlay-eldoc-message-toggle-stauts)
		 ("C-x P h" . eldoc)))

(provide 'emacs-peek)
