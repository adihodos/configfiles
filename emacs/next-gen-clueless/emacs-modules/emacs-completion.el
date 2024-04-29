;; emacs-completion.el --- completion config file -*- lexical-binding: t -*-
;;; Code:

(use-package minibuffer
  :ensure nil
  :config
  
;;;; Minibuffer configurations
  (setq completion-styles '(basic substring initials flex orderless)) ; also see `completion-category-overrides'
  (setq completion-category-defaults nil)

  ;; A list of known completion categories:
  ;;
  ;; - `bookmark'
  ;; - `buffer'
  ;; - `charset'
  ;; - `coding-system'
  ;; - `color'
  ;; - `command' (e.g. `M-x')
  ;; - `customize-group'
  ;; - `environment-variable'
  ;; - `expression'
  ;; - `face'
  ;; - `file'
  ;; - `function' (the `describe-function' command bound to `C-h f')
  ;; - `info-menu'
  ;; - `imenu'
  ;; - `input-method'
  ;; - `kill-ring'
  ;; - `library'
  ;; - `minor-mode'
  ;; - `multi-category'
  ;; - `package'
  ;; - `project-file'
  ;; - `symbol' (the `describe-symbol' command bound to `C-h o')
  ;; - `theme'
  ;; - `unicode-name' (the `insert-char' command bound to `C-x 8 RET')
  ;; - `variable' (the `describe-variable' command bound to `C-h v')
  ;; - `consult-grep'
  ;; - `consult-isearch'
  ;; - `consult-kmacro'
  ;; - `consult-location'
  ;; - `embark-keybinding'
  ;;
  (setq completion-category-overrides
        ;; NOTE 2021-10-25: I am adding `basic' because it works better as a
        ;; default for some contexts.  Read:
        ;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=50387>.
        ;;
        ;; `partial-completion' is a killer app for files, because it
        ;; can expand ~/.l/s/fo to ~/.local/share/fonts.
        ;;
        ;; If `basic' cannot match my current input, Emacs tries the
        ;; next completion style in the given order.  In other words,
        ;; `orderless' kicks in as soon as I input a space or one of its
        ;; style dispatcher characters.
        '((file (styles . (basic partial-completion orderless)))
          (bookmark (styles . (basic substring)))
          (library (styles . (basic substring)))
          (embark-keybinding (styles . (basic substring)))
          (imenu (styles . (basic substring orderless)))
          (consult-location (styles . (basic substring orderless)))
          (kill-ring (styles . (emacs22 orderless)))
          (eglot (styles . (emacs22 substring orderless)))))

  (setq completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq-default case-fold-search t)   ; For general regexp

  (setq enable-recursive-minibuffers t)
  (setq read-minibuffer-restore-windows nil) ; Emacs 28
  (minibuffer-depth-indicate-mode 1)

  (setq minibuffer-default-prompt-format " [%s]") ; Emacs 29
  (minibuffer-electric-default-mode 1)

  (setq resize-mini-windows t)
  (setq read-answer-short t) ; also check `use-short-answers' for Emacs28
  (setq echo-keystrokes 0.25)
  (setq kill-ring-max 60) ; Keep it small

  ;; Do not allow the cursor to move inside the minibuffer prompt.  I
  ;; got this from the documentation of Daniel Mendler's Vertico
  ;; package: <https://github.com/minad/vertico>.
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Add prompt indicator to `completing-read-multiple'.  We display
  ;; [`completing-read-multiple': <separator>], e.g.,
  ;; [`completing-read-multiple': ,] if the separator is a comma.  This
  ;; is adapted from the README of the `vertico' package by Daniel
  ;; Mendler.  I made some small tweaks to propertize the segments of
  ;; the prompt.
  (defun crm-indicator (args)
    (cons (format "[`completing-read-multiple': %s]  %s"
                  (propertize
                   (replace-regexp-in-string
                    "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                    crm-separator)
                   'face 'error)
                  (car args))
          (cdr args)))

  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (file-name-shadow-mode 1)

  (setq completions-format 'one-column)
  (setq completion-show-help nil)
  (setq completion-auto-help t)
  (setq completion-auto-select nil)
  (setq completions-detailed t)
  (setq completion-show-inline-help nil)
  (setq completions-max-height 6)
  (setq completions-header-format (propertize "%s candidates:\n" 'face 'font-lock-comment-face))
  (setq completions-highlight-face 'completions-highlight)
  (setq minibuffer-visible-completions t) ; Emacs 30

;;;; `savehist' (minibuffer and related histories)
  (setq savehist-file (locate-user-emacs-file "savehist"))
  (setq history-length 100)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (setq savehist-additional-variables '(register-alist kill-ring))
  (savehist-mode 1)

  ;; Because the *scratch* buffer is produced before we load this, we
  ;; have to explicitly activate the mode there.
  (when-let ((scratch (get-buffer "*scratch*")))
    (with-current-buffer scratch
      (abbrev-mode 1)))

  ;; By default, abbrev asks for confirmation on whether to use
  ;; `abbrev-file-name' to save abbrevations.  I do not need that, nor
  ;; do I want it.
  (remove-hook 'save-some-buffers-functions #'abbrev--possibly-save))

;;; yasnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  ;; (yas-reload-all)
  ;; (add-hook 'prog-mode-hook 'yas-minor-mode)
  ;; (add-hook 'text-mode-hook 'yas-minor-mode)
  )

;;;; `savehist' (minibuffer and related histories)
(use-package savehist
  :ensure t
  :hook (after-init . savehist-mode)
  :config
  (setq savehist-file (locate-user-emacs-file "savehist"))
  (setq history-length 100)
  (setq history-delete-duplicates t)
  (setq savehist-save-minibuffer-history t)
  (add-to-list 'savehist-additional-variables 'kill-ring))

;;; Orderless completion style (and prot-orderless.el)
(use-package orderless
  :ensure t
  :after minibuffer
  :config
  ;; Remember to check my `completion-styles' and the
  ;; `completion-category-overrides'.
  (setq orderless-matching-styles
        '(orderless-prefixes orderless-regexp))

  ;; SPC should never complete: use it for `orderless' groups.
  ;; The `?' is a regexp construct.
  :bind (:map minibuffer-local-completion-map
          ("SPC" . nil)
          ("?" . nil)))

(use-package prot-orderless
  :ensure nil
  :config
  (setq orderless-style-dispatchers
        '(prot-orderless-literal
          prot-orderless-file-ext
          prot-orderless-beg-or-end)))

(use-package rfn-eshadow
  :ensure nil
  :hook (minibuffer-setup . cursor-intangible-mode)
  :config
  ;; Not everything here comes from rfn-eshadow.el, but this is fine.

  (setq resize-mini-windows t)
  (setq read-answer-short t) ; also check `use-short-answers' for Emacs28
  (setq echo-keystrokes 0.25)
  (setq kill-ring-max 60) ; Keep it small

  ;; Do not allow the cursor to move inside the minibuffer prompt.  I
  ;; got this from the documentation of Daniel Mendler's Vertico
  ;; package: <https://github.com/minad/vertico>.
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))


    ;; Add prompt indicator to `completing-read-multiple'.  We display
    ;; [`completing-read-multiple': <separator>], e.g.,
    ;; [`completing-read-multiple': ,] if the separator is a comma.  This
    ;; is adapted from the README of the `vertico' package by Daniel
    ;; Mendler.  I made some small tweaks to propertize the segments of
    ;; the prompt.
    (defun crm-indicator (args)
      (cons (format "[`completing-read-multiple': %s]  %s"
                    (propertize
                     (replace-regexp-in-string
                      "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                      crm-separator)
                     'face 'error)
                    (car args))
            (cdr args)))

    (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (file-name-shadow-mode 1))

  (use-package consult
    :ensure t
    :hook (completion-list-mode . consult-preview-at-point-mode)
    :bind
    (:map global-map
      ("M-g M-g" . consult-goto-line)
      ("M-K" . consult-keep-lines) ; M-S-k is similar to M-S-5 (M-%)
      ("M-F" . consult-focus-lines) ; same principle
      ("M-s M-b" . consult-buffer)
      ("M-s M-f" . consult-find)
      ("M-s M-g" . consult-grep)
      ("M-s M-h" . consult-history)
      ("M-s M-i" . consult-imenu)
      ("M-s M-l" . consult-line)
      ("M-s M-m" . consult-mark)
      ("M-s M-y" . consult-yank-pop)
      ("M-s M-s" . consult-outline)
      :map consult-narrow-map
      ("?" . consult-narrow-help))
    :config
    (setq consult-line-numbers-widen t)
    ;; (setq completion-in-region-function #'consult-completion-in-region)
    (setq consult-async-min-input 3)
    (setq consult-async-input-debounce 0.5)
    (setq consult-async-input-throttle 0.8)
    (setq consult-narrow-key nil)
    (setq consult-find-args
          (concat "find . -not ( "
                  "-path */.git* -prune "
                  "-or -path */.cache* -prune )"))
    (setq consult-preview-key 'any)

    (add-to-list 'consult-mode-histories '(vc-git-log-edit-mode . log-edit-comment-ring))

    (require 'consult-imenu) ; the `imenu' extension is in its own file

    (with-eval-after-load 'pulsar
      ;; see my `pulsar' package: <https://protesilaos.com/emacs/pulsar>
      (setq consult-after-jump-hook nil) ; reset it to avoid conflicts with my function
      (dolist (fn '(pulsar-recenter-top pulsar-reveal-entry))
        (add-hook 'consult-after-jump-hook fn))))

;;; Detailed completion annotations (marginalia.el)
(use-package marginalia
  :ensure t
  :defer 1
  :config
  (setq marginalia-max-relative-age 0) ; absolute time
  (marginalia-mode 1))

;;;; Custom completion annotations
(use-package prot-marginalia
  :ensure nil
  :after marginalia
  :config
  (setq marginalia-annotator-registry
        '((bookmark prot-marginalia-bookmark)
          (buffer prot-marginalia-buffer)
          (command marginalia-annotate-command)
          (function prot-marginalia-symbol)
          (symbol prot-marginalia-symbol)
          (variable prot-marginalia-symbol)
          (face marginalia-annotate-face)
          (imenu marginalia-annotate-imenu)
          (package prot-marginalia-package)
          (unicode-name marginalia-annotate-char))))

;; ;;; Vertical completion layout
;; (use-package vertico
;;   :ensure t
;;   :config
;;   (setq vertico-scroll-margin 0)
;;   (setq vertico-count 5)
;;   (setq vertico-resize nil)
;;   (setq vertico-cycle t)
;;   (vertico-mode 1)
;; 
;;   ;; This works with `file-name-shadow-mode' enabled.  When you are in
;;   ;; a sub-directory and use, say, `find-file' to go to your home '~/'
;;   ;; or root '/' directory, Vertico will clear the old path to keep
;;   ;; only your current input.
;;   :hook (rfn-eshadow-update-overlay  . vertico-directory-tidy))
;; 
;; ;;; Custom tweaks for vertico (prot-vertico.el)
;; (use-package prot-vertico
;;   :ensure nil
;;   :config
;;   (setq vertico-multiform-categories
;;         `(;; Maximal
;;           (embark-keybinding ,@prot-vertico-multiform-maximal)
;;           (multi-category ,@prot-vertico-multiform-maximal)
;;           (consult-location ,@prot-vertico-multiform-maximal)
;;           (imenu ,@prot-vertico-multiform-maximal)
;;           (unicode-name ,@prot-vertico-multiform-maximal)
;;           ;; Minimal
;;           (file ,@prot-vertico-multiform-minimal
;;                 (vertico-preselect . prompt)
;;                 (vertico-sort-function . prot-vertico-sort-directories-first))
;;           (t ,@prot-vertico-multiform-minimal)))
;; 
;;   (vertico-multiform-mode 1)
;;   :bind (:map vertico-map
;; 			  ("<left>" . backward-char)
;; 			  ("<right>" . forward-char)
;; 			  ("TAB" . prot-vertico-private-complete)
;; 			  ("DEL" . vertico-directory-delete-char)
;; 			  ("M-DEL" . vertico-directory-delete-word)
;; 			  ("M-," . vertico-quick-insert)
;; 			  ("M-." . vertico-quick-exit)
;; 
;;   :map vertico-multiform-map
;;   ("C-n" . prot-vertico-private-next)
;;   ("<down>" . prot-vertico-private-next)
;;   ("C-p" . prot-vertico-private-previous)
;;   ("<up>" . prot-vertico-private-previous)
;;   ("C-l" . vertico-multiform-vertical)))

(use-package company
  :ensure t
  :hook ((prog-mode text-mode) . company-mode)
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-show-numbers t)
  :bind ([C-tab] . company-complete))

(use-package company-prescient
  :ensure t
  :after (company)
  :config
  (company-prescient-mode 1))

(use-package company-box
  :ensure t
  :after (company all-the-icons)
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-icons-alist 'company-box-icons-all-the-icons)
	(setq company-box-backends-colors nil)
	(setq company-box-backends-colors nil)

  ;; These are the Doom Emacs defaults
  (setq company-box-icons-all-the-icons
  `((Unknown       . ,(all-the-icons-material "find_in_page"             :face 'all-the-icons-purple))
    (Text          . ,(all-the-icons-material "text_fields"              :face 'all-the-icons-green))
    (Method        . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
    (Function      . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
    (Constructor   . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
    (Field         . ,(all-the-icons-material "functions"                :face 'all-the-icons-red))
    (Variable      . ,(all-the-icons-material "adjust"                   :face 'all-the-icons-blue))
    (Class         . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
    (Interface     . ,(all-the-icons-material "settings_input_component" :face 'all-the-icons-red))
    (Module        . ,(all-the-icons-material "view_module"              :face 'all-the-icons-red))
    (Property      . ,(all-the-icons-material "settings"                 :face 'all-the-icons-red))
    (Unit          . ,(all-the-icons-material "straighten"               :face 'all-the-icons-red))
    (Value         . ,(all-the-icons-material "filter_1"                 :face 'all-the-icons-red))
    (Enum          . ,(all-the-icons-material "plus_one"                 :face 'all-the-icons-red))
    (Keyword       . ,(all-the-icons-material "filter_center_focus"      :face 'all-the-icons-red))
    (Snippet       . ,(all-the-icons-material "short_text"               :face 'all-the-icons-red))
    (Color         . ,(all-the-icons-material "color_lens"               :face 'all-the-icons-red))
    (File          . ,(all-the-icons-material "insert_drive_file"        :face 'all-the-icons-red))
    (Reference     . ,(all-the-icons-material "collections_bookmark"     :face 'all-the-icons-red))
    (Folder        . ,(all-the-icons-material "folder"                   :face 'all-the-icons-red))
    (EnumMember    . ,(all-the-icons-material "people"                   :face 'all-the-icons-red))
    (Constant      . ,(all-the-icons-material "pause_circle_filled"      :face 'all-the-icons-red))
    (Struct        . ,(all-the-icons-material "streetview"               :face 'all-the-icons-red))
    (Event         . ,(all-the-icons-material "event"                    :face 'all-the-icons-red))
    (Operator      . ,(all-the-icons-material "control_point"            :face 'all-the-icons-red))
    (TypeParameter . ,(all-the-icons-material "class"                    :face 'all-the-icons-red))
    (Template      . ,(all-the-icons-material "short_text"               :face 'all-the-icons-green))))
  ;; Add a space after the icon
  (dolist (elt company-box-icons-all-the-icons)
    (setcdr elt (concat (cdr elt) " "))))

(provide 'emacs-completion)
;; emacs-completion.el ends here
