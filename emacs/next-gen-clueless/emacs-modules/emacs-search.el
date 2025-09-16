;; emacs-search.el --- search config file -*- lexical-binding: t -*-
;;; Code:

;;; Isearch, occur, grep, and extras (prot-search.el)
(use-package isearch
  :ensure nil
  :demand t
  :config
  (setq search-whitespace-regexp ".*?" ; one `setq' here to make it obvious they are a bundle
        isearch-lax-whitespace t
        isearch-regexp-lax-whitespace nil))

(use-package isearch
  :ensure nil
  :demand t
  :config
  (setq search-highlight t)
  (setq isearch-lazy-highlight t)
  (setq lazy-highlight-initial-delay 0.5)
  (setq lazy-highlight-no-delay-length 4))

(use-package isearch
  :ensure nil
  :demand t
  :config
  (setq isearch-lazy-count t)
  (setq lazy-count-prefix-format "(%s/%s) ")
  (setq lazy-count-suffix-format nil))

(use-package isearch
  :ensure nil
  :demand t
  :config
  (setq isearch-wrap-pause t) ; `no-ding' makes keyboard macros never quit
  (setq isearch-repeat-on-direction-change t))

(use-package isearch
  :ensure nil
  :demand t
  :config
  (setq list-matching-lines-jump-to-current-line nil) ; do not jump to current line in `*occur*' buffers
  (add-hook 'occur-mode-hook #'prot-common-truncate-lines-silently) ; from `prot-common.el'
  (add-hook 'occur-mode-hook #'hl-line-mode))

(use-package isearch
  :ensure nil
  :demand t
  :bind
  ( :map minibuffer-local-isearch-map
    ("M-/" . isearch-complete-edit)
    :map occur-mode-map
    ("t" . toggle-truncate-lines)
    :map isearch-mode-map
    ("C-g" . isearch-cancel) ; instead of `isearch-abort'
    ("M-/" . isearch-complete)))

(use-package prot-search
  :after rg
  :ensure nil
  :bind
  ( :map global-map
    ("M-s M-%" . prot-search-replace-markup) ; see `prot-search-markup-replacements'
    ("M-s M-<" . prot-search-isearch-beginning-of-buffer)
    ("M-s M->" . prot-search-isearch-end-of-buffer)
    ("M-s G" . prot-search-grep)
	("M-s g" . adi/grep-project-or-current-dir)
    ("M-s u" . prot-search-occur-urls)
    ("M-s t" . prot-search-occur-todo-keywords)
    ("M-s M-t" . prot-search-grep-todo-keywords) ; With C-u it runs `prot-search-git-grep-todo-keywords'
    ("M-s M-T" . prot-search-git-grep-todo-keywords)
    ("M-s s" . prot-search-outline)
    ("M-s M-o" . prot-search-occur-outline)
    ("M-s M-u" . prot-search-occur-browse-url)
    :map isearch-mode-map
    ("<up>" . prot-search-isearch-repeat-backward)
    ("<down>" . prot-search-isearch-repeat-forward)
    ("<backspace>" . prot-search-isearch-abort-dwim)
    ("<C-return>" . prot-search-isearch-other-end))
  :config
  (setq prot-search-outline-regexp-alist
        '((emacs-lisp-mode . "^\\((\\|;;;+ \\)")
          (org-mode . "^\\(\\*+ +\\|#\\+[Tt][Ii][Tt][Ll][Ee]:\\)")
          (outline-mode . "^\\*+ +")
          (emacs-news-view-mode . "^\\*+ +")
          (conf-toml-mode . "^\\[")
          (markdown-mode . "^#+ +")))
  (setq prot-search-todo-keywords
        (concat "TODO\\|FIXME\\|NOTE\\|REVIEW\\|XXX\\|KLUDGE"
                "\\|HACK\\|WARN\\|WARNING\\|DEPRECATED\\|BUG"))

  (with-eval-after-load 'pulsar
    (add-hook 'prot-search-outline-hook #'pulsar-recenter-center)
    (add-hook 'prot-search-outline-hook #'pulsar-reveal-entry)))

;;; grep and xref
(use-package re-builder
  :ensure nil
  :commands (re-builder regexp-builder)
  :config
  (setq reb-re-syntax 'read))

(use-package xref
  :ensure nil
  :commands (xref-find-definitions xref-go-back)
  :config
  ;; All those have been changed for Emacs 28
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read) ; for M-.
  (setq xref-show-xrefs-function #'xref-show-definitions-buffer) ; for grep and the like
  (setq xref-file-name-display 'project-relative))

;; (use-package grep
;;   :ensure nil
;;   :commands (grep lgrep rgrep)
;;   :config
;;   (setq grep-save-buffers nil)
;;   (setq grep-use-headings t) ; Emacs 30
;; 
;;   (let ((executable (or (executable-find "rg") "grep"))
;;         (rgp (string-match-p "rg" grep-program)))
;;     (setq grep-program executable)
;;     (setq grep-template
;;           (if rgp
;;               "rg -nH --null -e <R> <F>"
;;             "grep <X> <C> -nH --null -e <R> <F>"))
;;     (setq xref-search-program (if rgp 'ripgrep 'grep))))

;;; wgrep (writable grep)
(use-package wgrep
  :ensure t
  :after grep
  :bind
  ( :map grep-mode-map
    ("e" . wgrep-change-to-wgrep-mode)
    ("C-x C-q" . wgrep-change-to-wgrep-mode)
    ("C-c C-c" . wgrep-finish-edit))
  :config
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t))

(provide 'prot-emacs-search)

;;
;; Embark
(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-M-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  ;; Add Embark to the mouse context menu. Also enable `context-menu-mode'.
  ;; (context-menu-mode 1)
  ;; (add-hook 'context-menu-functions #'embark-context-menu 100)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Ripgrep
;; https://www.youtube.com/watch?v=4qLD4oHOrlc
(use-package rg
  :ensure t
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
    ;; :dir (let ((vc (vc-root-dir)))
	;; 	   (if vc
	;; 		   vc
	;; 		 default-directory))
	;; :dir current
    :confirm prefix
    :flags ("--hidden -g !.git"))

    :bind (:map rg-mode-map
		 ("C-n" . next-line)
		 ("C-p" . previous-line)
		 ("M-n" . rg-next-file)
		 ("M-p" . rg-prev-file)))

(provide 'emacs-search)
;; emacs-search.el ends here
