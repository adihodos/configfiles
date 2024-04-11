;;; Essential configurations
;;; Adapted from https://github.com/protesilaos

(prot-emacs-configure
  (:delay 1)
  (require 'prot-common)
  (require 'prot-simple)
  (require 'prot-pair)
  (require 'prot-comment)

  (setq user-full-name "Adrian Hodos")
  (setq user-mail-address "adi.hodos@gmail.com")

;;;; General settings and common custom functions
  (setq blink-matching-paren nil)
  (setq delete-pair-blink-delay 0.1) ; Emacs28 -- see `prot-simple-delete-pair-dwim'
  (setq help-window-select t)
  (setq next-error-recenter '(4)) ; center of the window
  (setq find-library-include-other-files nil) ; Emacs 29
  (setq remote-file-name-inhibit-delete-by-moving-to-trash t) ; Emacs 30
  (setq remote-file-name-inhibit-auto-save t)		      ; Emacs 30
  (setq save-interprogram-paste-before-kill t)
  (setq mode-require-final-newline 'visit-save)
  (setq-default truncate-partial-width-windows nil)
  (setq eval-expression-print-length nil)
  (setq kill-do-not-save-duplicates t)
  (setq duplicate-line-final-position -1 ; both are Emacs 29
	duplicate-region-final-position -1)
  (setq scroll-error-top-bottom t)

  (setq prot-simple-date-specifier "%F")
  (setq prot-simple-time-specifier "%R %z")

  (setq prot-scratch-default-mode 'text-mode)

;;;; Comments (prot-comment.el)
  (setq comment-empty-lines t)
  (setq comment-fill-column nil)
  (setq comment-multi-line t)
  (setq comment-style 'multi-line)
  (setq-default comment-column 0)

  (setq prot-comment-comment-keywords '("TODO" "NOTE" "XXX" "REVIEW" "FIXME"))
  (setq prot-comment-timestamp-format-concise "%F")
  (setq prot-comment-timestamp-format-verbose "%F %T %z")

;;;; Mouse and mouse wheel behaviour
  (setq mouse-autoselect-window t) ; complements the auto-selection of my tiling window manager

  ;; In Emacs 27+, use Control + mouse wheel to scale text.
  (setq mouse-wheel-scroll-amount
	'(1
	  ((shift) . 5)
	  ((meta) . 0.5)
	  ((control) . text-scale))
	mouse-drag-copy-region nil
	make-pointer-invisible t
	mouse-wheel-progressive-speed t
	mouse-wheel-follow-mouse t)

  ;; Scrolling behaviour
  (setq-default scroll-preserve-screen-position t
		scroll-conservatively 1 ; affects `scroll-step'
		scroll-margin 0
		next-screen-context-lines 0)

  (mouse-wheel-mode 1)
  (define-key global-map (kbd "C-M-<mouse-3>") #'tear-off-window)

;;;; Repeatable key chords (repeat-mode)
  (setq repeat-on-final-keystroke t
	repeat-exit-timeout 5
	repeat-exit-key "<escape>"
	repeat-keep-prefix nil
	repeat-check-key t
	repeat-echo-function 'ignore
	;; Technically, this is not in repeal.el, though it is the
	;; same idea.
	set-mark-command-repeat-pop t)
  (repeat-mode 1)

;;;; Built-in bookmarking framework (bookmark.el)
  (setq bookmark-use-annotations nil)
  (setq bookmark-automatically-show-annotations nil)
  (setq bookmark-fringe-mark nil) ; Emacs 29 to hide bookmark fringe icon
  ;; Write changes to the bookmark file as soon as 1 modification is
  ;; made (addition or deletion).  Otherwise Emacs will only save the
  ;; bookmarks when it closes, which may never happen properly
  ;; (e.g. power failure).
  (setq bookmark-save-flag 1)

  (add-hook 'bookmark-bmenu-mode-hook #'hl-line-mode)

;;;; Auto revert mode
  (setq auto-revert-verbose t)
  (global-auto-revert-mode 1)

;;;; Delete selection
  (delete-selection-mode 1)

;;;; Tooltips (tooltip-mode)
  (setq tooltip-delay 0.5
	tooltip-short-delay 0.5
	x-gtk-use-system-tooltips nil
	tooltip-frame-parameters
	'((name . "tooltip")
	  (internal-border-width . 10)
	  (border-width . 0)
	  (no-special-glyphs . t)))

  (autoload #'tooltip-mode "tooltip")
  (tooltip-mode 1)

;;;; Display current time
  (setq display-time-format " %a %e %b, %H:%M ")
  ;;;; Covered by `display-time-format'
  ;; (setq display-time-24hr-format t)
  ;; (setq display-time-day-and-date t)
  (setq display-time-interval 60)
  (setq display-time-default-load-average nil)
  ;; NOTE 2022-09-21: For all those, I have implemented my own solution
  ;; that also shows the number of new items, although it depends on
  ;; notmuch: the `notmuch-indicator' package.
  (setq display-time-mail-directory nil)
  (setq display-time-mail-function nil)
  (setq display-time-use-mail-icon nil)
  (setq display-time-mail-string nil)
  (setq display-time-mail-face nil)

  ;; I don't need the load average and the mail indicator, so let this
  ;; be simple:
  (setq display-time-string-forms
	'((propertize
	   (format-time-string display-time-format now)
	   'face 'display-time-date-and-time
	   'help-echo (format-time-string "%a %b %e, %Y" now))
	  " "))

  (display-time-mode 1)

;;;; World clock (M-x world-clock)
  (setq display-time-world-list t)
  (setq zoneinfo-style-world-list ; M-x shell RET timedatectl list-timezones
	'(("America/Los_Angeles" "Los Angeles")
	  ("America/Vancouver" "Vancouver")
	  ("Canada/Pacific" "Canada/Pacific")
	  ("America/Chicago" "Chicago")
	  ("Brazil/Acre" "Rio Branco")
	  ("America/New_York" "New York")
	  ("Canada/Atlantic" "Canada/Atlantic")
	  ("Brazil/East" "Brasília")
	  ("UTC" "UTC")
	  ("Europe/Lisbon" "Lisbon")
	  ("Europe/Brussels" "Brussels")
	  ("Europe/Athens" "Athens")
	  ("Asia/Riyadh" "Riyadh")
	  ("Asia/Tehran" "Tehran")
	  ("Asia/Tbilisi" "Tbilisi")
	  ("Asia/Yekaterinburg" "Yekaterinburg")
	  ("Asia/Kolkata" "Kolkata")
	  ("Asia/Singapore" "Singapore")
	  ("Asia/Shanghai" "Shanghai")
	  ("Asia/Seoul" "Seoul")
	  ("Asia/Tokyo" "Tokyo")
	  ("Asia/Vladivostok" "Vladivostok")
	  ("Australia/Brisbane" "Brisbane")
	  ("Australia/Sydney" "Sydney")
	  ("Pacific/Auckland" "Auckland")))

  ;; All of the following variables are for Emacs 28
  (setq world-clock-list t)
  (setq world-clock-time-format "%R %z (%Z)	%A %d %B")
  (setq world-clock-buffer-name "*world-clock*") ; Placement handled by `display-buffer-alist'
  (setq world-clock-timer-enable t)
  (setq world-clock-timer-second 60)

;;;; `man' (manpages)
  (setq Man-notify-method 'pushy) ; does not obey `display-buffer-alist'

;;;; `proced' (process monitor, similar to `top')
  (setq proced-auto-update-flag t)
  (setq proced-enable-color-flag t) ; Emacs 29
  (setq proced-auto-update-interval 5)
  (setq proced-descend t)
  (setq proced-filter 'user)

;;;; Emacs server (allow emacsclient to connect to running session)
  ;; The "server" is functionally like the daemon, except it is run by
  ;; the first Emacs frame we launch.  When we close that frame, the
  ;; server is terminated.  Whereas the daemon remains active even if
  ;; all Emacs frames are closed.
  ;;
  ;; I experimented with the daemon for a while.  Emacs would crash
  ;; whenever I would encounter an error in some Lisp evaluation.
  ;; Whereas the server works just fine when I need to connect to it via
  ;; the emacsclient.
  (require 'server)
  (setq server-client-instructions nil)
  (unless (server-running-p)
    (server-start)))

;; General commands
(prot-emacs-keybind global-map
  "<insert>" nil
  "C-x C-z" nil
  "C-x C-c" nil ; avoid accidentally exiting Emacs
  "C-x C-c C-c" #'save-buffers-kill-emacs
  "C-h h" nil
  "M-`" nil
  ;; "C-z" prot-prefix-map
  ;; "<f2>" prot-prefix-map ; override that two-column gimmick
  "ESC ESC" #'prot-simple-keyboard-quit-dwim
  "C-g" #'prot-simple-keyboard-quit-dwim
  "C-h K" #'describe-keymap ; overrides `Info-goto-emacs-key-command-node'
  "C-h u" #'apropos-user-option
  "C-h F" #'apropos-function ; lower case is `describe-function'
  "C-h V" #'apropos-variable ; lower case is `describe-variable'
  "C-h L" #'apropos-library ; lower case is `view-lossage'
  "C-h c" #'describe-char ; overrides `describe-key-briefly'
  "C-M-SPC" #'prot-simple-mark-sexp   ; will be overriden by `expreg' if tree-sitter is available
  ;; Commands for lines
  "M-o" #'delete-blank-lines   ; alias for C-x C-o
  "M-k" #'prot-simple-kill-line-backward
  "C-S-d" #'prot-simple-duplicate-line-or-region
  "C-S-w" #'prot-simple-copy-line
  "C-S-y" #'prot-simple-yank-replace-line-or-region
  "M-SPC" #'cycle-spacing
  "C-v" #'prot-simple-multi-line-below ; overrides `scroll-up-command'
  "M-v" #'prot-simple-multi-line-above ; overrides `scroll-down-command'
  "<C-return>" #'prot-simple-new-line-below
  "<C-S-return>" #'prot-simple-new-line-above
  ;; Commands for text insertion or manipulation
  "C-=" #'prot-simple-insert-date
  "C-<" #'prot-simple-escape-url-dwim
  ;; "C->" #'prot-simple-insert-line-prefix-dwim
  "C-'" #'prot-pair-insert
  "M-'" #'prot-pair-insert
  "M-\\" #'prot-pair-delete
  "M-z" #'zap-up-to-char ; NOT `zap-to-char'
  "M-Z" #'prot-simple-zap-to-char-backward
  "<C-M-backspace>" #'backward-kill-sexp
  "M-c" #'capitalize-dwim
  "M-l" #'downcase-dwim        ; "lower" case
  "M-u" #'upcase-dwim
  ;; Commands for object transposition
  "C-S-p" #'prot-simple-move-above-dwim
  "C-S-n" #'prot-simple-move-below-dwim
  "C-t" #'prot-simple-transpose-chars
  "C-x C-t" #'prot-simple-transpose-lines
  "C-S-t" #'prot-simple-transpose-paragraphs
  "C-x M-t" #'prot-simple-transpose-sentences
  "C-M-t" #'prot-simple-transpose-sexps
  "M-t" #'prot-simple-transpose-words
  ;; Commands for paragraphs
  "M-Q" #'prot-simple-unfill-region-or-paragraph
  ;; Commands for windows and pages
  "C-x O" #'next-multiframe-window
  "C-x n k" #'prot-simple-delete-page-delimiters
  "C-x M-r" #'prot-simple-swap-window-buffers
  ;; Commands for buffers
  "M-=" #'count-words
  "<C-f2>" #'prot-simple-rename-file-and-buffer
  "C-x k" #'prot-simple-kill-buffer-current
  "C-x K" #'kill-buffer
  "M-s b" #'prot-simple-buffers-major-mode
  "M-s v" #'prot-simple-buffers-vc-root
  ;; Scratch buffer for major mode of choice
  "C-c s" #'prot-scratch-buffer
  ;; Comments
  "C-;" #'prot-comment
  "C-x C-;" #'prot-comment-timestamp-keyword)

;;; Visualise undo ring (`vundo')
(prot-emacs-package vundo
  (:install t)
  (:delay 1)
  (setq vundo-glyph-alist vundo-unicode-symbols)

  (defvar prot/vundo-undo-functions '(undo undo-only undo-redo)
    "List of undo functions to check if we need to visualise the undo ring.")

  (defvar prot/vundo-undo-command #'undo
    "Command to call if we are not going to visualise the undo ring.")

  (defun prot/vundo-if-repeat-undo (&rest args)
    "Use `vundo' if the last command is among `prot/vundo-undo-functions'.
In other words, start visualising the undo ring if we are going
to be cycling through the edits."
    (interactive)
    (if (and (member last-command prot/vundo-undo-functions)
	     (not undo-in-region))
	(call-interactively 'vundo)
      (apply args)))

  (mapc
   (lambda (fn)
     (advice-add fn :around #'prot/vundo-if-repeat-undo))
   prot/vundo-undo-functions)

  (prot-emacs-keybind vundo-mode-map
    "C-/" #'vundo-backward
    "C-?" #'vundo-forward
    "u" #'vundo-backward
    "U" #'vundo-forward
    "g" #'vundo-goto-last-saved
    "." #'vundo-goto-last-saved
    "h" #'vundo-backward
    "j" #'vundo-next
    "k" #'vundo-previous
    "l" #'vundo-forward)

  (with-eval-after-load 'pulsar
    (add-hook 'vundo-post-exit-hook #'pulsar-pulse-line-green)))

;;; Shell (M-x shell)
(when (featurep 'shell)

  ;; Check my .bashrc which handles `comint-terminfo-terminal':
  ;;
  ;; # Default pager.  The check for the terminal is useful for Emacs with
  ;; # M-x shell (which is how I usually interact with bash these days).
  ;; #
  ;; # The COLORTERM is documented in (info "(emacs) General Variables").
  ;; # I found the reference to `dumb-emacs-ansi' in (info "(emacs)
  ;; # Connection Variables").
  ;; if [ "$TERM" = "dumb" ] && [ "$INSIDE_EMACS" ]
  ;; then
  ;;	 export PAGER="cat"
  ;;	 alias less="cat"
  ;;	 export TERM=dumb-emacs-ansi
  ;;	 export COLORTERM=1
  ;; else
  ;;	 # Quit once you try to scroll past the end of the file.
  ;;	 export PAGER="less --quit-at-eof"
  ;; fi

  (setq shell-command-prompt-show-cwd t) ; Emacs 27.1
  (setq ansi-color-for-comint-mode t)
  (setq shell-input-autoexpand 'input)
  (setq shell-highlight-undef-enable t) ; Emacs 29.1
  (setq shell-has-auto-cd nil) ; Emacs 29.1
  (setq shell-get-old-input-include-continuation-lines t) ; Emacs 30.1
  (setq shell-kill-buffer-on-exit t) ; Emacs 29.1
  (setq shell-completion-fignore '("~" "#" "%"))
  (setq-default comint-scroll-to-bottom-on-input t)
  (setq-default comint-scroll-to-bottom-on-output nil)
  (setq-default comint-input-autoexpand 'input)
  (setq comint-prompt-read-only t)
  (setq comint-buffer-maximum-size 9999)
  (setq comint-completion-autolist t)
  (setq comint-input-ignoredups t)
  (setq tramp-default-remote-shell "/bin/bash")

  (setq shell-font-lock-keywords
	'(("[ \t]\\([+-][^ \t\n]+\\)" 1 font-lock-builtin-face)
	  ("^[^ \t\n]+:.*" . font-lock-string-face)
	  ("^\\[[1-9][0-9]*\\]" . font-lock-constant-face)))

  ;; Support for OS-specific escape sequences such as what `ls
  ;; --hyperlink' uses.	 I normally don't use those, but I am checking
  ;; this to see if there are any obvious advantages/disadvantages.
  (add-hook 'comint-output-filter-functions 'comint-osc-process-output)

  (define-key global-map (kbd "<f1>") #'shell) ; I don't use F1 for help commands

  (prot-emacs-keybind shell-mode-map
    "C-c C-k" #'comint-clear-buffer
    "C-c C-w" #'comint-write-output))

(prot-emacs-package multiple-cursors  
  (:install t)
  (:delay 5)
  (prot-emacs-keybind global-map
	"C-S-c C-S-c" #'mc/edit-lines
	"C->" #'mc/mark-next-like-this
	"C-<" #'mc/mark-previous-like-this
	"M-<f2>" #'mc/mark-next-like-this-word
	"M-<f3>" #'mc/mark-all-like-this
	"M-<f4>" #'mc/mark-all-like-this-dwim))

(prot-emacs-package move-text
  (:install t)
  (:delay 5)
  (move-text-default-bindings))

(prot-emacs-package gitignore-templates
  (:install t)
  (:delay 5))

(when (not (eq system-type 'windows-nt))
  (prot-emacs-package direnv
	(:install t)
	(direnv-mode 1)
	(setq direnv-always-show-summary t)))

(prot-emacs-package p4
  (:install t)
  (:delay 8))

(provide 'emacs-essentials)
;; emacs-essentials.el ends here
