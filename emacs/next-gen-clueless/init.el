;; init.el --- Main Init File -*- lexical-binding: t -*-
;;; Code:

;; Taken from
;; https://github.com/protesilaos/dotfiles/blob/master/emacs/.emacs.d/init.el

(defgroup prot-emacs nil
  "User options for my dotemacs.
These produce the expected results only when set in a file called
prot-emacs-pre-custom.el.  This file must be in the same
directory as the init.el."
  :group 'file)

(defcustom prot-emacs-omit-packages nil
  "List of package names to not load.
This instructs the relevant macros to not `require' the given
package.  In the case of `prot-emacs-elpa-package', the package
will not be installed if it is not already available on the
system.

This user option must be set in the `prot-emacs-pre-custom.el'
file.  If that file exists in the Emacs directory, it is loaded
before all other modules of my setup."
  :group 'prot-emacs
  :type '(repeat symbol))

(setq make-backup-files nil)
(setq backup-inhibited nil) ; Not sure if needed, given `make-backup-files'
(setq create-lockfiles nil)

;; Make native compilation silent and prune its cache.
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent) ; Emacs 28 with native compilation
  (setq native-compile-prune-cache t)) ; Emacs 29

;; Disable the damn thing by making it disposable.
(setq custom-file (make-temp-file "emacs-custom-"))

(mapc
 (lambda (string)
   (add-to-list 'load-path (locate-user-emacs-file string)))
 '("emacs-lisp" "emacs-modules"))

;;;; Packages

(require 'package)

(setq package-vc-register-as-project nil) ; Emacs 30

(add-hook 'package-menu-mode-hook #'hl-line-mode)

;; Also read: <https://protesilaos.com/codelog/2022-05-13-emacs-elpa-devel/>
(setq package-archives
      '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
        ("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;; Highest number gets priority (what is not mentioned has priority 0)
(setq package-archive-priorities
      '(("gnu-elpa" . 3)
        ("melpa" . 2)
        ("nongnu" . 1)))

(setq custom-safe-themes t)

(defmacro prot-emacs-configure (&rest body)
  "Evaluate BODY as a `progn'.
BODY consists of ordinary Lisp expressions.  The sole exception
is an unquoted plist of the form (:delay NUMBER) which evaluates
BODY with NUMBER seconds of `run-with-timer'.

Note that `prot-emacs-configure' does not try to autoload
anything.  Use it only for forms that evaluate regardless.

Also see `prot-emacs-package'."
  (declare (indent 0))
  (let (delay)
    (dolist (element body)
      (when (plistp element)
        (pcase (car element)
          (:delay (setq delay (cadr element)
                        body (delq element body))))))
    (if delay
        `(run-with-timer ,delay nil (lambda () ,@body))
      `(progn ,@body))))

(defun prot-emacs-package-install (package &optional method)
  "Install PACKAGE with optional METHOD.

If METHOD is nil or the `builtin' symbol, PACKAGE is not
installed as it is considered part of Emacs.

If METHOD is a string, it must be a URL pointing to the version
controlled repository of PACKAGE.  Installation is done with
`package-vc-install'.

If METHOD is a quoted list, it must have a form accepted by
`package-vc-install' such as:

\\='(denote :url \"https://github.com/protesilaos/denote\" :branch \"main\")

If METHOD is any other non-nil value, install PACKAGE using
`package-install'."
  (unless (or (eq method 'builtin) (null method))
    (unless (package-installed-p package)
      (when (or (stringp method) (listp method))
        (package-vc-install method))
      (unless package-archive-contents
        (package-refresh-contents))
      (package-install package))))

(defvar prot-emacs-loaded-packages nil)

(defmacro prot-emacs-package (package &rest body)
  "Require PACKAGE with BODY configurations.

PACKAGE is an unquoted symbol that is passed to `require'.  It
thus conforms with `featurep'.

BODY consists of ordinary Lisp expressions.  There are,
nevertheless, two unquoted plists that are treated specially:

1. (:install METHOD)
2. (:delay NUMBER)

These plists can be anywhere in BODY and are not part of its
final expansion.

The :install property is the argument passed to
`prot-emacs-package-install' and has the meaning of METHOD
described therein.

The :delay property makes the evaluation of PACKAGE with the
expanded BODY happen with `run-with-timer'.

Also see `prot-emacs-configure'."
  (declare (indent defun))
  (unless (memq package prot-emacs-omit-packages)
    (let (install delay)
      (dolist (element body)
        (when (plistp element)
          (pcase (car element)
            (:install (setq install (cdr element)
                            body (delq element body)))
            (:delay (setq delay (cadr element)
                          body (delq element body))))))
      (let ((common `(,(when install
                         `(prot-emacs-package-install ',package ,@install))
                      (require ',package)
                      (add-to-list 'prot-emacs-loaded-packages ',package)
                      ,@body
                      ;; (message "Prot Emacs loaded package: %s" ',package)
                      )))
        (cond
         ((featurep package)
          `(progn ,@body))
         (delay
          `(run-with-timer ,delay nil (lambda () ,@(delq nil common))))
         (t
          `(progn ,@(delq nil common))))))))


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

(defvar adi/cfg-root-directory
  (file-name-directory (or load-file-name (buffer-file-name)))
  "Directory where this config is loaded from")

(require 'emacs-essentials)
(require 'emacs-win32)
(require 'emacs-themes)
(require 'emacs-icons)
(require 'emacs-dashboard)
(require 'emacs-modeline)
(require 'emacs-which-key)
(require 'emacs-window)
(require 'emacs-completion)
(require 'emacs-langs)
(require 'emacs-lsp)
(require 'emacs-search)
(require 'emacs-explorer)

;; ;; A few more useful configurations...
;; (use-package emacs
;;   :init
;;   ;; Add prompt indicator to `completing-read-multiple'.
;;   ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
;;   (defun crm-indicator (args)
;;     (cons (format "[CRM%s] %s"
;;                   (replace-regexp-in-string
;;                    "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
;;                    crm-separator)
;;                   (car args))
;;           (cdr args)))
;;   (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;;   ;; Do not allow the cursor in the minibuffer prompt
;;   (setq minibuffer-prompt-properties
;;         '(read-only t cursor-intangible t face minibuffer-prompt))
;;   (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;   ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
;;   ;; Vertico commands are hidden in normal buffers.
;;   ;; (setq read-extended-command-predicate
;;   ;;       #'command-completion-default-include-p)

;;   ;; Enable recursive minibuffers
;;   (setq enable-recursive-minibuffers t))



  
;; (setq-default fill-column 80)
;; (setq column-number-mode t)
;; (setq tab-width 4)
;; (defalias 'yes-or-no-p 'y-or-n-p)

;; (recentf-mode 1)
;; (setq recentf-max-saved-items 100
;;       inhibit-startup-message t
;;       ring-bell-function 'ignore)

;; ;; (setq scroll-step 1
;; ;;       scroll-conservatively 10000
;; ;;       mouse-wheel-progressive-speed 1
;; ;;       mouse-wheel-scroll-amount '(1 ((shift) . 1)))

;; ;; (defvar adi/default-font "Iosevka Nerd Font" "Font to use")
;; ;; (defvar adi/default-font "Input" "Font to use")
;; ;; (defvar adi/default-font "JetBrainsMono Nerd Font" "Font to use")
;; (global-set-key (kbd "C-x k") 'adi/kill-buffer-current)

;; ;; (add-to-list 'load-path #'locate-user-emacs-file)
;; (require 'debugging)
;; (require 'sane-defaults)
;; (require 'essentials)
;; (require 'emacs-window)
;; (require 'emacs-langs)
;; ;; (require 'lsp-bridge-cfg)
;; (require 'lsp)
;; (provide 'init)

;; ;;; init.el ends here
