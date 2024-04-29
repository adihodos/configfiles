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

(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))
(setq backup-bycopying t)
(setq version-control nil)
(setq delete-old-versions t)
(setq make-backup-files nil)
(setq backup-inhibited nil) ; Not sure if needed, given `make-backup-files'
(setq create-lockfiles nil)
;; Make native compilation silent and prune its cache.
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent) ; Emacs 28 with native compilation
  (setq native-compile-prune-cache t)) ; Emacs 29

;; Disable the damn thing by making it disposable.
(setq custom-file (make-temp-file "emacs-custom-"))

;; Enable these
(mapc
 (lambda (command)
   (put command 'disabled nil))
 '(list-timers narrow-to-region narrow-to-page upcase-region downcase-region))

;; And disable these
(mapc
 (lambda (command)
   (put command 'disabled t))
 '(eshell project-eshell overwrite-mode iconify-frame diary))


(mapc
 (lambda (string)
   (add-to-list 'load-path (locate-user-emacs-file string)))
 '("emacs-lisp" "emacs-modules"))

;;;; Packages

(require 'package)

(setq package-vc-register-as-project nil) ; Emacs 30
(add-hook 'package-menu-mode-hook #'hl-line-mode)

(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

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

(require 'emacs-themes)
(require 'emacs-dashboard)
(require 'emacs-essentials)
(require 'emacs-completion)
(require 'emacs-vertico)
(require 'emacs-icons)
(require 'emacs-lsp)
;; (require 'emacs-peek)

(require 'emacs-modeline)
(require 'emacs-which-key)
(require 'emacs-window)
(require 'emacs-langs)
(require 'emacs-search)
(require 'emacs-explorer)

;;; init.el ends here
