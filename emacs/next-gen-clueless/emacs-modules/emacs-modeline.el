;; emacs-modeline.el --- modeline config file -*- lexical-binding: t -*-
;;; Code:

;;
;; DOOM modeline
(prot-emacs-package doom-modeline
  (:install t)
  (:delay 1)
  (doom-modeline-mode 1)
  (setq doom-modeline-height 35      ;; sets modeline height
        doom-modeline-bar-width 5    ;; sets right bar width
        doom-modeline-persp-name t   ;; adds perspective name to modeline
        doom-modeline-persp-icon t
	    doom-modeline-icon t
	    doom-modeline-modal-modern-icon t)) ;; adds folder icon next to persp name

(provide 'emacs-modeline)
;; emacs-modeline.el ends here
