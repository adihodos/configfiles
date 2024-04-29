;; emacs-modeline.el --- modeline config file -*- lexical-binding: t -*-
;;; Code:

;;
;; Nice Flat Modeline
;; Https://www.youtube.com/watch?v=E1u6DcHis9M
(use-package emacs
  :ensure nil
  :config
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
  (advice-add 'load-theme :after #'flat-style))


;;
;; DOOM modeline
(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode 1)
  (setq doom-modeline-height 24      ;; sets modeline height
        doom-modeline-bar-width 5    ;; sets right bar width
        doom-modeline-persp-name t   ;; adds perspective name to modeline
        doom-modeline-persp-icon t
	doom-modeline-icon t
	doom-modeline-modal-modern-icon t)) ;; adds folder icon next to persp name

(provide 'emacs-modeline)
;; emacs-modeline.el ends here
