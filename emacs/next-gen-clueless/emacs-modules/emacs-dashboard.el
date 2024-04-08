;; emacs-dashboard.el --- Dashboard package config file -*- lexical-binding: t -*-
;;; Code:

(prot-emacs-package page-break-lines
  (:install t)
  ;; (:delay 5)
;;   :diminish (page-break-lines-mode visual-line-mode)
  (global-page-break-lines-mode 1))

(defun random-startup-banner()
	(let ((banner-list (directory-files "~/.emacs.d/banners" t directory-files-no-dot-files-regexp)))
		(nth (random (length banner-list)) banner-list)))
;;
;; DASHBOARD
(prot-emacs-package dashboard
  (:install t)
  ;; (:delay 5)
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-banner-logo-title "Emacs Is ... Everything ??!!!!")
    (setq dashboard-startup-banner (random-startup-banner))
  (setq dashboard-center-content t) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)
                          ;; (agenda . 5 )
                          (bookmarks . 3)
                          ;; (projects . 3)
                          (registers . 3)))
  (setq dashboard-display-icons-p t) ;; display icons on both GUI and terminal
  (setq dashboard-icon-type 'nerd-icons) ;; use `nerd-icons' package
  ;; (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (dashboard-modify-heading-icons '((recents . "file-text")
									(bookmarks . "book")))
  (dashboard-setup-startup-hook))

(provide 'emacs-dashboard)
;; emacs-dashboard.el ends here
