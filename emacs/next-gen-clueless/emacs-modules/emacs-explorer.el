;; emacs-explorer.el --- file-management config file -*- lexical-binding: t -*-
;;; Code:

;; (prot-emacs-package neotree
;;   (:install t)
;;   (:delay 5)
;;   (prot-emacs-keybind global-map "C-c C-t" #'neotree-toggle)
;;   (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
;;   (setq neo-smart-open t
;; 		neo-show-hidden-files t
;; 		neo-window-width 55
;; 		neo-window-fixed-size nil
;; 		inhibit-compacting-font-caches t
;; 		projectile-switch-project-action 'neotree-projectile-action)
;;   ;; truncate long file names in neotree
;;   (add-hook 'neo-after-create-hook
;; 			#'(lambda (_)
;; 				(with-current-buffer (get-buffer neo-buffer-name)
;; 				  (setq truncate-lines t)
;; 				  (setq word-wrap nil)
;; 				  (make-local-variable 'auto-hscroll-mode)
;; 				  (setq auto-hscroll-mode nil)))))

;; (use-package dired-single :ensure t)

(use-package emacs
  :ensure nil
    ;; (add-hook 'dired-mode-hook #'dired-hide-details-mode)
  :hook (dired-mode . hl-line-mode)
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches
		"-AGFhlv --group-directories-first --time-style=long-iso")
  (setq dired-dwim-target t)
  (setq dired-auto-revert-buffer #'dired-directory-changed-p) ; also see `dired-do-revert-buffer'
  (setq dired-make-directory-clickable t) ; Emacs 29.1
  (setq dired-free-space nil) ; Emacs 29.1
  (setq dired-mouse-drag-files t) ; Emacs 29.1
  (setq dired-guess-shell-alist-user ; those are the suggestions for ! and & in Dired
		'(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open")
		  ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open")
		  (".*" "xdg-open")))

  (setq dired-isearch-filenames 'dwim)
  (setq dired-create-destination-dirs 'ask) ; Emacs 27
  (setq dired-vc-rename-file t)             ; Emacs 27
  (setq dired-do-revert-buffer (lambda (dir) (not (file-remote-p dir)))) ; Emacs 28
  (setq dired-create-destination-dirs-on-trailing-dirsep t)

  (setq dired-clean-up-buffers-too t)
  (setq dired-clean-confirm-killing-deleted-buffers t)
  (setq dired-x-hands-off-my-keys t)    ; easier to show the keys I use
  (setq dired-bind-man nil)
  (setq dired-bind-info nil)
  (prot-emacs-keybind dired-mode-map "I" #'dired-info)

  (setq wdired-allow-to-change-permissions t)
  (setq wdired-create-parent-directories t))

(provide 'emacs-explorer)
;; emacs-explorer.el ends here
