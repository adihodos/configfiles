;; early-init.el --- Early Init File -*- lexical-binding: t -*-
;;; Code:

(setq frame-resize-pixelwise t
      frame-inhibit-implied-resize t
      use-dialog-box t ; only for mouse events, which I seldom use
      use-file-dialog nil
      inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-x-resources t
      inhibit-startup-echo-area-message user-login-name ; read the docstring
      inhibit-startup-buffer-menu t)

;; I do not use those graphical elements by default, but I do enable
;; them from time-to-time for testing purposes or to demonstrate
;; something.
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Temporarily increase the garbage collection threshold.  These
;; changes help shave off about half a second of startup time.  The
;; `most-positive-fixnum' is DANGEROUS AS A PERMANENT VALUE.  See the
;; `emacs-startup-hook' a few lines below for what I actually use.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

(setq gc-cons-threshold (* 128 1024 1024)
      read-process-output-max (* 1024 1024)
      make-backup-files nil
      create-lockfiles nil)

(add-hook 'emacs-startup-hook
		  #'(lambda ()
			  (message "Startup in %s seconds with %d garbage collection"
					   (emacs-init-time "%.2f")
					   gcs-done)))

(setq backup-directory-alist '((".*" . "~/.Trash")))
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file t)
