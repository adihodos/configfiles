;; emacs-sys.el --- win32 config file -*- lexical-binding: t -*-
;;; Code:

(defvar adi/default-font "Iosevka NFM")
(defvar adi/font-variable-pitch "Iosevka NFP")

(when (eq system-type 'windows-nt)
  (prot-emacs-configure
	(setq w32-pass-lwindow-to-system nil)
	(setq w32-lwindow-modifier 'super) ; Left Windows key
	(w32-register-hot-key [s-]) ; disable all Windows shortcuts while Emacs has focus
	(setq w32-pass-rwindow-to-system nil)
	(setq w32-rwindow-modifier 'super) ; Right Windows key
	(setq w32-apps-modifier 'hyper) ; Menu/App key
	
	))

(when (not (eq system-type 'windows-nt)))


(provide 'emacs-sys)

;; emacs-sys.el ends here
