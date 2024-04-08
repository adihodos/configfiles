;; emacs-langs.el --- langs config file -*- lexical-binding: t -*-
;;; Code:

;; Ripgrep
;; https://www.youtube.com/watch?v=4qLD4oHOrlc
(prot-emacs-package rg
  (:install t)
  (:delay 10)
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
    :dir (let ((vc (vc-root-dir)))
		   (if vc
			   vc
			 default-directory))
    :confirm prefix
    :flags ("--hidden -g !.git"))

    (prot-emacs-keybind global-map
        "M-s g" #'adi/grep-project-or-current-dir)
    (prot-emacs-keybind rg-mode-map
		 "C-n" #'next-line
		 "C-p" #'previous-line
		 "M-n" #'rg-next-file
		 "M-p" #'rg-prev-file))

(provide 'emacs-search)
;; emacs-search.el ends here
