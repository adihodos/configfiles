
;; (dolist (path '("lsp-config-modules"))
;;   (add-to-list 'load-path (locate-user-emacs-file path)))

(defvar adi/lsp-bridge-lsp-config-dir (concat adi/cfg-root-directory "emacs-modules/lsp-server-config"))
;;
;; LSP bridge
(use-package lsp-bridge
  :straight '(lsp-bridge :type git :host github :repo "manateelazycat/lsp-bridge"
			 :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
			 :build (:not compile))
  :init
  (global-lsp-bridge-mode)
  :config
  (global-set-key (kbd "M-.") #'lsp-bridge-find-def)
  (global-set-key (kbd "M-,") #'lsp-bridge-find-def-return)
  (global-set-key (kbd "M-?") #'lsp-bridge-find-references)
  (global-set-key (kbd "C-c C-h") #'lsp-bridge-show-documentation)
  (global-set-key (kbd "C-c l r r") #'lsp-bridge-rename)
  (global-set-key (kbd "C-c l a a") #'lsp-bridge-code-action)
  (global-set-key (kbd "C-<tab>") #'lsp-bridge-popup-complete-menu)
  (global-set-key (kbd "C-c l = =") #'lsp-bridge-code-format)
  (global-set-key (kbd "C-c l d") #'lsp-bridge-diagnostic-list)
  (setq lsp-bridge-user-langserver-dir adi/lsp-bridge-lsp-config-dir)
  ;; (setq lsp-bridge-enable-inlay-hint nil)
  )

(provide 'lsp-bridge-cfg)

