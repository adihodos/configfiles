;; (defvar adi/lldb-vscode-path (executable-find "lldb-vscode") "Path to the lldb-vscode executable")

(use-package treemacs
  :commands (treemacs
             treemacs-follow-mode
             treemacs-filewatch-mode
             treemacs-fringe-indicator-mode)
  :bind (("<f8>" . treemacs)
         ("<f9>" . treemacs-select-window))
  :init
  (when window-system
    (setq treemacs-width-is-initially-locked nil)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t))) 

(use-package dap-mode
  :commands (dap-debug)
  :init
  (setq lsp-enable-dap-auto-configure nil)
  ;; (setq dap-auto-configure-mode nil)
;;   :bind
;;   (:map dap-mode-map
;; 	("<f5>" . dap-debug)
;; 	("<f10>" . dap-next)
;; 	("<f11>" . dap-step-in)
;; 	("<S-f11>" . dap-step-out)
;; 	("S-<f5>" . dap-continue)
;; 	("<f9>" . dap-breakpoint-toggle))
  :config
;;   (setq dap-ui-variable-length 100)
;;   (setq dap-auto-show-output nil)
  (require 'dap-codelldb)
  (dap-mode 1)
;;   (dap-ui-mode 1)
;;   (dap-tooltip-mode 1)
;;   (dap-ui-controls-mode 1)
  ;;   (tooltip-mode 1)
  (setq dap-auto-show-output nil)
  (setq dap-default-terminal-kind "integrated")
  (setq dap-codelldb-debug-program (executable-find "codelldb")))
;; 
;; (with-eval-after-load 'dap-ui
;;   (setq dap-ui-buffer-configurations
;; 	`((,dap-ui--locals-buffer . ((side . right) (slot . 1) (window-width . 0.20)))
;; 	  (,dap-ui--expressions-buffer . ((side . right) (slot . 2) (window-width . 0.20)))
;; 	  (,dap-ui--sessions-buffer . ((side . right) (slot . 3) (window-width . 0.20)))
;; 	  (,dap-ui--breakpoints-buffer . ((side . left) (slot . 2) (window-width . ,treemacs-width)))
;; 	  (,dap-ui--debug-window-buffer . ((side . bottom) (slot . 3) (window-width . 0.20)))
;; 	  (,dap-ui--repl-buffer . ((side . bottom) (slot . 1) (window-height . 0.45))))))
	 
	 
        ;; `((,dap-ui--locals-buffer . ((side . right) (slot . 1) (window-width . 0.40))))))
 ;;          (,dap-ui--expressions-buffer . ((side . right) (slot . 2) (window-width . 0.20)))
 ;;          (,dap-ui--sessions-buffer . ((side . right) (slot . 3) (window-width . 0.20)))
 ;;          (,dap-ui--breakpoints-buffer . ((side . left) (slot . 2) (window-width . ,treemacs-width)))
 ;;          (,dap-ui--debug-window-buffer . ((side . bottom) (slot . 3) (window-width . 0.20)))
 ;;          (,dap-ui--repl-buffer . ((side . right) (slot . 2) (window-width . 0.45))))))

(provide 'debugging)
