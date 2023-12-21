;; (defvar adi/lldb-vscode-path (executable-find "lldb-vscode") "Path to the lldb-vscode executable")

;; (use-package dap-mode
;;   :commands dap-debug
;;   :init
;;   (dap-mode 1)
;;   (dap-tooltip-mode 1)
;;   (dap-ui-mode 1)
;;   (dap-ui-controls-mode 1)
;;   (dap-auto-configure-mode 1)
;;   (require 'dap-lldb)
;;   :bind
;;   (:map dap-mode-map
;; 	("<f5>" . dap-debug)
;; 	("<f11>" . dap-step-in)
;; 	("<S-f11>" . dap-step-out)
;; 	("S-<f5>" . dap-continue)
;; 	("S-<f9>" . dap-breakpoint-toggle))
;;   :config
;;   (setq dap-lldb-debug-program (list (executable-find "lldb-vscode")))
;;   (setq dap-auto-configure-features '(locals breakpoints expressions repl controls tooltip)))

(provide 'debugging)
