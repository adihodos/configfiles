(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

;;(require 'company)
;;(add-hook 'after-init-hook 'global-company-mode)

(require 'cc-mode)
;;(setq company-backends (delete 'company-semantic company-backends))
;;(define-key c-mode-map  [(tab)] 'company-complete)
;;(define-key c++-mode-map  [(tab)] 'company-complete)

;; Available C style:
;;(setq
;; c-default-style "ellemtel" ;; set style to "linux"
;; )

(global-set-key (kbd "RET") 'newline-and-indent)  ; automatically indent when press RET

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; use space to indent by default
(setq-default indent-tabs-mode nil)

(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

;; no beeps
(setq visible-bell 1)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-showhide-fringe-menu-customize-disable)

(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

(global-hl-line-mode)
(global-linum-mode)
(load-theme 'leuven t)
(show-paren-mode t)

(set-frame-font "Roboto Mono Thin-15")

;;(global-set-key "\C-cw"
;;                (lambda ()
;;                  (interactive)
;;                  (let ((woman-use-topic-at-point t))
;;                    (woman))))

(global-set-key  "\C-cw" (lambda () (interactive) (man (current-word))))

;;(require 'column-marker)
;; (add-hook 'prog-mode-hook (lambda () (interactive) (column-marker-1 80)))

;;(require 'fill-column-indicator)
;;(setq fci-rule-width 1)
;;(setq fci-rule-color "OrangeRed")
;;(add-hook 'after-change-major-mode-hook 'fci-mode)

;;(require 'helm)
;;(require 'helm-projectile)
;;(require 'projectile)
;;(add-hook 'c-mode-hook 'projectile-mode)
;;(add-hook 'c++-mode-hook 'projectile-mode)

;; GLSL
(autoload 'glsl-mode "glsl-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))

;(require 'google-c-style)
;(add-hook 'c-mode-common-hook 'google-set-c-style)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("1160f5fc215738551fce39a67b2bcf312ed07ef3568d15d53c87baa4fd1f4d4e" default)))
 '(fringe-mode 0 nil (fringe))
 '(package-selected-packages
   (quote
    (boron-theme atom-one-dark-theme cmake-mode smooth-scrolling clang-format irony helm-projectile glsl-mode fill-column-indicator company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(require 'clang-format)
;;(require 'clang-format)
(global-set-key (kbd "C-c i") 'clang-format-region)
(global-set-key (kbd "C-c u") 'clang-format-buffer)
(setq clang-format-style-option "webkit")
;(define-key c-mode-base-map (kbd "C-S-f") 'clang-format-buffer)

(add-hook 'c++-mode-hook
          (lambda () (add-hook 'before-save-hook (lambda () (clang-format-buffer)) nil 'local)))

(add-hook 'c-mode-hook
          (lambda () (add-hook 'before-save-hook (lambda () (clang-format-buffer)) nil 'local)))

(require 'smooth-scrolling)
;;(require 'cmake-mode)

;;(add-hook 'c++-mode-hook 'irony-mode)
;;(add-hook 'c-mode-hook 'irony-mode)
;;(add-hook 'objc-mode-hook 'irony-mode)

;;(add-hook 'c++-mode-hook 'company-mode)
;;(add-hook 'c-mode-hook 'company-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
;;(defun my-irony-mode-hook ()
;;  (define-key irony-mode-map [remap completion-at-point]
;;    'irony-completion-at-point-async)
;;  (define-key irony-mode-map [remap complete-symbol]
;;    'irony-completion-at-point-async))
;;(add-hook 'irony-mode-hook 'my-irony-mode-hook)
;;(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;;(eval-after-load 'company
;;'(add-to-list 'company-backends 'company-irony))
;; (optional) adds CC special commands to `company-begin-commands' in order to
;; trigger completion at interesting places, such as after scope operator
;;     std::|
;;(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;;(add-to-list 'irony-cdb-search-directory-list "build")
;;(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

