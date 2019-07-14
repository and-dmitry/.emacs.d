;; Next line is added by Package.el. Commented out because we're
;; doing it later.
;(package-initialize)


;;; Configuring GUI. Doing it early to avoid redrawing later.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)

;; russian input
(setq default-input-method "russian-computer")


;; set code paths
(let ((personal-dir (locate-user-emacs-file "elisp/")))
  ;; load path for my personal code
  (add-to-list 'load-path personal-dir)
  ;; separate file for Custom
  (setq custom-file (concat personal-dir "custom.el"))
)


;; enable/disable features
(put 'narrow-to-region 'disabled nil)


(load "init-packages")
(load "init-defaults")
(load "init-modes")
(load "utils")
(load "init-keybindings")
;; Load customizations. It's important that packages have been already
;; installed and initialized.
(load-file custom-file)


;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))
