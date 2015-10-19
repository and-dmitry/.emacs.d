;; Disable mouse interface. Doing it early to avoid redrawing later.
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


(load "init-packages")
;; custom version of pt.el (testing)
(require 'pt)

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
