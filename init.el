;; Next line is added by Package.el. Commented out because we're
;; doing it later.
;(package-initialize)


;;; Configuring GUI. Doing it early to avoid redrawing later.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
;; This is the right way to start emacs maximized. Using
;; default-frame-alist breaks ediff. Of course, there's also a command
;; line option (-mm), but I want it to be part of my .emacs.d.
(setq initial-frame-alist '((fullscreen . maximized)))


;; russian input
(setq default-input-method "russian-computer")


;; set code paths
(let ((personal-dir (locate-user-emacs-file "elisp/")))
  ;; load path for my personal code
  (add-to-list 'load-path personal-dir)
)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))


;; enable/disable features
(put 'narrow-to-region 'disabled nil)


(load "init-packages")
(load "init-defaults")
(load "init-modes")
(load "utils")
(load "init-keybindings")
;; Load customizations. It's important that packages have been already
;; installed and initialized.
(load custom-file 'noerror)


;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))
