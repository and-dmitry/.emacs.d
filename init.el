;; Disable mouse interface. Doing it early to avoid redrawing later.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-screen t)

;; russian input
(setq default-input-method "russian-computer")

;; load path for my personal code
(add-to-list 'load-path "~/.emacs.d/elisp/")

;; separate file for Custom
;; TODO: use user-emacs-directory?
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


;;; repos and packages
;;; FIXME: fails to install packages
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize)
  ;; check if the packages is installed; if not, install it.
  (mapc
   (lambda (package)
     (or (package-installed-p package)
	 (if (y-or-n-p (format "Package %s is missing. Install it? " package))
	     (package-install package))))
   '(magit projectile ack-and-a-half ag grep-a-lot nose yaml-mode smex expand-region smartscan))
  )


(load "init-defaults")
(load "init-modes")
(load "utils")
(load "init-keybindings")
