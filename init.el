;; Disable mouse interface. Doing it early to avoid redrawing later.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-screen t)

;; russian input
(setq default-input-method "russian-computer")

(let ((personal-dir (locate-user-emacs-file "elisp/")))
  ;; load path for my personal code
  (add-to-list 'load-path personal-dir)
  ;; separate file for Custom
  (setq custom-file (concat personal-dir "custom.el"))
  (load-file custom-file)
)


;;; packages

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; initialize packages to be able to install and configure them
(setq package-enable-at-startup nil)
(package-initialize)
;; get list of packages
(unless package-archive-contents
  (package-refresh-contents))
;; install missing
(let ((packages
       '(ack-and-a-half
	 ag
	 expand-region
	 grep-a-lot
         log4j-mode
	 magit
	 nose
	 projectile
	 smartscan
	 smex
	 yaml-mode)))
  (mapc
   (lambda (package)
     (unless (package-installed-p package)
       (if (y-or-n-p (format "Package %s is missing. Install it?" package))
	   (package-install package))))
   packages)
)

(load "init-defaults")
(load "init-modes")
(load "utils")
(load "init-keybindings")
