;; This file makes sure all required packages are installed.

;; Setup the package subsystem

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(setq package-enable-at-startup nil)
(package-initialize)


;; Helper defuns

;; Check if all specified packages are installed.
;;
;; Returns a list of missing packages' names.
(defun init--get-missing-packages (packages)
  (delq nil
      (mapcar (lambda (package)
                (and (not (package-installed-p package)) package))
              packages)))

;; Install specified packages
(defun init--install-packages (packages)
  ;; Always update archive contents before installing packages.
  (package-refresh-contents)
  (mapc (lambda (package)
          (package-install package))
        packages))


;; These packages should be installed
;; TODO: Use package-selected-packages?
(setq my-packages
      '(
         adoc-mode
         beacon
         easy-kill
         elfeed
         elfeed-org
         expand-region
         free-keys
         grep-a-lot
         groovy-mode
         log4j-mode
         magit
         nginx-mode
         nose
         projectile
         rg
         smartscan
         smex
         solarized-theme
         web-mode
         which-key
         yaml-mode))


;; Install missing packages
(let ((missing (init--get-missing-packages my-packages)))
  (if (and missing
           (y-or-n-p
            (format "These packages are missing: %s. Install them? "
                    (mapconcat 'symbol-name missing ", "))))
      (init--install-packages missing)))
