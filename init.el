;; Disable mouse interface. Doing it early to avoid redrawing later.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-screen t)

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

;;; lang
(setq default-input-method "russian-computer")

;;; show line numbers
(global-linum-mode 1)

;;; show column number
(column-number-mode 1)

;;; buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;; highlight parentheses
(show-paren-mode 1)

;;; subword moving
(global-subword-mode 1)

;;; re-builder string syntax by default
(setq reb-re-syntax 'string)

;;; compilation links for shell
(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

;;; smartscan for python
(add-hook 'python-mode-hook 'smartscan-mode)

;;; nosetests
(require 'nose)
(add-hook 'python-mode-hook (lambda () (nose-mode t)))
(setq nose-global-name "nosetests")
(add-hook 'python-mode-hook
	  '(lambda () (define-key python-mode-map "\C-ct" 'nosetests-again)))

;;; backups settings
(setq
 backup-by-copying t
 backup-directory-alist
 '(("." . "~/.saves"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

;;; tramp
(require 'tramp)
(setq tramp-default-method "ssh")

;;; ido
(ido-mode 1)
(setq ido-enable-flex-matching 1)

;;; windmove
;;; Fix for shift up = <select> is undefined for windmove
(define-key input-decode-map "\e[1;2A" [S-up])
(windmove-default-keybindings)

;;; projectile
(projectile-global-mode)

;;; grep-a-lot
(require 'grep-a-lot)
(grep-a-lot-setup-keys)

;;; ag
(setq ag-highlight-search t)

;;; yaml-mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;;; smex
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;;; ibuffer sorting
(setq ibuffer-default-sorting-mode 'major-mode)

;;; org-mode
;;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)


;;; files - modes
(setq auto-mode-alist
      (append
       '(("SConstruct" . python-mode)
	 ("SConscript" . python-mode))
       auto-mode-alist))


;;;; functions

;;; http://www.emacswiki.org/emacs/UnfillParagraph
;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;;; http://www.emacswiki.org/emacs/UnfillRegion
(defun unfill-region (beg end)
  "Unfill the region, joining text paragraphs into a single
    logical line.  This is useful, e.g., for use with
    `visual-line-mode'."
  (interactive "*r")
  (let ((fill-column (point-max)))
    (fill-region beg end)))

;;;; global keys
(global-set-key (kbd "C-c g") 'magit-status)
(define-key global-map "\M-Q" 'unfill-paragraph)
(define-key global-map "\C-\M-Q" 'unfill-region)
(global-set-key (kbd "C-c q") 'auto-fill-mode)
;;;; ibuffer instead of buffer-menu
(global-set-key (kbd "C-x C-b") 'ibuffer)
;;; killing buffer without prompt
(global-set-key (kbd "C-x k") 'kill-this-buffer)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(history-length 1000)
 '(show-trailing-whitespace t)
 '(x-select-enable-clipboard t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
