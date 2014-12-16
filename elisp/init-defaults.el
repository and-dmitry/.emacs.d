;;; global defaults

;; show line numbers
(global-linum-mode 1)

;; show column number
(column-number-mode 1)

;; highlight parentheses
(show-paren-mode 1)

;; subword moving
(global-subword-mode 1)

;; use spaces for indentation
(setq-default indent-tabs-mode nil)


;;; backups settings
(setq
 backup-by-copying t
 backup-directory-alist
 '(("." . "~/.saves"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)
