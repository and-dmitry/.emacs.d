;;; global defaults

;; show line numbers
(global-display-line-numbers-mode)

;; show column number
(column-number-mode 1)

;; highlight parentheses
(show-paren-mode 1)

;; subword moving
(global-subword-mode 1)

;; use 4 spaces for indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default c-basic-offset 4)

;; confirm exit
(setq confirm-kill-emacs 'y-or-n-p)

;; change all prompts to y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; kill-ring settings
(setq kill-do-not-save-duplicates t)

;; C-u C-SPC C-SPC ... keeps popping mark
(setq set-mark-command-repeat-pop t)

;; cursor visibility
(beacon-mode 1)

;;; backups settings
(setq
 backup-by-copying t
 backup-directory-alist
 '(("." . "~/.saves"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

;; store more minibuffer history
(setq history-length 1000)

;; use system clipboard
(setq select-enable-clipboard t)

(setq visible-bell t)
