;;;; set global key bindings


;;; windmove

;; Fix for "shift up = <select> is undefined for windmove"
(define-key input-decode-map "\e[1;2A" [S-up])
(windmove-default-keybindings)
;; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

;; killing buffer without prompt
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; use ibuffer instead of buffer-menu
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; smex
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; magit
(global-set-key (kbd "C-c g") 'magit-status)

;; fill commands
(define-key global-map "\M-Q" 'unfill-paragraph)
(define-key global-map "\C-\M-Q" 'unfill-region)
(global-set-key (kbd "C-c q") 'auto-fill-mode)

;; expand region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; elfeed
(global-set-key (kbd "C-c f") 'elfeed)

;; easy-kill
(global-set-key [remap kill-ring-save] 'easy-kill)

;; compile
(global-set-key [f9] #'recompile)

;; dumb-jump
(global-set-key (kbd "C-c j") 'dumb-jump-go)
(global-set-key (kbd "C-c J") 'dumb-jump-back)
