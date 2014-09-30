;;; major modes

;;; python

;; smartscan for python
(add-hook 'python-mode-hook 'smartscan-mode)

;; highlight trailing whitespace in some modes
(add-hook 'python-mode-hook (lambda () (setq show-trailing-whitespace t)))

;; nosetests
(require 'nose)
(add-hook 'python-mode-hook (lambda () (nose-mode t)))
(setq nose-global-name "nosetests")
(add-hook 'python-mode-hook
	  '(lambda () (define-key python-mode-map "\C-ct" 'nosetests-again)))

;; scons files
(setq auto-mode-alist
      (append
       '(("SConstruct" . python-mode)
	 ("SConscript" . python-mode))
       auto-mode-alist))


;; elisp
(add-hook 'emacs-lisp-mode-hook (lambda () (setq show-trailing-whitespace t)))

;; yaml
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))


;;; ruby

;; smartscan
(add-hook 'ruby-mode-hook 'smartscan-mode)

;; ruby on rails files
(add-to-list 'auto-mode-alist '("\\.jbuilder$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))


;; ag
(setq ag-highlight-search t)


;;; minor modes and global

;; re-builder: string syntax by default
(setq reb-re-syntax 'string)

;; tramp
(require 'tramp)
(setq tramp-default-method "ssh")

;; reuse dired buffer
(put 'dired-find-alternate-file 'disabled nil)

;; ibuffer sorting
(setq ibuffer-default-sorting-mode 'major-mode)

;; compilation links for shell
(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

;; buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; ido
(ido-mode 1)
(setq ido-enable-flex-matching 1)

;; smex
(smex-initialize)

;; projectile
(projectile-global-mode)

;; grep-a-lot
(require 'grep-a-lot)
(grep-a-lot-setup-keys)
