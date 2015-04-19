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


;;; generic mode for 1C sources

(require 'generic-x)

(define-generic-mode
    '1c-mode  ;; mode name
  '("//")  ;; comments

  '("if" "если" "then" "тогда" "elsif" "иначеесли" "else" "иначе" "endif"
    "конецесли" "do" "цикл" "for" "для" "to" "по" "each" "каждого" "in" "из"
    "while" "пока" "enddo" "конеццикла" "procedure" "процедура" "endprocedure"
    "конецпроцедуры" "function" "функция" "endfunction" "конецфункции" "var"
    "перем" "export" "экспорт" "goto" "перейти" "and" "и" "or" "или" "not" "не"
    "val" "знач" "break" "прервать" "continue" "продолжить" "return" "возврат"
    "try" "попытка" "except" "исключение" "endtry" "конецпопытки" "raise"
    "вызватьисключение" "false" "ложь" "true" "истина" "undefined"
    "неопределено" "null" "new" "новый" "execute" "выполнить"

    "If" "Если" "Then" "Тогда" "Elsif" "ИначеЕсли" "Else" "Иначе" "Endif"
    "КонецЕсли" "Do" "Цикл" "For" "Для" "To" "По" "Each" "Каждого" "In" "Из"
    "While" "Пока" "Enddo" "КонецЦикла" "Procedure" "Процедура" "Endprocedure"
    "КонецПроцедуры" "Function" "Функция" "Endfunction" "КонецФункции" "Var"
    "Перем" "Export" "Экспорт" "Goto" "Перейти" "And" "И" "Or" "Или"
    "Not " "Не " "Val" "Знач" "Break" "Прервать" "Continue" "Продолжить"
    "Return" "Возврат" "Try" "Попытка" "Except" "Исключение" "Endtry"
    "КонецПопытки" "Raise" "ВызватьИсключение" "False" "Ложь" "True" "Истина"
    "Undefined" "Неопределено" "Null" "New" "Новый" "Execute" "Выполнить")
  '(("=" . 'font-lock-operator)
    (";" . 'font-lock-builtin))
  nil  ;; no extensions
  nil
  "Generic mode for 1C"
  )


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

;; log4j
(add-to-list 'auto-mode-alist '("\\.log\\'" . log4j-mode))
(add-to-list 'auto-mode-alist '("\\.out\\'" . log4j-mode))
