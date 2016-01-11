;;; major modes

;; highlight trailing whitespace in programming modes
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))


;;; python

;; smartscan for python
(add-hook 'python-mode-hook 'smartscan-mode)

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


;; yaml
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))


;;; ruby

;; smartscan
(add-hook 'ruby-mode-hook 'smartscan-mode)

;; ruby on rails files
(add-to-list 'auto-mode-alist '("\\.jbuilder$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))


;;; java

;; use minimal arguments identation to reduce line length
(add-hook 'java-mode-hook (lambda () (c-set-offset 'arglist-intro '+)))


;;; groovy

;; This caused errors. Now using more general approach in
;; init-defaults.el
;(add-hook 'groovy-mode-hook (lambda () (setq c-basic-offset 4)))


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
;; enable Dired Extra
(require 'dired-x)

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

;; which-key
(which-key-mode)

;; log4j
;; TODO: case-fold-search is nil in this mode. why? I don't like it.
(add-to-list 'auto-mode-alist '("\\.log\\'" . log4j-mode))
(add-to-list 'auto-mode-alist '("\\.out\\'" . log4j-mode))

;;; org-mode

(setq org-modules '(org-habit))
;; throw error on edits that affect invisible part of buffer
(setq org-catch-invisible-edits 'error)
;; add timestamp when closing task
(setq org-log-done 'time)
;; fontify src blocks
(setq org-src-fontify-natively t)
;; use something shorter and better looking than "..."
(setq org-ellipsis "⤵")
;; open agenda in the current window
(setq org-agenda-window-setup (quote current-window))
;; show deadlines in next 7 days
(setq org-deadline-warning-days 7)
;; 2 weeks agenda span
(setq org-agenda-span (quote fortnight))
;; don't show tasks as scheduled if they are already shown as a deadline
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
;; files
(setq org-agenda-files '("~/sync/org/gtd.org" "~/sync/org/dates.org"))
(setq org-refile-targets '(("~/sync/org/gtd.org" :level . 1)
                           ("~/sync/org/someday.org" :level . 1)))
;; configure capturing
(setq org-directory "~/sync/org")
(setq org-default-notes-file "~/sync/org/log.org")
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/sync/org/gtd.org" "Задачи")
         "* TODO %?\n  %i")
        ("l" "Log" entry (file+datetree "~/sync/org/log.org")
         "* %?\n  Добавлено %U\n  %i")))

;; auto-fill for org-mode
(add-hook 'org-mode-hook 'turn-on-auto-fill)


;; calendar
(add-hook 'calendar-load-hook
          (lambda ()
            (calendar-set-date-style 'european)))
(setq calendar-week-start-day 1)
(setq calendar-day-name-array ["Вс" "Пн" "Вт" "Ср" "Чт" "Пт" "Сб"]
      calendar-month-name-array ["Январь" "Февраль" "Март" "Апрель" "Май"
                                 "Июнь" "Июль" "Август" "Сентябрь"
                                 "Октябрь" "Ноябрь" "Декабрь"])


;;; elfeed
(require 'elfeed-org)
(elfeed-org)
(setq rmh-elfeed-org-files (list "~/sync/org/elfeed.org"))
(setq elfeed-db-directory "~/sync/app/elfeed/")


;; rst

;; rst - electric indent messes up everything (emacs 24.4.1)
(add-hook 'rst-mode-hook
          (lambda ()
            (electric-indent-local-mode -1)))
;; enable auto-fill
(add-hook 'rst-mode-hook 'turn-on-auto-fill)


;; web-mode

(require 'web-mode)
;; extensions
(add-to-list 'auto-mode-alist '("\\.gsp$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))
;; always use server comments
(setq web-mode-comment-style 2)


;; nxml-mode, xmllint format
(add-hook 'nxml-mode-hook
          '(lambda ()
             (define-key nxml-mode-map "\C-cm" 'xmllint-format-buffer)))
