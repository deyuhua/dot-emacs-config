;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;	Org TODO keywords
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(setq org-todo-keywords 
      '((sequence "TODO(t)" "INPROGRESS(i)" "WAITING(w)" "REVIEW(r)" "|" "DONE(d)" "CANCELED(c)")))

(setq org-todo-keyword-faces
      '(("TODO" . org-warning)
	("INPROGRESS" . "yellow")
	("WAITING" . "purple")
	("REVIEW" . "orange")
	("DONE" . "green")
	("CANCELED" .  "red")))

;; close todo with note
(setq org-log-done 'note)
(setq org-startup-truncated t)

(setq org-log-into-drawer t)
(setq org-agenda-custom-commands
      '(("b" "Blog idea" tags-todo "BLOG")
	("s" "Someday" todo "SOMEDAY")
	("S" "Started" todo "STARTED")
	("w" "Waiting" todo "WAITING")
	("d" . " 任务安排 ")
	("da" " 重要且紧急的任务 " tags-todo "+PRIORITY=\"A\"")
	("db" " 重要且不紧急的任务 " tags-todo "+PRIORITY=\"B\"")
	("dc" " 不重要且紧急的任务 " tags-todo "+PRIORITY=\"C\"")
	("p" . " 项目安排 ")
	("W" "Weekly Review" tags-todo "PROJECT")))

;; add the org-pomodoro-mode-line to the global-mode-string
(unless global-mode-string (setq global-mode-string '("")))
(unless (memq 'org-pomodoro-mode-line global-mode-string)
  (setq global-mode-string (append global-mode-string
                                   '(org-pomodoro-mode-line))))

(add-hook 'org-mode-hook '(lambda () (setq fill-column 80)))
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; auto toggle todo status
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; pomodoro tech setting
(use-package org-pomodoro
  :ensure t
  :commands (org-pomodoro)
  :config
  (setq alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil))))
  (setq org-pomodoro-length 25)
  (setq org-pomodoro-short-break-length 5)
  (setq org-pomodoro-long-break-length 15)
  (setq org-pomodoro-format " ⏰  %s ")
  (setq org-pomodoro-long-break-format "  ☕☕☕  %s ")
  (setq org-pomodoro-short-break-format "  ☕  %s ")
  (setq org-pomodoro-ticking-sound-p nil)
  )

;; change a good appearance of todo items
(use-package org-bullets
  :config
  (progn
    (setq org-bullets-bullet-list '("☯" "✿" "✚" "◉" "❀"))
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
    ))

(use-package org-alert
  :defer t
  :config
  (progn
    (setq alert-default-style 'libnotify)
    ))

;; org files include in agenda view
(defvar org-icloud-path "/Users/deyuhua/Library/Mobile Documents/iCloud~com~appsonthemove~beorg/Documents/org/")
(defvar ep-work-org (concat org-icloud-path "work.org"))
(defvar ep-learning-org (concat org-icloud-path "learning.org"))
(defvar ep-personal-org (concat org-icloud-path "personal.org"))
(defvar ep-inbox-org (concat org-icloud-path "inbox.org"))
(defvar ep-habit-org (concat org-icloud-path "habit.org"))

(setq org-agenda-files (list ep-work-org
			     ep-learning-org
			     ep-personal-org
			     ep-inbox-org
			     ep-habit-org))
(setq org-archive-location (concat  "/Users/deyuhua/Workspace/Archive/Orgs/" "%s_archive::"))

;; org caputer settings
(setq org-default-notes-file ep-inbox-org)
(define-key global-map "\C-cc" 'org-capture)

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline ep-inbox-org "代办事项")
         "* TODO %?\n %i\n")))

(setq org-refile-targets
      '((ep-learning-org :maxlevel . 1)
        (ep-work-org :maxlevel . 1)
	(ep-personal-org :maxlevel . 1)
	(ep-habit-org :maxlevel . 1)))

;; open agenda view when emacs startup
(defun agenda-view()
  (interactive)
  (progn
    (org-agenda 'a "a")
    (delete-other-windows)))

;; open agenda view every hour
;; (run-at-time "0 sec" 3600 'agenda-view)

;; org bable playground
(require 'ob-C)
(require 'ob-js)
(require 'ob-python)
(require 'ob-shell)
(require 'ob-java)
;;(require 'ob-scala)
(require 'ob-plantuml)
(require 'ob-R)
(require 'ob-redis)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((plantuml . t)
   (C . t)
   (R . t)
   (js . t)
   (python . t)
   (shell . t)
   (java . t)
   (redis . t)
   ;; (scala . t))
 ))


(provide 'pkg-org-mode)
