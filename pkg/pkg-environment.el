;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;	Setup pkg repo and install use-package
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(require 'package)
(setq package-enable-at-startup nil)

(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))
(unless (assoc-default "marmalade" package-archives)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)


;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;	setup coding system and window property
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(prefer-coding-system 'utf-8)
(setenv "LANG" "en_US.UTF-8")
(setenv	"LC_ALL" "en_US.UTF-8")
(setenv	"LC_CTYPE" "en_US.UTF-8")

;; setup titlebar appearance
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; useful mode settings
(display-time-mode 1)
(column-number-mode 1)
(show-paren-mode nil)
(display-battery-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(global-auto-revert-mode t)
(global-hl-line-mode nil)

(fset 'yes-or-no-p 'y-or-n-p)
(toggle-frame-fullscreen)

;; file edit settings
(setq tab-width 4
      inhibit-splash-screen t
      initial-scratch-message nil
      sentence-end-double-space nil
      make-backup-files nil
      indent-tabs-mode nil
      make-backup-files nil
      auto-save-default nil)


;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;	setup history of edited file
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(savehist-mode 1)
(setq savehist-file "~/.emacs.d/.savehist")
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))


;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;	operation system settings
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(cond ((string-equal system-type "darwin")
       (progn
	 ;; modify option and command key
	 (setq mac-command-modifier 'control)
	 (setq mac-option-modifier 'meta)

	 ;; batter copy and paste support for mac os x
	 (defun copy-from-osx ()
	   (shell-command-to-string "pbpaste"))
	 (defun paste-to-osx (text &optional push)
	   (let ((process-connection-type nil))
	     (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
	       (process-send-string proc text)
	       (process-send-eof proc))))
	 (setq interprogram-cut-function 'paste-to-osx)
	 (setq interprogram-paste-function 'copy-from-osx)

	 (message "Wellcome To Mac OS X, Have A Nice Day!!!"))))


;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;	coding font for english and chinese
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(set-face-attribute 'default nil
                    :family "Source Code Pro for Powerline"
		    :height 140
                    :weight 'medium
                    :width 'medium)

(if (display-graphic-p)
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
			charset (font-spec :family "Microsoft Yahei"
					   :size 14)))
  )


;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;	misc settings
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(setq custom-file "~/.emacs.d/.custom.el")
(load custom-file t)


;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;	Usefule global key bind
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(global-set-key (kbd "C-\\") 'comment-line)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; Tmux already use F1
;; Neotree refresh use F2
(global-set-key (kbd "<f3>") 'helm-recentf)
(global-set-key (kbd "<f4>") 'yas-insert-snippet)
(global-set-key (kbd "<f5>") 'grep-find)
(global-set-key (kbd "<f6>") 'pkg-org-publish)
(global-set-key (kbd "<f7>") 'helm-dash)
;; Neotree toggle use F8
(global-set-key (kbd "<f9>") 'agenda-view)
(global-set-key (kbd "<f10>") 'helm-M-x)
(global-set-key (kbd "<f12>") 'fiplr-find-file)

(global-set-key (kbd "M-0") 'next-multiframe-window)
(global-set-key (kbd "M-9") 'previous-multiframe-window)

(global-set-key (kbd "C-x C-m") 'helm-M-x)


(provide 'pkg-environment)
