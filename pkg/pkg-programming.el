;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;	Python IDE Setup
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(setq python-indent-offset 4)
(setenv "PYTHONIOENCODING" "utf-8")

(use-package pyvenv
  :config
  (progn
    (pyvenv-mode)
    (defalias 'workon 'pyvenv-workon)))

(use-package elpy
  :ensure t
  :init
  (progn
    (elpy-enable)
    (setq python-shell-interpreter "ipython"
	  python-shell-interpreter-args "-i --simple-prompt")
    ;; enable elpy jedi backend
    (setq elpy-rpc-backend "jedi")
    (setq elpy-modules '(elpy-module-sane-defaults
			 elpy-module-company
			 elpy-module-eldoc
			 elpy-module-highlight-indentation
			 elpy-module-pyvenv
			 elpy-module-yasnippet))
    (define-key python-mode-map (kbd "RET")
      'newline-and-indent)
    ))

(use-package cython-mode :defer t)

(use-package yapfify
  :init
  (progn
    (add-hook 'python-mode-hook 'yapf-mode)
    ))

(use-package anaconda-mode
  :init
  (progn
    (add-hook 'python-mode-hook 'anaconda-mode)
    ))

(use-package company-anaconda
  :ensure t
  :init
  (progn
    (add-to-list 'company-backends '(company-anaconda :with company-yasnippet))
    (add-hook 'python-mode-hook 'anaconda-mode)
    )
  )

;; (defun pkg-disable-multi-auto-complete ()
;;   (auto-complete-mode -1))
;; (add-hook 'python-mode-hook 'pkg-disable-multi-auto-complete)


;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;	Markdown mode setting
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package markdown-mode
  :ensure t
  :bind (("C-c p" . livedown-preview)
	 ("C-c k" . livedown-kill))
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;	Go IDE Setup
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package go-mode
  :config
  (progn
    (add-hook 'before-save-hook 'gofmt-before-save)
    ))

(use-package company-go
  :init
  (progn
    (add-hook 'go-mode-hook (lambda ()
                              (set (make-local-variable 'company-backends) '(company-go))
                              (company-mode)))
    (setq company-go-show-annotation t)
    ))

(use-package go-eldoc
  :config
  (progn
    (add-hook 'go-mode-hook 'go-eldoc-setup)
    ))

(use-package go-guru :defer t)

(use-package flycheck-gometalinter :defer t)

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))
;; (setenv "GOPATH" "/Users/deyuhua/Workspace/SourceCode/golang")
(defun go-work-on ()
  (interactive)
  (let ((path (read-directory-name "Golang PATH: ")))
    (progn
      (setenv "GOPATH" path)
      (message "Work On Golang PATH: %s" path))))

;; (defun go-get-pkg ()
;;   (interactive)
;;   (progn
;;     (let ((pkg-name (read-string "Enter GO Pkg Name:")))
;;       (start-process "gopkg" "gopkg" "go" "get" pkg-name)
;;       (switch-to-buffer "gopkg")
;;       (message "Try to install go package: " pkg-name)
;;       )))

(defun auto-complete-for-go ()
  (auto-complete-mode 1))
(add-hook 'go-mode-hook 'auto-complete-for-go)

(use-package go-autocomplete)
(with-eval-after-load 'go-mode
  (require 'go-autocomplete))

(defun pkg-go-mode-hook ()
					; Use goimports instead of go-fmt
  (setq gofmt-command "goimports")
					; Call Gofmt before saving
  (add-hook 'before-save-hook 'gofmt-before-save)
					; Customize compile command to run go build
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
					; Godef jump key binding
  (local-set-key (kbd "M-l") 'godef-jump)
  (local-set-key (kbd "M-h") 'pop-tag-mark)
  )
(add-hook 'go-mode-hook 'pkg-go-mode-hook)

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; 	Golang Playgraound for emacs
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package go-playground)
(global-set-key (kbd "M-<RET>") 'go-playground-exec)


;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;	C/C++ IDE Setup
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(add-hook 'c-mode-hook 'hs-minor-mode)
(add-hook 'c++-mode-hook 'hs-minor-mode)

(use-package cc-mode
  :bind (("M-h" . helm-gtags-pop-stack)
	 ("M-l" . helm-gtags-find-tag))
  :config
  (progn
    (require 'compile)
    (c-toggle-auto-newline 1)))

(use-package disaster
  :commands (disaster))

(use-package clang-format)

(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode)))

(use-package company-c-headers :defer t)

(use-package gdb-mi
  :init
  (setq
   ;; use gdb-many-windows by default when `M-x gdb'
   gdb-many-windows t
   ;; Non-nil means display source file containing the main routine at startup
   gdb-show-main t))


;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;	Scala IDE Setup
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))


;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;	Dart IDE Setup
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package dart-mode
  :custom
  (dart-format-on-save t)
  (dart-enable-analysis-server t)
  (dart-sdk-path "/usr/local/opt/dart/libexec"))

(use-package flutter
  :after dart-mode
  :custom
  (flutter-sdk-path "/Users/deyuhua/Workspace/Packages/SDKs/flutter")
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; 	Web IDE Setup
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package emmet-mode)
(use-package web-mode
  :config
  (progn
	(defun pkg-web-mode-hook ()
      "Hooks for Web mode."
      (setq web-mode-markup-indent-offset 4)
      (setq web-mode-code-indent-offset 4)
      (setq web-mode-css-indent-offset 4))

    (add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))

    (add-hook 'web-mode-hook  'my-web-mode-hook)    
    (setq tab-width 4)

    (add-hook 'web-mode-hook  'emmet-mode)))
(use-package web-beautify)


(provide 'pkg-programming)
