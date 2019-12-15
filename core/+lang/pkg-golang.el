;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;	Go IDE Setup
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package go-mode
  :bind (:map go-mode-map
	      ("M-l" . godef-jump)
	      ("M-h" . pop-tag-mark))
  :config
  (progn
    (setq gofmt-command "goimports")
    (add-hook 'before-save-hook 'gofmt-before-save)
    ))

(use-package company-go
  :init
  (progn
    (add-hook 'go-mode-hook (lambda ()
                              (set (make-local-variable 'company-backends) '(company-go))
                              (company-mode)))
    (setq company-go-show-annotation t)
    (setq company-tooltip-limit 20)                      ; bigger popup window
    )
  )

(use-package go-eldoc
  :config
  (progn
    (add-hook 'go-mode-hook 'go-eldoc-setup)
    ))

(use-package go-guru
  :defer t
  :hook (go-mode . go-guru-hl-identifier-mode))
(use-package go-rename
  :ensure t)

(use-package flycheck-gometalinter :defer t)

;; Golang Playgraound for emacs
(use-package go-playground
  :bind (:map go-mode-map
	      ("M-<RET>" . go-playground-exec)))

;; Dlv Debugger
;; (require 'go-dlv)

;; go get golang.org/x/tools/cmd/...
;; go get github.com/rogpeppe/godef
;; go get -u github.com/nsf/gocode
;; go get golang.org/x/tools/cmd/goimports

(provide 'pkg-golang)
