;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;	Go IDE Setup
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package go-mode
  :bind (:map go-mode-map
	      ("M-l" . godef-jump)
	      ("M-h" . pop-tag-mark))
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
    (setq company-tooltip-limit 20)                      ; bigger popup window
    (setq company-idle-delay .2)                         ; decrease delay before autocompletion popup shows
    (setq company-echo-delay 0)                          ; remove annoying blinking
    ))

(use-package go-eldoc
  :config
  (progn
    (add-hook 'go-mode-hook 'go-eldoc-setup)
    ))

(use-package go-guru :defer t)
(use-package flycheck-gometalinter :defer t)

;; Golang Playgraound for emacs
(use-package go-playground
  :bind (:map go-mode-map
	      ("M-<RET>" . go-playground-exec)))

;; Dlv Debugger
;; (require 'go-dlv)

(provide 'pkg-golang)
