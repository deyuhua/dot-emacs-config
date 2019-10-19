;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;	Language server protocol Setup
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package lsp-mode
  :ensure t
  :config
  (add-hook 'go-mode-hook #'lsp)
  (add-hook 'python-mode-hook #'lsp)

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "pyls")
		    :major-modes '(python-mode)
		    :server-id 'pyls))
  :commands lsp
  )

(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-sideline-ignore-duplicate t)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package company-lsp
  :ensure t
  :config
  (push 'company-lsp company-backends))


(provide 'pkg-lsp)
