;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;	Language server protocol Setup
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package lsp-mode
  :ensure t
  
  :config
  
  (add-hook 'go-mode-hook #'lsp)
  (add-hook 'python-mode-hook #'lsp)
  (add-hook 'c++-mode-hook #'lsp)
  (add-hook 'c-mode-hook #'lsp)
  (add-hook 'rust-mode-hook #'lsp)

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "pyls")
		    :major-modes '(python-mode)
		    :server-id 'pyls))
  :commands lsp
  )

(use-package lsp-ui
  :ensure t
  :custom-face
  (lsp-ui-doc-background ((t (:background "#8b8b7a"))))
  :init (setq lsp-ui-doc-enable t
              lsp-ui-doc-include-signature t
              lsp-ui-doc-position 'at-point
              lsp-ui-doc-border (face-foreground 'default)

              ;; lsp-enable-snippet nil
              lsp-ui-sideline-enable nil
	      lsp-ui-peek-enable nil)
  :bind (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu))
  :config
  (setq lsp-ui-sideline-ignore-duplicate t)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package company-lsp
  :ensure t
  :config
  (push 'company-lsp company-backends))


(provide 'pkg-lsp)
