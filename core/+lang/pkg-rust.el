(use-package toml-mode)

(use-package rust-mode
  :hook (rust-mode . lsp)
  :config
  (defun format-rust()
    (when (eq major-mode 'rust-mode)
      (rust-format-buffer)))
  (add-hook 'before-save-hook #'format-rust)
  )

;; Add keybindings for interacting with Cargo
(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package rust-playground)

(provide 'pkg-rust)
