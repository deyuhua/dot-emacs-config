;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;	Python IDE Setup
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(use-package python
  :mode ("\\.py" . python-mode)
  :ensure t)

(use-package pyenv-mode
  :init
  (add-to-list 'exec-path "~/.pyenv/shims")
  (setenv "WORKON_HOME" "~/.pyenv/versions/")
  :config
  (pyenv-mode))

(use-package yapfify
  :config
  (defun format-python()
    (when (eq major-mode 'python-mode)
      (yapfify-buffer)))
  
  (add-hook 'python-mode-hook 'yapf-mode)
  (add-hook 'before-save-hook #'format-python)
  )

(provide 'pkg-python)
