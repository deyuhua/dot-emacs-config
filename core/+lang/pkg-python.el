;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;	Python IDE Setup
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(use-package python
  :mode ("\\.py" . python-mode)
  :ensure t
  :config
  (flymake-mode)
  (use-package elpy
    :ensure t
    :bind (:map python-mode-map
		("M-." . elpy-goto-definition)
		("M-," . xref-pop-marker-stack)
		("C-x C-e" . python-shell-send-region)
		("C-x C-r" . run-python))
    :init
    (remove-hook 'elpy-modules 'elpy-module-flymake)
    (setq flymake-start-on-flymake-mode nil)
    (setq flymake-start-syntax-check nil)
    :config
    (progn
      (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
      (setq python-shell-interpreter "ipython"
	    python-shell-interpreter-args "-i --simple-prompt")
      
      ;; enable elpy jedi backend
      (setq elpy-rpc-backend "jedi")
      (define-key python-mode-map (kbd "RET")
	'newline-and-indent)))
  
  (elpy-enable)
  )

(provide 'pkg-python)
