;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;	Python IDE Setup
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(use-package elpy
  :ensure t
  :bind (:map python-mode-map
	      ("M-l" . elpy-goto-definition)
	      ("M-h" . xref-pop-marker-stack)
	      ("C-x C-e" . python-shell-send-region)
	      ("C-x C-r" . run-python))
  :init
  (progn
    (elpy-enable)
    (setq python-shell-interpreter "ipython"
	  python-shell-interpreter-args "-i --simple-prompt")

    ;; enable elpy jedi backend
    (setq elpy-rpc-backend "jedi")
    (define-key python-mode-map (kbd "RET")
      'newline-and-indent)
    ))

;; (use-package yapfify
  ;; :config
  ;; (add-hook 'python-mode-hook 'yapf-mode))
;; (use-package yapfify
;;   :init
;;   (progn
;;     (add-hook 'python-mode-hook 'yapf-mode)
    
;;     (defun pkg-enable-yapfify-buffer ()
;;       (yapfify-buffer))
;;     (add-hook 'before-save-hook 'pkg-enable-yapfify-buffer)
;;     ))

(provide 'pkg-python)
