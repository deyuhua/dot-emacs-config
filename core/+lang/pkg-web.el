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

(provide 'pkg-web)
