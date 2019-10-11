;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;	Markdown mode setting
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))


(provide 'pkg-markdown)
