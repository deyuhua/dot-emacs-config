(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp)))
  )

(setq ccls-executable "/usr/local/bin/ccls")


;; (setq ccls-initialization-options '(:index (:comments 2) :completion (:detailedLabel t)))
;; (setq ccls-executable "/usr/local/Cellar/ccls/0.20190823.3/bin/ccls")

;; (use-package eglot
  ;; :config
  ;; (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  ;; (add-hook 'c-mode-hook 'eglot-ensure)
  ;; (add-hook 'c++-mode-hook 'eglot-ensure)
;; )

;; (use-package cquery)
;; (setq cquery-executable "/usr/local/Cellar/cquery/20180718/bin/cquery")
;; (setq cquery-extra-init-params '(:index (:comments 2) :cacheFormat "msgpack"))

;; (use-package eglot
  ;; :hook
  ;; (c-mode . eglot-ensure)
  ;; (python-mode . eglot-ensure)
  ;; :config
;; (setq eglot-ignored-server-capabilites (quote (:documentHighlightProvider))))



(provide 'pkg-c++)
