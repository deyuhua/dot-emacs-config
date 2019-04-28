;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;	coding theme settings
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(if (display-grayscale-p)
    ;; (use-package atom-one-dark-theme
    ;;   :config
    ;;   (load-theme 'atom-one-dark t))
    (use-package dracula-theme
      :config
      (load-theme 'dracula t))
    ;; (use-package nova-theme
    ;;   :config
    ;;   (load-theme 'nova t))
    ;; (use-package 'spacemacs-theme
      ;; :config
    ;; (load-theme 'spacemacs-dark t))
    ;; (load-theme 'nord t)
  (require 'item2-molokai)
  )

(use-package doom-modeline
      :ensure t
      :hook (after-init . doom-modeline-mode))

;; (set-face-background hl-line-face "#00a8b5")

(provide 'pkg-appearance)
