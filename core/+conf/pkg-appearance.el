;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;	coding theme settings
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; Code:

(use-package doom-themes)
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes"))
(if (not (display-graphic-p))
    (progn
      (add-to-list 'load-path (expand-file-name "~/.emacs.d/themes"))
      ;; (require 'doom-dracula-alt-theme)
      (load-theme 'dracula)
      ;; (load-theme 'spacemacs-light)
      ;; (load-theme 'doom-solarized-dark)
      )
  ;; (load-theme 'dracula)
  )

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; pick faces function
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(defun pkg-pick-gui-faces ()
  (custom-set-faces
   '(flycheck-error ((t (:box (:color "violet") :underline nil  :weight bold :foreground "violet") )))
   '(flycheck-warning ((t (:box (:color "yellow") :underline nil  :weight bold :foreground "yellow") )))
   '(flycheck-info ((t (:box (:color "green") :underline nil  :weight bold :foreground "green") )))
   )
  )

(defun pkg-pick-terminal-faces ()
  (custom-set-faces
   ;; Flycheck faces
   '(flycheck-error ((t (:underline nil :background "violet" :weight bold :foreground "white") )))
   '(flycheck-warning ((t (:underline nil :background "yellow" :weight bold :foreground "black") )))
   '(flycheck-info ((t (:underline nil :background "green" :weight bold :foreground "black") )))

   ;; Flymake faces
   '(flymake-error ((t (:underline nil  :weight bold :slant oblique :foreground nil) )))
   '(flymake-warning ((t (:underline nil  :weight bold :slant oblique :foreground nil) )))
   '(flymake-info ((t ( :underline nil  :weight bold :slant oblique :foreground nil) )))
   '(flymake-note ((t ( :underline nil  :weight bold :slant oblique :foreground nil) )))

   ;; other faces
   '(helm-selection ((t (:background "nil" :foreground "magenta" :background nil))))
   '(lsp-ui-doc-background ((t (:background "lightblue" :foreground "grey" :background nil))))
   )

  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(neotree-width 48)
   )
  )

(pkg-pick-terminal-faces)

;; ************************************************************
;; modify face base on env
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(defun pkg-modify-faces (frame)
  (select-frame frame)
  (progn
    (if (window-system frame)
        (pkg-pick-gui-faces)
      (pkg-pick-terminal-faces)
      )
    )
  )

(add-hook 'after-make-frame-functions 'pkg-modify-faces)

(if (display-graphic-p)
    (pkg-pick-gui-faces)
  (pkg-pick-terminal-faces)
  )

(set-face-background 'vertical-border (face-background 'default))
(set-face-foreground 'vertical-border "grey")

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

;; FEATURE
(set-face-background hl-line-face nil)

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(if (fboundp 'mac-auto-operator-composition-mode)
    (mac-auto-operator-composition-mode))

(provide 'pkg-appearance)
