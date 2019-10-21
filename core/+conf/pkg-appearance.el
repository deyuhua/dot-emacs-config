;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;	coding theme settings
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; Code:

(if (not (display-graphic-p))
    (progn
      (add-to-list 'load-path (expand-file-name "~/.emacs.d/themes"))
      (require 'doom-dracula-alt)
      )
  (load-theme 'doom-dracula)
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
   '(flycheck-error ((t (:background "violet" :weight bold :foreground "white") )))
   '(flycheck-warning ((t (:background "yellow" :weight bold :foreground "white") )))
   '(flycheck-info ((t (:background "green" :weight bold :foreground "white") )))
   
   '(flymake-error ((t (:underline nil  :weight bold :style italic :foreground nil) )))
   '(flymake-warning ((t (:underline nil  :weight bold :style italic :foreground nil) )))
   '(flymake-info ((t ( :underline nil  :weight bold :style italic :foreground nil) )))
   '(flymake-note ((t ( :underline nil  :weight bold :style italic :foreground nil) )))
   )
  )

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

(provide 'pkg-appearance)
