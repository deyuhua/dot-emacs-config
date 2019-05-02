;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;	Close All Buffers
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(defun pkg-close-all-buffers ()
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;; ************************************************************
;; 	TTS for mac
;; ************************************************************
(defun say (string &optional speed)
  (interactive "MString: ")
  (start-process "say" nil "/usr/bin/say" string))

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; 	"Minifies the buffer contents by removing whitespaces."
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(defun pkg-minify-buffer-contents()
  (interactive)
  (mark-whole-buffer)
  (goto-char (point-min))
  (while (search-forward-regexp "[\s\n]*" nil t) (replace-match "" nil t)))

;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; 	Insert Src Block
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(use-package ido-completing-read+)
(defun pkg-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite" "html")))
     (list (ido-completing-read+ "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))


;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;; 	Mark Current Word Quickly
;; ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
(defun pkg-mark-word ()
  "Select Current Word"
  (interactive)
  (progn
    (backward-word)
    (set-mark (point))
    (forward-word)))

(global-set-key (kbd "M-@") 'pkg-mark-word)

(defun flymake--severity (type)
  "Get the severity for diagnostic TYPE."
  (flymake--lookup-type-property type 'severity
                                 (warning-numeric-level :error)))

(provide 'pkg-functions)
