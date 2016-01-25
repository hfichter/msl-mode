;;; msl-mode.el --- sample major mode for editing MSL

;; Author: Hugo Fichter 
;; Version: 1.0
;; Created: Jan 2016
;; Keywords: languages
;; Homepage: https://github.com/hfichter/msl-mode

;; This file is not part of GNU Emacs.

;; Murex Scripting Language (MSL) mode for Emacs

;;; Code:

;; Define several category of keywords
(setq msl-keywords '("IF" "THEN" "ELSE" "WHILE"))
(setq msl-constants '("TRUE" "FALSE"))
(setq msl-boolean-operators '("NOT" "AND" "OR"))

;; TODO: load funciton names in this version
;; (load-file "msl-functions.el")

;; generate regex string for each category of keywords
(setq msl-keywords-regexp (regexp-opt msl-keywords 'words))
(setq msl-constant-regexp (regexp-opt msl-constants 'words))
(setq msl-boolean-operators-regexp (regexp-opt msl-boolean-operators 'words))
;;(setq msl-functions-regexp (regexp-opt msl-functions 'words))

;; create the list for font-lock.
;; each category of keyword is given a particular face
(setq msl-font-lock-keywords
      `(
        (,msl-constant-regexp .  font-lock-constant-face)
        (,msl-boolean-operators-regexp .  msl-face-syntax)
	;; (,msl-functions-regexp . font-lock-function-name-face)
        (,msl-keywords-regexp . msl-face-keywords)
        ))
;; set syntax table to detect C and C++ styles comments
(defvar msl-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 14" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\r "> b" table)
    table)
  "Syntax table in use in `msl-mode' buffers.")

;;;###autoload
(define-derived-mode msl-mode fundamental-mode
  "MSL mode"
  "Major mode for editing MSL (Murex Scripting Language)"
  ;; Load MSL custom faces
  (defvar msl-face-syntax 'msl-face-syntax)
  (defvar msl-face-comment 'msl-face-comment)
  (defvar msl-face-string 'msl-face-string)
  (defvar msl-face-keywords 'msl-face-keywords)
  (defface msl-face-syntax
    '((((class color))
       :foreground "red1"))
    "Face used for MSL syntax: := , {} () "
    :group 'msl-faces)
  (defface msl-face-comment
    '((((class color))
       :foreground "SpringGreen4"))
    "Face used for MSL comments"
    :group 'msl-faces)
  (defface msl-face-string
    '((((class color))
       :foreground "DeepSkyBlue4"
       :weight bold))
    "Face used for MSL strings"
    :group 'msl-faces)
  (defface msl-face-keywords
    '((((class color))
       :foreground "blue1"
       :weight bold))
    "Face used for MSL keywords"
    :group 'msl-faces)
  ;; set  MSL syntax table
  ;; to detect C and C++ style comments
  (set-syntax-table msl-mode-syntax-table)
  ;; set face for comments and strings
  (make-local-variable 'font-lock-comment-face)
  (make-local-variable 'font-lock-string-face)
  (setq font-lock-comment-face msl-face-comment)
  (setq font-lock-string-face msl-face-string)
  ;; code for syntax highlighting for MSL keywords and const
  (setq font-lock-defaults '((msl-font-lock-keywords)))
  ;; additonal keywords for MSL separators 
  (font-lock-add-keywords nil
			  ;;(regexp-opt '("," ":" "=" "{" "}" "(" ")"))
			  '(("[(),:={}]" . msl-face-syntax))))

;; clear memory. no longer needed
(setq msl-keywords nil)
(setq msl-constants nil)
;;(setq msl-functions nil)

;; clear memory. no longer needed
(setq msl-keywords-regexp nil)
(setq msl-constants-regexp nil)
;;(setq msl-functions-regexp nil)

;; add the mode to the `features' list
(provide 'msl-mode)

;; Local Variables:
;; coding: utf-8
;; End:

;; msl-mode.el ends here
