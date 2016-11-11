;;; msl-mode.el --- Major mode for editing MSL

;; Author: Hugo Fichter 
;; Version: 1.1
;; Created: Jan 2016
;; Last modified: Feb 2016
;; Keywords: languages
;; Homepage: https://github.com/hfichter/msl-mode

;; This file is not part of GNU Emacs.

;; Murex Scripting Language (MSL) mode for Emacs

;;; Code:

;; Define several category of keywords
(defvar msl-mode-keywords '("IF" "THEN" "ELSE" "WHILE")
  "MSL language syntax keywords")
(defvar msl-mode-constants '("TRUE" "FALSE")
  "MSL language constants (booleans)")
(defvar msl-mode-boolean-operators '("NOT" "AND" "OR")
  "MSL language boolean operators")

;; TODO: load funciton names in this version
;; (load-file "msl-functions.el")

;; generate regex string for each category of keywords
(setq msl-keywords-regexp (regexp-opt msl-mode-keywords 'words))
(setq msl-constant-regexp (regexp-opt msl-mode-constants 'words))
(setq msl-boolean-operators-regexp (regexp-opt msl-mode-boolean-operators 'words))
;;(setq msl-functions-regexp (regexp-opt msl-functions 'words))

;; create the list for font-lock.
;; each category of keyword is given a particular face
(defvar msl-mode-font-lock-keywords
      `(
        (,msl-constant-regexp .  font-lock-constant-face)
        (,msl-boolean-operators-regexp .  'msl-face-syntax)
	;; (,msl-functions-regexp . font-lock-function-name-face)
        (,msl-keywords-regexp . 'msl-face-keywords)
        ))

;; Load MSL custom faces
(defface msl-face-syntax
  '((((class color))
     :foreground "red1"))
  "Face used for MSL syntax: := , {} () "
  :group 'msl-faces)
(defface msl-face-comment
  '((((class color))
     :foreground "green3"))
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

;;;###autoload
(define-derived-mode msl-mode prog-mode
  "MSL mode"
  "Major mode for editing Murex Scripting Language files"
  ;; set  MSL syntax table
  ;; to detect C and C++ style comments
  ;;  (set-syntax-table msl-mode-syntax-table)
  (modify-syntax-entry ?/ ". 14" msl-mode-syntax-table)
  (modify-syntax-entry ?* ". 23" msl-mode-syntax-table)
  (modify-syntax-entry ?/ ". 124b" msl-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" msl-mode-syntax-table)
  (modify-syntax-entry ?\r "> b" msl-mode-syntax-table)
  ;; set face for comments and strings
  (make-local-variable 'font-lock-comment-face)
  (make-local-variable 'font-lock-string-face)
  (setq font-lock-comment-face 'msl-face-comment)
  (setq font-lock-string-face 'msl-face-string)
  ;; code for syntax highlighting for MSL keywords and const
  (setq font-lock-defaults '((msl-mode-font-lock-keywords)))
  ;; additonal keywords for MSL separators 
  (font-lock-add-keywords nil
			  ;;(regexp-opt '("," ":" "=" "{" "}" "(" ")"))
			  '(("[(),:={}]" . 'msl-face-syntax)))
  ;; Default MSL mode bindings
  (local-set-key (kbd "C-c f")  'msl-current-function-name))

;;(setq msl-functions nil)

;; clear memory. no longer needed, regexp
(makunbound 'msl-keywords-regexp)
(makunbound 'msl-boolean-operators-regexp)
(makunbound 'msl-constant-regexp)

;; add the mode to the `features' list
(provide 'msl-mode)

;; Custom functions
(defun msl-current-function-name ()
  (interactive)
  (save-excursion
    (search-backward-regexp "    //FILE BEING EXECUTED : ")
    (message (buffer-substring-no-properties (match-end 0) (line-end-position)))))

(defun msl-list-all-functions ()
  (interactive)
  (save-excursion
    (let ((res "" ))
      (beginning-of-buffer)
      (while (search-forward-regexp "    //FILE BEING EXECUTED : " nil t nil) 
	(setq res (concat
		   res
		   (buffer-substring-no-properties
			       (match-end 0)
			       (line-end-position))
		   "\n"))
	)
      (message (concat "msg: " res))
      (switch-to-buffer "*MSL output*")
      (erase-buffer)
      (insert res)
      (beginning-of-buffer)
      )))

;; msl-mode.el ends here
