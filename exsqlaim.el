;;; -*- lexical-binding: t;-*-

;;; exsqlaim.el --- Exsqlaim : Use variables in sql queries

;; Author: Ahmad Nazir Raja <ahmadnazir@gmail.com>
;; Version: 0.0.1

(require 's)
(require 'sql)

;; Variables can be defined as:
;;
;; @db   = `test`
;; @id   =  1234
;; @name = 'John Doe'
;;
(defconst exsqlaim/regexp-stmt-var-assign "^\\(@[^@ ]+\\)[ \t]*=[ \t]*\\(.*\\)$")

;; Inspired and modified from restclient.el: restclient-find-vars-before-point
;;
;;;###autoload
(defun exsqlaim/find-vars-before-point ()
  (let ((vars nil)
        (bound (point)))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp exsqlaim/regexp-stmt-var-assign bound t)
        (let ((name (match-string-no-properties 1))
              (value (match-string-no-properties 2)))
          (setq vars (cons (cons name (message value)) vars))))
      vars)))

;;;###autoload
(defun exsqlaim/get-vars ()
  (cons
   '(";"."\\p;") ;; echo the query to the terminal
   (exsqlaim/find-vars-before-point)))

;; Overriding the sql send region so that I can replace the query params
;;
;;;###autoload
(defun sql-send-region (start end)
  "Send a region to the SQL process."
  (interactive "r")
  (sql-send-string (buffer-substring-no-properties start end)))

;; Modified the original function from sql.el
;;
;;;###autoload
(defun exsqlaim/sql-send-region (start end)
  "Send a region to the SQL process."
  (interactive "r")
  (sql-send-string
   (s-replace-all (exsqlaim/get-vars)
                  (buffer-substring-no-properties start end)))
  )

;; Modified the original function from sql.el
;;
;;;###autoload
(defun exsqlaim/sql-send-paragraph ()
  "Send the current paragraph to the SQL process."
  (interactive)
  (let ((start (save-excursion
                 (backward-paragraph)
                 (point)))
        (end (save-excursion
               (forward-paragraph)
               (point))))
    (exsqlaim/sql-send-region start end)))

;; Highlight variables
(add-hook 'sql-mode-hook
          '(lambda ()
             (font-lock-add-keywords
              'sql-mode
              '(("@[^@= \n\"'\.]+" . font-lock-variable-name-face)))
             ))

(provide 'exsqlaim)

;;; exsqlaim.el ends here
