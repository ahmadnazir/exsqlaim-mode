;;; exsqlaim.el --- Use variables inside sql queries
;;; -*- lexical-binding: t;-*-

;; Author: Ahmad Nazir Raja <ahmadnazir@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((s "1.10.0"))
;; URL: https://github.com/ahmadnazir/exsqlaim-mode

;;; Commentary:
;;
;; Variables can be defined as:
;;
;; @db   = `test`
;; @id   =  1234
;; @name = "John Doe"
;;
;; and the query can be:
;;
;; SELECT * FROM @db.users WHERE id = @id OR name = @name;
;;
;; NOTE: The parameters are not escaped as the values are define by the user and
;; hence it is a trusted source. However, functionality for evaluating a value
;; will probably be added in the future which would require the values to be
;; escaped (hence, a breaking change is expected in the future).

(require 's)
(require 'sql)

;;; Code:

(defconst exsqlaim-regexp-stmt-var-assign "^\\(@[^@ ]+\\)[ \t]*=[ \t]*\\(.*\\)$")

;; Inspired and modified from restclient.el: restclient-find-vars-before-point
;;
;;;###autoload
(defun exsqlaim-find-vars-before-point ()
  "Find the mapping between variables and their values before point."
  (let ((vars nil)
        (bound (point)))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp exsqlaim-regexp-stmt-var-assign bound t)
        (let ((name (match-string-no-properties 1))
              (value (match-string-no-properties 2)))
          (setq vars (cons (cons name value) vars))))
      vars)))

;;;###autoload
(defun exsqlaim-get-vars ()
  "Get a map of all variables and values."
  (cons
   '(";"."\\p;") ;; echo the query to the terminal
   (exsqlaim-find-vars-before-point)))

;;;###autoload
(defun exsqlaim-get-raw-query (start end)
  "Get the raw query with variables.
Argument START Point where the query starts.
Argument END Point where the query ends."
  (interactive "r")
  (buffer-substring-no-properties start end))

;;;###autoload
(defun exsqlaim-build-query (query vars)
  "Build the sql QUERY using defined variables.
Argument VARS Map of variables and values."
  (s-replace-all vars query))

;; Modified the original function from sql.el
;;
;;;###autoload
(defun exsqlaim-build-query-at-point()
  "Build the query to be executed at point"
  (let ((start (save-excursion
                 (backward-paragraph)
                 (point)))
        (end (save-excursion
               (forward-paragraph)
               (point))))
    (exsqlaim-build-query (exsqlaim-get-raw-query start end) (exsqlaim-get-vars))
    ))

;;;###autoload
(defun exsqlaim-update-query-at-point ()
  "Update the query at point with the values from the variables."
  (interactive)
  (let ((start (save-excursion
                 (backward-paragraph)
                 (point)))
        (end (save-excursion
               (forward-paragraph)
               (point))))
    (let ((query (exsqlaim-build-query-at-point)))
      (kill-region start end)
      (insert query))))

;;;###autoload
(defun exsqlaim-send ()
  "Build a query at point and send it to the sql process."
  (interactive)
  (sql-send-string (exsqlaim-build-query-at-point))
  )

;; Enable exsqlaim mode with sql mode
(add-hook 'sql-mode-hook 'exsqlaim-mode)

;; Minor Mode
(defvar exsqlaim-mode-map (make-sparse-keymap)
  "Exsqlaim-mode keymap.")

(define-key exsqlaim-mode-map
  (kbd "C-c C-c") 'exsqlaim-send)

(define-key exsqlaim-mode-map
  (kbd "C-c C-i") 'exsqlaim-update-query-at-point)

(define-minor-mode exsqlaim-mode
  "Exsqlaim mode" nil " Exsqlaim" exsqlaim-mode-map
  (if exsqlaim-mode
      ;; Highlight variables
      (font-lock-add-keywords
       'sql-mode
       '(("@[^@= \n\"'\.]+" . font-lock-variable-name-face)))
    (message "Exsqlaim minor mode disabled"))
  )

(provide 'exsqlaim)

;;; exsqlaim.el ends here
