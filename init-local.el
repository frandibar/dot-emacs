;; configure scheme
(setq scheme-program-name "racket")
(setq auto-mode-alist (cons '("\\.rkt$" . scheme-mode) auto-mode-alist))
;(setq auto-mode-alist (cons '("\.[rkt]\\>" . quack-pltfile-mode) auto-mode-alist))

;; use chrome as default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;; configure connection to mysql
(eval-after-load "sql"
  '(progn
     (sql-set-product 'mysql)
     (setq sql-server "localhost")
     (setq sql-user "")
     (setq sql-password "")
     (setq sql-database "")
     (sql-connect-mysql)
     (defalias 'sql-get-login 'ignore)

     ;; TODO fix, not working
     ;; (require 'sql-completion)
     ;; (setq sql-interactive-mode-hook
     ;;       (lambda ()
     ;;         (define-key sql-interactive-mode-map "\t" 'comint-dynamic-complete)
     ;;         (sql-mysql-completion-init)))

     ;; When using sql-send-region to execute a query in a SQLi buffer, the table formatting is off because the column names are printed on the same row as the the prompt. 
     ;; By adding a newline before the comint output we can make sure everything lines up nice. 
     ;; This will add a preceding newline to every comint output, even queries run at the prompt - though the extra line isn’t too noticeable.
     (defun sql-add-newline-first (output)
       "Add newline to beginning of OUTPUT for `comint-preoutput-filter-functions'"
       (concat "\n" output))
 
     (defun sqli-add-hooks ()
       "Add hooks to `sql-interactive-mode-hook'."
       (add-hook 'comint-preoutput-filter-functions
                 'sql-add-newline-first))
 
     (add-hook 'sql-interactive-mode-hook 'sqli-add-hooks)
  ))



