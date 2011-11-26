;; configure scheme
(setq scheme-program-name "racket")
(setq auto-mode-alist (cons '("\\.rkt$" . scheme-mode) auto-mode-alist))
;(setq auto-mode-alist (cons '("\.[rkt]\\>" . quack-pltfile-mode) auto-mode-alist))

;; use chrome as default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

(eval-after-load "sql"
  '(progn
     (sql-set-product 'mysql)
     (setq sql-server "localhost")
     (setq sql-user "root")
     (setq sql-password "root")
     (setq sql-database "m2000")
     (sql-connect-mysql)

     ;; TODO fix, not working
     ;; (require 'sql-completion)
     ;; (setq sql-interactive-mode-hook
     ;;       (lambda ()
     ;;         (define-key sql-interactive-mode-map "\t" 'comint-dynamic-complete)
     ;;         (sql-mysql-completion-init)))
  ))
