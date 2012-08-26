;; This function was extracted from
;; https://github.com/technomancy/emacs-starter-kit/blob/v2/starter-kit-defuns.el
(defun esk-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(provide 'starter-kit-defuns)
