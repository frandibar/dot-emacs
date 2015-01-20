;;; package --- fd-eshell.el

;;; Commentary:
;;; My custom miscellaneous functions

;; Package-Requires: ((names "0.5") (emacs "24") (cl-lib "1.0") (helm "20141210.919"))

;;; Code:

;;;###autoload
(define-namespace fd-eshell-

(defun eshell-kill-line ()
  "Kill line in eshell."
  (interactive)
  (when (fboundp 'eshell-bol)
    (eshell-bol)
    (kill-line)))

;; Must start with `eshell/' so it can be called as `clear' from the eshell prompt.
(defun ::eshell/clear ()
  "Clears the eshell buffer.
Extracted from URL `http://www.khngai.com/emacs/eshell.php'."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun ::eshell-prompt-function-short ()
  "Make a short eshell prompt to avoid moving out of the buffer window boundary."
  (when (featurep 'eshell/pwd)
    (let* ((pwd (eshell/pwd))
           (pwdlst (split-string pwd "/"))
           (rpwdlst (reverse pwdlst))
           (base (car rpwdlst)))
      (concat (if (string= base "")
                  "/"
                (if (cdr pwdlst) "<...> /" ""))
              base
              (if (= (user-uid) 0) " # " " $ ")))))

(defun ::eshell-prompt-function-long ()
  "Make a long standard eshell prompt."
  (when (featurep 'eshell/pwd)
    (concat (abbreviate-file-name (eshell/pwd))
            (if (= (user-uid) 0) " # " " $ "))))

(setq eshell-prompt-function 'eshell-prompt-function-long)

(defun ::eshell/sprompt ()
  "Make a short eshell prompt to avoid moving out of the buffer
window boundary (link to `eshell-prompt-function-short')."
  (when (boundp 'eshell-prompt-function)
    (setq eshell-prompt-function 'eshell-prompt-function-short)))

(defun ::eshell/lprompt ()
  "Make a long standard eshell prompt (link to `eshell-prompt-function-long')."
  (when (boundp 'eshell-prompt-function)
    (setq eshell-prompt-function 'eshell-prompt-function-long)))

(defun move-dir-to-trash (dir)
  (let ((flag delete-by-moving-to-trash))
    (setq delete-by-moving-to-trash t)
    (delete-directory dir t t) ; recursive delete to trash
    (setq delete-by-moving-to-trash flag)))

(defun eshell/trash (&rest args)
  (dolist (name args)
    (if (file-directory-p name)
        (move-dir-to-trash name)
      (move-file-to-trash name))))

)

(provide 'fd-eshell)
;;; fd-eshell.el ends here
