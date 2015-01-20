;;; package --- fd-vim.el

;;; Commentary:
;;; My custom functions inspired from Vim.

;;; Code:

;;;###autoload
(define-namespace fd-vim-

(defun copy-current-line (arg)
  "Copy and paste ARG lines from point.
If point is on last buffer line, then no newline is inserted."
  (interactive "p")
  (save-excursion
    (kill-whole-line arg)
    (yank)
    (yank)))

(defun fast-buffer-switch ()
  "Switch to last buffer."
  (interactive)
  ;; Ignore if most recent is visible or not.
  (switch-to-buffer (other-buffer (current-buffer) t)))

;; FIXME
(defun current-line-to-clipboard (arg)
  "Copy ARG lines to clipboard.  Default value for ARG is 1.
Similar to 'Y' in vim."
  (interactive "p")
  (message (format "Copied %d line(s) to clipboard" arg))
  (save-excursion
    (copy-region-as-kill (line-beginning-position) (line-beginning-position (+ 1 arg)))))

(defun backward-up-sexp (arg)
  "Replacement for `backward-up-list'.
When point is between double quotes the former doesn't work.
Use ARG as in `backward-up-list'.

Extracted from URL
`http://stackoverflow.com/questions/5194417/how-to-mark-the-text-between-the-parentheses-in-emacs'."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

;; TODO: make it work for other pairs. There must be an existing implementation already!
(defun match-paren ()
  "Go to the matching parenthesis if cursor on a parenthesis."
  (interactive)
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))))


(defun point-to-middle ()
  "Put cursor on top line of window.
Similar to 'M' in vim."
  (interactive)
  (push-mark)
  (move-to-window-line nil))

(defun point-to-top ()
  "Put cursor on top line of window.
Similar to 'H' in vim."
  (interactive)
  (push-mark)
  (move-to-window-line 0))

(defun point-to-bottom ()
  "Put cursor at bottom of last visible line.
Similar to 'L' in vim."
  (interactive)
  (push-mark)
  (move-to-window-line -1))

(defun with-number-at-point (fn n)
  "Helper function for applying FN to number at point with argument N."
  (save-excursion
    (skip-chars-forward "[:alpha:]_")
    (or (looking-at "-?[0-9]+")
        (error "No number at point"))
    (replace-match (number-to-string (funcall fn (string-to-number (match-string 0)) n)))))

(defun increment-number-at-point (&optional n)
  "Increment number at point by N."
  (interactive "p")
  (with-number-at-point #'+ n))

(defun decrement-number-at-point (&optional n)
  "Decrement number at point by N."
  (interactive "p")
  (with-number-at-point #'- n))

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy the current line.

Extracted from URL `http://xahlee.org/emacs/emacs_copy_cut_current_line.html'."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (progn
       (message "Current line is copied.")
       (list (line-beginning-position) (line-beginning-position 2))))))

)

(provide 'fd-vim)
;;; fd-vim.el ends here
