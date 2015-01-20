;;; package --- fd-py.el

;;; Commentary:
;;; My custom Python related functions.

;;; Code:

;;;###autoload
(define-namespace fd-py-

(defun point-in-string-p (pt)
  "Returns t if PT is in a string
Works in strings enclosed in single quotes, as opposed to `in-string-p'
Extracted from URL `http://www.masteringemacs.org/articles/2014/08/26/swapping-quote-symbols-emacs-parsepartialsexp/'"
  (eq 'string (syntax-ppss-context (syntax-ppss pt))))

(defun beginning-of-string ()
  "Moves to the beginning of a syntactic string
Extracted from URL `http://www.masteringemacs.org/articles/2014/08/26/swapping-quote-symbols-emacs-parsepartialsexp/'"
  (interactive)
  (unless (point-in-string-p (point))
    (error "You must be in a string for this command to work"))
  (while (point-in-string-p (point))
    (forward-char -1))
  (point))

(defun swap-quotes ()
  "Swap the quote symbols in a \\[python-mode] string.
Extracted from URL `http://www.masteringemacs.org/articles/2014/08/26/swapping-quote-symbols-emacs-parsepartialsexp/'"
  (interactive)
  (save-excursion
    (let ((bos (save-excursion
                 (beginning-of-string)))
          (eos (save-excursion
                 (beginning-of-string)
                 (forward-sexp)
                 (point)))
          (replacement-char ?\'))
      (goto-char bos)
      ;; if the following character is a single quote then the
      ;; `replacement-char' should be a double quote.
      (when (eq (following-char) ?\')
        (setq replacement-char ?\"))
      (delete-char 1)
      (insert replacement-char)
      (goto-char eos)
      (delete-char -1)
      (insert replacement-char))))

(defun comment-tests ()
  "Comment tests."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil))       ; case-sensitive
      (while (search-forward "def test" nil t)
        (replace-match "def XXXtest")))
    ))

(defun uncomment-tests ()
  "Uncomment tests."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil))       ; case-sensitive
      (while (search-forward "def XXXtest" nil t)
        (replace-match "def test")))
    ))

(defun pdb-break ()
  "Insert pdb break."
  (interactive)
  (insert "import pdb;pdb.set_trace()  # TODO remove\n"))

;; Toggle between python buffers and python shell
;; Extracted from
;; http://www.masteringemacs.org/articles/2011/02/23/toggling-python-buffers/

(defvar python-last-buffer nil
  "Name of the Python buffer that last invoked `fd-py-toggle-between-python-buffers'.")

(make-variable-buffer-local 'python-last-buffer)

(defun toggle-between-python-buffers ()
  "Toggle between a `python-mode' buffer and its inferior Python process.

When invoked from a `python-mode' buffer it will switch the
active buffer to its associated Python process.  If the command is
invoked from a Python process, it will switch back to the `python-mode' buffer."
  (interactive)
  ;; check if `major-mode' is `python-mode' and if it is, we check if
  ;; the process referenced in `python-buffer' is running
  (if (and (eq major-mode 'python-mode)
           (processp (get-buffer-process python-buffer)))
      (progn
        ;; store a reference to the current *other* buffer; relying
        ;; on `other-buffer' alone wouldn't be wise as it would never work
        ;; if a user were to switch away from the inferior Python
        ;; process to a buffer that isn't our current one.
        (switch-to-buffer python-buffer)
        (setq python-last-buffer (other-buffer)))
    ;; switch back to the last `python-mode' buffer, but only if it
    ;; still exists.
    (when (eq major-mode 'inferior-python-mode)
      (if (buffer-live-p python-last-buffer)
          (switch-to-buffer python-last-buffer)
        ;; buffer's dead; clear the variable.
        (setq python-last-buffer nil)))))

(define-key inferior-python-mode-map (kbd "<f10>") 'toggle-between-python-buffers)
(define-key python-mode-map (kbd "<f10>") 'toggle-between-python-buffers)

)

(provide 'fd-py)
;;; fd-py.el ends here
