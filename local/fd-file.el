;;; package --- fd-file.el

;;; Commentary:
;;; My custom functions related to file operations

;;; Code:

;;;###autoload
(define-namespace fd-file-

(defun filename-to-clipboard ()
  "Copy the current buffer file name to the clipboard.

Extracted from URL `http://emacsredux.com'."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun dos-to-unix ()
  "Remove all ^M from the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\r" nil t)
      (replace-match ""))))

(defun unix-to-dos ()
  "Convert a buffer from Unix end of lines to DOS `^M' end of lines."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (replace-match "\r\n"))))

(defun hide-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings.
Note: This function overrides variable `buffer-display-table'."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

)

(provide 'fd-file)
;;; fd-file.el ends here
