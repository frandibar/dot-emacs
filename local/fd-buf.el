;;; package --- fd-buf.el

;;; Commentary:
;;; My custom miscellaneous functions

;;; Code:

;;;###autoload
(define-namespace fd-buf-

(defun window-horizontal-to-vertical ()
  "Switch from a horizontal split to a vertical split.

Extracted from URL http://www.emacswiki.org/emacs/Rick_Bielawski#toc5"
  (interactive)
  (let ((one-buf (window-buffer (selected-window)))
        (buf-point (point)))
    (other-window 1)
    (delete-other-windows)
    (split-window-horizontally)
    (switch-to-buffer one-buf)
    (goto-char buf-point)))

(defun window-vertical-to-horizontal ()
  "Switch from a vertical split to a horizontal split."
  (interactive)
  (let ((one-buf (window-buffer (selected-window)))
        (buf-point (point)))
    (other-window 1)
    (delete-other-windows)
    (split-window-vertically)
    (switch-to-buffer one-buf)
    (goto-char buf-point)))

(defun close-buffer-and-window ()
  "Kill buffer and window.  Ask for confirmation if buffer is not associated to a file nor is a dired buffer."
  (interactive)
  (when (or (buffer-file-name)
            (equal major-mode 'dired-mode)
            (yes-or-no-p "Do you wish to kill buffer?"))
    (kill-buffer)
    (delete-window)))

(defun next-user-buffer ()
  "Switch to the next user buffer in cyclic order.
User buffers are those not starting with * nor in `dired-mode'.
Based on http://xahlee.org/emacs/elisp_examples.html"
  (interactive)
  (let ((start-buf (buffer-name)))
    (next-buffer)
    (while (and (or (string-match "^*" (buffer-name))
                    (string-equal "dired-mode" (symbol-name major-mode)))
                (not (string-equal start-buf (buffer-name))))
      (next-buffer))))

(defun previous-user-buffer ()
  "Switch to the previous user buffer in cyclic order.
User buffers are those not starting with * nor in `dired-mode'."
  (interactive)
  (let ((start-buf (buffer-name)))
    (previous-buffer)
    (while (and (or (string-match "^*" (buffer-name))
                    (string-equal "dired-mode" (symbol-name major-mode)))
                (not (string-equal start-buf (buffer-name))))
      (previous-buffer))))

)

(provide 'fd-buf)
;;; fd-buf.el ends here
