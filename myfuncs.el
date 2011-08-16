;; My custom miscellaneous functions
(provide 'myfuncs)

(defun dip ()
  "Kill text inside parenthesis.
Similar to di) in vim.
It doesn't work if cursor is between double quotes."
  (interactive)
  (backward-up-sexp nil)
  (kill-sexp)
  (insert-parentheses))

(defun vip ()
  "Mark text inside parenthesis (excluding parenthesis). 
Similar to vi) in vim. 
It doesn't work if cursor is between double quotes."
  (interactive)
  (backward-up-sexp nil)
  (mark-sexp)
  (forward-char)
  (exchange-point-and-mark)
  (backward-char))

(defun vap ()
  "Mark text inside parenthesis (including parenthesis). 
Similar to va) in vim. 
It doesn't work if cursor is between double quotes."
  (interactive)
  (backward-up-sexp nil)
  (mark-sexp)
  (exchange-point-and-mark))

(defun match-paren (arg)
  "Go to the matching parenthesis if cursor on a parenthesis; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;; scroll the window without moving the cursor

(defun scroll-n-lines-up (&optional n)
  "Scroll ahead N lines (1 by default)."
  (interactive "P")
  (scroll-up (prefix-numeric-value n)))

(defun scroll-n-lines-down (&optional n)
  "Scroll behind N lines (1 by default)."
  (interactive "P")
  (scroll-down (prefix-numeric-value n)))

(defun toggle-fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))
; disabled because sometimes leaves a status bar with too many lines
;(toggle-fullscreen)

(defun point-to-top ()
  "Put cursor on top line of window.
Similar to 'H' in vim."
  (interactive)
  (move-to-window-line 0))

(defun point-to-bottom ()
  "Put cursor at bottom of last visible line.
Similar to 'L' in vim."
  (interactive)
  (move-to-window-line -1))

(defun insert-date()
  (interactive)
  (insert (format-time-string "%a %b %d, %Y")))

(defun advance-to (arg char)
  "Advance cursor to ARGth CHAR if it exists, if not, do nothing.
Similar to 'f' in vim.
Case sensitiveness depends on `case-fold-search'.
TODO:
 - restrict to current line.
 - if already on CHAR, continue search."
  (interactive "p\ncAdvance to char: ")
  (let ((curr (point))
        (end (move-end-of-line 1)))
  (save-restriction
    (narrow-to-region curr end)
    (progn
      (goto-char curr)
      (if (search-forward (char-to-string char) end nil arg)
          (backward-char))))))

(defun back-to (arg char)
  "Take cursor back to ARGth CHAR if it exists, if not, do nothing.
Similar to 'F' in vim.
Case sensitiveness depends on `case-fold-search'.
TODO:
 - restrict to current line."
  (interactive "p\ncGo back to char: ")
  (search-backward (char-to-string char) nil t arg))

(defun hide-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings.
Note: This function overrides variable `buffer-display-table'."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun switch-cpp-h-file ()
  "Switches buffer to the corresponding header file (.h) if current buffer
is a .cpp file, and vice-versa.
It assumes both files are in the same path. If not, it creates a new file."
  (interactive)
  (defun alternate-file (cpp-or-h-file)
    (cond ((equal ".h" (substring cpp-or-h-file -2))
           (concat (substring cpp-or-h-file 0 (- (length cpp-or-h-file) 2)) ".cpp"))
          ((equal ".cpp" (substring cpp-or-h-file -4))
           (concat (substring cpp-or-h-file 0 (- (length cpp-or-h-file) 4)) ".h"))))
  (find-file (alternate-file (buffer-file-name (current-buffer)))))

(defun fast-buffer-switch ()
  "Switch to last buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))

(defun point-to-eol ()
  "Move point to end of line.
Similar to '$' in vim."
  (interactive)
  (next-line)
  (beginning-of-line)
  (backward-char))