;; My custom miscellaneous functions
(provide 'myfuncs)

(defun dip ()
  "Kill text inside parenthesis. Same as vim's di) command. It doesn't work if cursor is between double quotes."
  (interactive)
  (backward-up-sexp nil)
  (kill-sexp)
  (insert-parentheses))

(defun vap ()
  "Select text inside parenthesis (including parenthesis). Same as vim's va) command. It doesn't work if cursor is between double quotes."
  (interactive)
  (backward-up-sexp nil)
  (mark-sexp))

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

;; jump the cursor to the top of the window, and the bottom, 
;; like H and L vim commands

(defun point-to-top ()
  "Put cursor on top line of window, like Vi's H."
  (interactive)
  (move-to-window-line 0))

(defun point-to-bottom ()
  "Put cursor at bottom of last visible line, like Vi's L."
  (interactive)
  (move-to-window-line -1))

(defun insert-date()
  (interactive)
  (insert (format-time-string "%a %b %d, %Y")))
