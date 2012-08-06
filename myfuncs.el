;; My custom miscellaneous functions
(provide 'myfuncs)

(defun mine-find-file-in-this-dir ()
  "find-file in the dir of current buffer."
  (interactive)
  (ido-find-file-in-dir (file-name-directory (buffer-file-name))))

(defun mine-backward-up-sexp (arg)
  "Added because existing function backward-up-list won't work when point is between double quotes.

Extracted from
http://stackoverflow.com/questions/5194417/how-to-mark-the-text-between-the-parentheses-in-emacs
"
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (mine-backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(defun mine-dip ()
  "Kill text inside parenthesis.
Similar to di) in vim.
It doesn't work if cursor is between double quotes."
  (interactive)
  (mine-backward-up-sexp nil)
  (kill-sexp)
  (insert-parentheses))

(defalias 'dip 'mine-dip)

(defun mine-vip ()
  "Mark text inside parenthesis (excluding parenthesis).
Similar to vi) in vim.
It doesn't work if cursor is between double quotes."
  (interactive)
  (mine-backward-up-sexp nil)
  (mark-sexp)
  (forward-char)
  (exchange-point-and-mark)
  (backward-char))

(defalias 'vip 'mine-vip)

(defun mine-vap ()
  "Mark text inside parenthesis (including parenthesis).
Similar to va) in vim.
It doesn't work if cursor is between double quotes."
  (interactive)
  (mine-backward-up-sexp nil)
  (mark-sexp)
  (exchange-point-and-mark))

(defalias 'vap 'mine-vap)

(defun mine-match-paren (arg)
  "Go to the matching parenthesis if cursor on a parenthesis; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

;; scroll the window without moving the cursor

(defun mine-scroll-n-lines-up (&optional n)
  "Scroll ahead N lines (1 by default)."
  (interactive "p")
  (scroll-up n))

(defun mine-scroll-n-lines-down (&optional n)
  "Scroll behind N lines (1 by default)."
  (interactive "p")
  (scroll-down n))

(defun mine-toggle-fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))
; disabled because sometimes leaves a status bar with too many lines
;(toggle-fullscreen)

(defun mine-point-to-middle ()
  "Put cursor on top line of window.
Similar to 'M' in vim."
  (interactive)
  (push-mark)
  (move-to-window-line nil))

(defun mine-point-to-top ()
  "Put cursor on top line of window.
Similar to 'H' in vim."
  (interactive)
  (push-mark)
  (move-to-window-line 0))

(defun mine-point-to-bottom ()
  "Put cursor at bottom of last visible line.
Similar to 'L' in vim."
  (interactive)
  (push-mark)
  (move-to-window-line -1))

(defun mine-insert-date()
  (interactive)
  (insert (format-time-string "%a %b %d, %Y")))

(defun mine-back-to (arg char)
  "Take cursor back to ARGth CHAR if it exists, if not, do nothing.
Similar to 'F' in vim.
Case sensitiveness depends on `case-fold-search'."
  (interactive "p\ncGo back to char: ")
  (let ((curr (point))
        end)
    (move-beginning-of-line 1)
    (setq end (point))
    (goto-char curr)  ; because we moved to beginning of line
    (search-backward (char-to-string char) end t arg)))

(defun mine-advance-to (arg char)
  "Advance cursor to ARGth CHAR in current line if it exists, if not, do nothing.
Similar to 'f' in vim.
Case sensitiveness depends on `case-fold-search'.
"
  (interactive "p\ncAdvance to char: ")
  (let ((curr (point))
        end)
    (move-end-of-line 1)
    (setq end (point))
    (goto-char (1+ curr))  ; because we moved to end of line
    (if (= curr end)
        (backward-char)
      (progn (search-forward (char-to-string char) end t arg)
             (backward-char)))))      ; since point was set after the ocurrence

(defun mine-hide-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings.
Note: This function overrides variable `buffer-display-table'."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun mine-switch-cpp-h-file ()
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

(defun mine-fast-buffer-switch ()
  "Switch to last buffer."
  (interactive)
  ;; (switch-to-buffer (other-buffer)))    ; switch to most recent non visible buffer
  (switch-to-buffer (other-buffer (current-buffer) t))) ; ignore if most recent is visible or not

;; commented out due to move-end-of-line with C-e
;; (defun mine-point-to-eol ()
;;   "Move point to end of line.
;; Similar to '$' in vim."
;;   (interactive)
;;   (next-line)
;;   (beginning-of-line)
;;   (backward-char))

;; TODO: extend to work if point not over number, like in vim
;; Based on http://www.emacswiki.org/emacs/IncrementNumber
;; See also http://www.emacswiki.org/emacs/IntegerAtPoint
(defun mine-with-number-at-point (fn n)
  (save-excursion
    (skip-chars-backward "-0123456789")
    (or (looking-at "-?[0-9]+")
        (error "No number at point"))
    (replace-match (number-to-string (funcall fn (string-to-number (match-string 0)) n)))))

(defun mine-increment-number-at-point (&optional n)
  (interactive "p")
  (mine-with-number-at-point '+ n))

(defun mine-decrement-number-at-point (&optional n)
  (interactive "p")
  (mine-with-number-at-point '- n))

(defun mine-copy-current-line (arg)
  "Copy and paste ARG lines from point.
If point is on last buffer line, then no newline is inserted."
  (interactive "p")
  (save-excursion
    (kill-whole-line arg)
    (yank)
    (yank)))

(defun mine-current-line-to-clipboard (arg)
  "Copy ARG lines to clipboard. Default value for ARG is 1.
Similar to 'Y' in vim."
  (interactive "p")
  (message (format "Copied %d line(s) to clipboard" arg))
  (save-excursion
    (copy-region-as-kill (line-beginning-position) (line-beginning-position (+ 1 arg)))))

(require 'highlight-symbol)

(defun mine-hl-symbol-and-jump-next ()
  "Search for next occurance of symbol under cursor, with highlight.
Similar to '*' in vim, except that the highlighting is preserved on next search."
  (interactive)
  (mine-hl-symbol-and-jump 'highlight-symbol-next))

(defun mine-hl-symbol-and-jump-prev ()
  (interactive)
  (mine-hl-symbol-and-jump 'highlight-symbol-prev))

(defun mine-hl-symbol-and-jump (fn-next-or-prev)
  "Search for previous occurance of symbol under cursor, with highlight.
Similar to '#' in vim, except that the highlighting is preserved on next search."
  (let ((symbol (highlight-symbol-get-symbol)))
    (unless symbol (error "No symbol at point"))
    (unless hi-lock-mode (hi-lock-mode 1))
    (if (member symbol highlight-symbol-list)
        (funcall fn-next-or-prev)
      (highlight-symbol-at-point)
      (funcall fn-next-or-prev))))

(defun mine-hl-symbol-cleanup ()
  "Clear all highlighted symbols.
Taken from http://www.emacswiki.org/emacs/SearchAtPoint."
  (interactive)
  (mapc 'hi-lock-unface-buffer highlight-symbol-list)
  (setq highlight-symbol-list ()))

;; Search at point, similar to * in vim
;; http://www.emacswiki.org/emacs/SearchAtPoint
;; I-search with initial contents
(defvar mine-isearch-initial-string nil)

(defun mine-isearch-set-initial-string ()
  (remove-hook 'isearch-mode-hook 'mine-isearch-set-initial-string)
  (setq isearch-string mine-isearch-initial-string)
  (isearch-search-and-update))

(defun mine-isearch-forward-at-point (&optional regexp-p no-recursive-edit)
  "Interactive search forward for the symbol at point."
  (interactive "P\np")
  (if regexp-p (isearch-forward regexp-p no-recursive-edit)
    (let* ((end (progn (skip-syntax-forward "w_") (point)))
           (begin (progn (skip-syntax-backward "w_") (point))))
      (if (eq begin end)
          (isearch-forward regexp-p no-recursive-edit)
        (setq mine-isearch-initial-string (buffer-substring begin end))
        (add-hook 'isearch-mode-hook 'mine-isearch-set-initial-string)
        (isearch-forward regexp-p no-recursive-edit)))))

;; http://www.emacswiki.org/emacs/Rick_Bielawski#toc5
;; Idea and starter code from Benjamin Rutt (rutt.4+news@osu.edu) on comp.emacs
(defun mine-window-horizontal-to-vertical ()
  "Switches from a horizontal split to a vertical split."
  (interactive)
  (let ((one-buf (window-buffer (selected-window)))
        (buf-point (point)))
    (other-window 1)
    (delete-other-windows)
    (split-window-horizontally)
    (switch-to-buffer one-buf)
    (goto-char buf-point)))

;; complement of above created by rgb 11/2004
(defun mine-window-vertical-to-horizontal ()
  "Switches from a vertical split to a horizontal split."
  (interactive)
  (let ((one-buf (window-buffer (selected-window)))
        (buf-point (point)))
    (other-window 1)
    (delete-other-windows)
    (split-window-vertically)
    (switch-to-buffer one-buf)
    (goto-char buf-point)))

;; extracted from http://xahlee.org/emacs/modernization_mark-word.html
;; by Nikolaj Schumacher, 2008-10-20. Released under GPL.
(defun mine-semnav-up (arg)
  (interactive "p")
  (when (nth 3 (syntax-ppss))
    (if (> arg 0)
        (progn
          (skip-syntax-forward "^\"")
          (goto-char (1+ (point)))
          (decf arg))
      (skip-syntax-backward "^\"")
      (goto-char (1- (point)))
      (incf arg)))
  (up-list arg))

;; by Nikolaj Schumacher, 2008-10-20. Released under GPL.
(defun mine-extend-selection (arg &optional incremental)
  "Select the current word.
Subsequent calls expands the selection to larger semantic unit."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (or (region-active-p)
                         (eq last-command this-command))))
  (if incremental
      (progn
        (semnav-up (- arg))
        (forward-sexp)
        (mark-sexp -1))
    (if (> arg 1)
        (mine-extend-selection (1- arg) t)
      (if (looking-at "\\=\\(\\s_\\|\\sw\\)*\\_>")
          (goto-char (match-end 0))
        (unless (memq (char-before) '(?\) ?\"))
          (forward-sexp)))
      (mark-sexp -1))))

(defun mine-select-text-in-quote ()
  "Select text between the nearest left and right delimiters.
Delimiters are paired characters:
 () [] {} «» ‹› “” 〖〗 【】 「」 『』 （） 〈〉 《》 〔〕 ⦗⦘ 〘〙

For practical purposes, it also includes double straight quote
\", but not curly single quote matching pairs ‘’, because that is
often used as apostrophy. It also consider both left and right
angle brackets <> as either beginning or ending pair, so that it
is easy to get content inside HTML tags."
 (interactive)
 (let (b1 b2)
   (skip-chars-backward "^<>([{“「『‹«（〈《〔【〖⦗〘\"")
   (setq b1 (point))
   (skip-chars-forward "^<>)]}”」』›»）〉》〕】〗⦘〙\"")
   (setq b2 (point))
   (set-mark b1)
   ))

(defun mine-close-buffer-and-window ()
  (interactive)
  (kill-buffer)
  (delete-window))


(defun mine-enclose-quotes (start end)
  "Insert double quotes around a region."
  (interactive "r")
  (save-excursion
    (goto-char end) (insert-char ?" 1)
    (goto-char start) (insert-char ?" 1)
    ))

(defun mine-enclose-parens (start end)
  "Insert parenthesis around a region."
  (interactive "r")
  (save-excursion
    (goto-char end) (insert-char ?) 1)
    (goto-char start) (insert-char ?( 1)
    ))

(defun mine-replace-enclosing-char (old new)
  "Replace the enclosing OLD char with NEW.
The cursor must be located in between the enclosing chars. For empty strings, cursor should be on closing pair."
  (interactive "cEnclosing char to replace: \ncNew enclosing char: ")

  (defun opener (char)
    (cond ((member char '(?\( ?\) )) ?\()
          ((member char '(?\[ ?\] )) ?\[)
          ((member char '(?\{ ?\} )) ?\{)
          (t char)))

  (defun closer (char)
    (cond ((member char '(?\( ?\) )) ?\))
          ((member char '(?\[ ?\] )) ?\])
          ((member char '(?\{ ?\} )) ?\})
          (t char)))

  (save-excursion
    (progn
      (search-backward (char-to-string (opener old)))
      (delete-char 1)
      (insert-char (opener new) 1))
      (search-forward (char-to-string (closer old)))
      (backward-char)
      (delete-char 1)
      (insert-char (closer new) 1)))

;; Based on http://xahlee.org/emacs/elisp_examples.html
(defun mine-next-user-buffer ()
  "Switch to the next user buffer in cyclic order.\n
User buffers are those not starting with * nor in dired-mode."
  (interactive)
  (let ((start-buf (buffer-name)))
    (next-buffer)
    (while (and (or (string-match "^*" (buffer-name))
                    (string-equal "dired-mode" (symbol-name major-mode)))
                (not (string-equal start-buf (buffer-name))))
      (next-buffer))))

(defun mine-previous-user-buffer ()
  "Switch to the previous user buffer in cyclic order.\n
User buffers are those not starting with * nor in dired-mode."
  (interactive)
  (let ((start-buf (buffer-name)))
    (previous-buffer)
    (while (and (or (string-match "^*" (buffer-name))
                    (string-equal "dired-mode" (symbol-name major-mode)))
                (not (string-equal start-buf (buffer-name))))
      (previous-buffer))))

;; displays a popup window, useful for agenda notifications.
;; requires zenity
(defun mine-popup (title msg)
  (shell-command (concat "zenity --warning --width=300 --title=\"" title "\" --text=\"" msg "\"")))

;; extracted from http://xahlee.org/emacs/emacs_copy_cut_current_line.html
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy the current line."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (progn
       (message "Current line is copied.")
       (list (line-beginning-position) (line-beginning-position 2)) ) ) ))

(defadvice kill-region (before slick-copy activate compile)
  "When called interactively with no active region, cut the current line."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (progn
       (list (line-beginning-position) (line-beginning-position 2)) ) ) ))

;; functions to move selected region up and down
;; based on https://groups.google.com/group/gnu.emacs.help/msg/a784fbb684a24e17?pli=1
;; TODO: undo does not restore state
(defun mine-move-text-internal (arg)
   (cond
    ((and mark-active transient-mark-mode)
     (if (> (point) (mark))
        (exchange-point-and-mark))
     (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
       (forward-line arg)
       (move-to-column column t)
       (set-mark (point))
       (insert text)
       (exchange-point-and-mark)
       (setq deactivate-mark nil)))
    (t
     (beginning-of-line)
     (when (or (> arg 0) (not (bobp)))
       (forward-line)
       (when (or (< arg 0) (not (eobp)))
        (transpose-lines arg))
       (forward-line -1)))))

(defun mine-move-text-up (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines up."
   (interactive "*p")
   (mine-move-text-internal (- arg)))

(defun mine-move-text-down (arg)
   "Move region (transient-mark-mode active) or current line
  arg lines down."
   (interactive "*p")
   (mine-move-text-internal arg))
