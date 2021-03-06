;;; package --- myfuncs.el

;;; Commentary:
;;; My custom miscellaneous functions

;; Package-Requires: ((names "0.5") (emacs "24"))

;;; Code:

;;;###autoload
(define-namespace mine-

(defun backward-up-sexp (arg)
  "Added because existing function `backward-up-list' won't work when
point is between double quotes.

Extracted from URL
`http://stackoverflow.com/questions/5194417/how-to-mark-the-text-between-the-parentheses-in-emacs'."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(defun dip ()
  "Kill text inside parentheses.
Similar to di) in vim.
It doesn't work if cursor is between double quotes."
  (interactive)
  (backward-up-sexp nil)
  (kill-sexp)
  (insert-parentheses))

(defalias 'dip #'dip)

                                        ; FIXME
(defun vip ()
  "Mark text inside parenthesis (excluding parentheses).
Similar to vi) in vim.
It doesn't work if cursor is between double quotes."
  (interactive)
  (backward-up-sexp nil)
  (mark-sexp)
  (forward-char)
  (exchange-point-and-mark)
  (backward-char))

(defalias 'vip #'vip)

                                        ; FIXME
(defun vap ()
  "Mark text inside parenthesis (including parenthesis).
Similar to va) in vim.
It doesn't work if cursor is between double quotes."
  (interactive)
  (backward-up-sexp nil)
  (mark-sexp)
  (exchange-point-and-mark))

(defalias 'vap #'vap)

(defun match-paren (arg)
  "Go to the matching parenthesis if cursor on a parenthesis;
otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

                                        ; FIXME
(defun toggle-fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0)))
                                        ; disabled because sometimes leaves a status bar with too many lines
                                        ;(toggle-fullscreen)

(defun point-to-middle ()
  "Put cursor on top line of window.
Similar to 'M' in vim."
  (interactive)
  (push-mark)
  (move-to-window-line nil))

(defalias 'mm 'point-to-middle)

(defun point-to-top ()
  "Put cursor on top line of window.
Similar to 'H' in vim."
  (interactive)
  (push-mark)
  (move-to-window-line 0))

(defalias 'hh 'point-to-top)

(defun point-to-bottom ()
  "Put cursor at bottom of last visible line.
Similar to 'L' in vim."
  (interactive)
  (push-mark)
  (move-to-window-line -1))

(defalias 'll 'point-to-bottom)

(defun insert-date()
  (interactive)
  (insert (format-time-string "%a %b %d, %Y")))

;; Convert a buffer from DOS `^M' end of lines to Unix end of lines.
(defun dos-to-unix ()
  "Cut all visible ^M from the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\r" nil t)
      (replace-match ""))))

(defun unix-to-dos ()
  "Convert a buffer from Unix end of lines to DOS `^M' end of
lines."
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

(defun switch-cpp-h-file ()
  (interactive)
  "Switches buffer to the corresponding header file (.h) if current buffer
is a .cpp file, and vice-versa.
It assumes both files are in the same path. If not, it creates a new file."
  (defun alternate-file (cpp-or-h-file)
    (cond ((equal ".h" (substring cpp-or-h-file -2))
           (concat (substring cpp-or-h-file 0 (- (length cpp-or-h-file) 2)) ".cpp"))
          ((equal ".cpp" (substring cpp-or-h-file -4))
           (concat (substring cpp-or-h-file 0 (- (length cpp-or-h-file) 4)) ".h"))))
  (find-file (alternate-file (buffer-file-name (current-buffer)))))

(defun fast-buffer-switch ()
  "Switch to last buffer."
  (interactive)
  ;; (switch-to-buffer (other-buffer)))    ; switch to most recent non visible buffer
  (switch-to-buffer (other-buffer (current-buffer) t))) ; ignore if most recent is visible or not

(defun with-number-at-point (fn n)
  (save-excursion
    (skip-chars-forward "[:alpha:]_")
    (or (looking-at "-?[0-9]+")
        (error "No number at point"))
    (replace-match (number-to-string (funcall fn (string-to-number (match-string 0)) n)))))

(defun increment-number-at-point (&optional n)
  (interactive "p")
  (with-number-at-point '+ n))

(defun decrement-number-at-point (&optional n)
  (interactive "p")
  (with-number-at-point '- n))

(defun copy-current-line (arg)
  "Copy and paste ARG lines from point.
If point is on last buffer line, then no newline is inserted."
  (interactive "p")
  (save-excursion
    (kill-whole-line arg)
    (yank)
    (yank)))

(defun current-line-to-clipboard (arg)
  "Copy ARG lines to clipboard. Default value for ARG is 1.
Although for ARG=1 it doesn't make much sense since M-w does the
same thing.  Similar to 'Y' in vim."
  (interactive "p")
  (message (format "Copied %d line(s) to clipboard" arg))
  (save-excursion
    (copy-region-as-kill (line-beginning-position) (line-beginning-position (+ 1 arg)))))

;; (require 'highlight-symbol)

;; (defun hl-symbol-and-jump-next ()
;;   "Search for next occurance of symbol under cursor, with highlight.
;; Similar to '*' in vim, except that the highlighting is preserved
;; on next search."
;;   (interactive)
;;   (hl-symbol-and-jump 'highlight-symbol-next))

;; (defun hl-symbol-and-jump-prev ()
;;   (interactive)
;;   (hl-symbol-and-jump 'highlight-symbol-prev))

;; (defun hl-symbol-and-jump (fn-next-or-prev)
;;   "Search for previous occurance of symbol under cursor, with highlight.
;; Similar to '#' in vim, except that the highlighting is preserved
;; on next search."
;;   (let ((symbol (highlight-symbol-get-symbol)))
;;     (unless symbol (error "No symbol at point"))
;;     (unless hi-lock-mode (hi-lock-mode 1))
;;     (if (member symbol highlight-symbol-list)
;;         (funcall fn-next-or-prev)
;;       (highlight-symbol-at-point)
;;       (funcall fn-next-or-prev))))

;; (defun hl-symbol-cleanup ()
;;   "Clear all highlighted symbols.
;; Taken from http://www.emacswiki.org/emacs/SearchAtPoint."
;;   (interactive)
;;   (mapc 'hi-lock-unface-buffer highlight-symbol-list)
;;   (setq highlight-symbol-list ()))

;; Search at point, similar to * in vim
;; http://www.emacswiki.org/emacs/SearchAtPoint
;; I-search with initial contents
(defvar isearch-initial-string nil)

(defun isearch-set-initial-string ()
  (remove-hook 'isearch-mode-hook 'isearch-set-initial-string)
  (setq isearch-string isearch-initial-string)
  (isearch-search-and-update))

                                        ; TODO: fix, fails when operating on last buffer occurrence
(defun isearch-forward-at-point (&optional regexp-p no-recursive-edit)
  "Interactive search forward for the symbol at point."
  (interactive "P\np")
  (if regexp-p (isearch-forward regexp-p no-recursive-edit)
    (let* ((end (progn (skip-syntax-forward "w_") (point)))
           (begin (progn (skip-syntax-backward "w_") (point))))
      (if (eq begin end)
          (isearch-forward regexp-p no-recursive-edit)
        (setq isearch-initial-string (buffer-substring begin end))
        (add-hook 'isearch-mode-hook 'isearch-set-initial-string)
                                        ;        (goto-char end)
        (isearch-forward regexp-p no-recursive-edit)))))

(defun window-horizontal-to-vertical ()
  "Switches from a horizontal split to a vertical split.
http://www.emacswiki.org/emacs/Rick_Bielawski#toc5
Idea and starter code from Benjamin Rutt (rutt.4+news@osu.edu) on comp.emacs"
  (interactive)
  (let ((one-buf (window-buffer (selected-window)))
        (buf-point (point)))
    (other-window 1)
    (delete-other-windows)
    (split-window-horizontally)
    (switch-to-buffer one-buf)
    (goto-char buf-point)))

;; complement of above created by rgb 11/2004
(defun window-vertical-to-horizontal ()
  "Switches from a vertical split to a horizontal split."
  (interactive)
  (let ((one-buf (window-buffer (selected-window)))
        (buf-point (point)))
    (other-window 1)
    (delete-other-windows)
    (split-window-vertically)
    (switch-to-buffer one-buf)
    (goto-char buf-point)))

(defun semnav-up (arg)
  "Extracted from URL `http://xahlee.org/emacs/modernization_mark-word.html'
by Nikolaj Schumacher, 2008-10-20. Released under GPL."
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

(defun extend-selection (arg &optional incremental)
  "Select the current word.
Subsequent calls expands the selection to larger semantic unit.
By Nikolaj Schumacher, 2008-10-20. Released under GPL."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (or (region-active-p)
                         (eq last-command this-command))))
  (if incremental
      (progn
        (semnav-up (- arg))
        (forward-sexp)
        (mark-sexp -1))
    (if (> arg 1)
        (extend-selection (1- arg) t)
      (if (looking-at "\\=\\(\\s_\\|\\sw\\)*\\_>")
          (goto-char (match-end 0))
        (unless (memq (char-before) '(?\) ?\"))
          (forward-sexp)))
      (mark-sexp -1))))

(defun select-text-in-quote ()
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

(defun close-buffer-and-window ()
  "Kills buffer and window. Asks for confirmation if buffer is not associated to a file nor is a dired buffer."
  (interactive)
  (when (or (buffer-file-name)
            (equal major-mode 'dired-mode)
            (yes-or-no-p "Do you wish to kill buffer?"))
    (kill-buffer)
    (delete-window)))


(defun enclose-quotes (start end)
  "Insert double quotes around a region."
  (interactive "r")
  (save-excursion
    (goto-char end) (insert-char ?" 1)
    (goto-char start) (insert-char ?" 1)
    ))

(defun enclose-parens (start end)
  "Insert parenthesis around a region."
  (interactive "r")
  (save-excursion
    (goto-char end) (insert-char ?) 1)
  (goto-char start) (insert-char ?( 1)
                                 ))

(defun replace-enclosing-char (old new)
  "Replace the enclosing OLD char with NEW.
The cursor must be located in between the enclosing chars.
For empty strings, cursor should be on closing pair.

TODO: make it in such a way that parameter OLD is detected
automatically, and allow specifying it with prefix argument.
"
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

(defun next-user-buffer ()
  "Switch to the next user buffer in cyclic order.
User buffers are those not starting with * nor in dired-mode.
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
(defun popup (title msg)
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

(defun select-current-line ()
  "Select the current line.
Extracted from URL `http://ergoemacs.org/emacs/modernization_mark-word.html'."
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))

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

(defun xml-format ()
  "Reformat xml using xmllint"
  (interactive)
  ;; use xmllint instead of sgml-pretty-print because it's output is nicer
  (call-process-region (point-min) (point-max) "/usr/bin/xmllint" t t t "--format" "-")
  (web-mode))


(defun toggle-folding-level (level)
  "Toggle folding level to show/hide only lines indentation level LEVEL lines
  TODO: not working"
  (interactive "P")
  (save-excursion
    (let ((offset
           (cond ((member mode-name '("Lisp Interaction" "Emacs-Lisp")) 2)
                 ((member mode-name '("Python")) 4)
                 (t 0))))
      (goto-char (point-min))
      ;; use set-selective display function instead of (setq selective-display)
      ;; so message appears
      (set-selective-display (cond (level (* level offset))
                                   ((eq selective-display 0) offset)
                                   (t 0))))))

(defun toggle-case ()
  "Toggle the letter case of current word or text selection.
Toggles between: “all lower”, “Init Caps”, “ALL CAPS”.

Extracted from URL `http://ergoemacs.org/emacs/modernization_upcase-word.html'."
  (interactive)
  (let (p1 p2 (deactivate-mark nil) (case-fold-search nil))
    (if (region-active-p)
        (setq p1 (region-beginning) p2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'word) ) )
        (setq p1 (car bds) p2 (cdr bds)) ) )

    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char p1)
        (cond
         ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all caps") )
         ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "init caps") )
         ((looking-at "[[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]]") (put this-command 'state "all caps") )
         (t (put this-command 'state "all lower") ) ) ) )

    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region p1 p2) (put this-command 'state "init caps"))
     ((string= "init caps" (get this-command 'state))
      (upcase-region p1 p2) (put this-command 'state "all caps"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region p1 p2) (put this-command 'state "all lower")) )
    ) )

;; must start with `eshell/' so it can be called as `clear' from the eshell prompt
(defun eshell/clear ()
  "Clears the eshell buffer.
Extracted from URL `http://www.khngai.com/emacs/eshell.php'."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun eshell-prompt-function-short ()
  "Makes a short eshell prompt to avoid moving out of the buffer window boundary"
  (let* ((pwd (eshell/pwd))
         (pwdlst (split-string pwd "/"))
         (rpwdlst (reverse pwdlst))
         (base (car rpwdlst)))
    (concat (if (string= base "")
                "/"
              (if (cdr pwdlst) "<...> /" ""))
            base
            (if (= (user-uid) 0) " # " " $ "))))

;; Returns the long prompt string for eshell
(defun eshell-prompt-function-long ()
  "Makes a long standard eshell prompt"
  (concat (abbreviate-file-name (eshell/pwd))
          (if (= (user-uid) 0) " # " " $ ")))

(setq eshell-prompt-function 'eshell-prompt-function-long)

(defun eshell/sprompt ()
  "Makes a short eshell prompt to avoid moving out of the buffer
window boundary (link to eshell-prompt-function-short)"
  (setq eshell-prompt-function 'eshell-prompt-function-short))

(defun eshell/lprompt ()
  "Makes a long standard eshell prompt (link to
eshell-prompt-function-long)"
  (setq eshell-prompt-function 'eshell-prompt-function-long))

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

(defun scissors ()
  (interactive)
  (insert "8<--------8<--------8<--------8<--------8<--------8<--------8<--------8<--------"))

;; OVERRIDES

(defun sauron-clear ()
  "Clear the sauron buffer."
  (interactive)
  (when
      (and sr-buffer (buffer-live-p sr-buffer))
                                        ; I commented this out to avoid being asked
                                        ;      (yes-or-no-p "Are you sure you want to clear the log? "))
    (with-current-buffer sr-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (message nil)
                                        ; added by me
    (sr-hide)))

(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.
Extracted from URL `http://emacsredux.com'."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

;; use the following functions to specify a font for a mode
;; i.e. (add-hook 'help-mode-hook 'use-proportional-font)
(defun use-proportional-font ()
  "Switch the current buffer to a proportional font."
  (face-remap-add-relative 'default '(:family "FreeSans")))

(defun use-monospace-font ()
  "Switch the current buffer to a monospace font."
  (face-remap-add-relative 'default '(:family "Monospace")))

(defun remove-crln ()
  "Removes CRLF (^M) in file"
  (interactive)
  (save-excursion
    (replace-string (string 13) "" nil (point-min) (point-max))))

(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary.
Ropen the selected file as root (you’ll be prompted for your sudo password)
if you don’t have write permissions for it.

Extracted from URL `http://emacsredux.com/blog/2013/04/21/edit-files-as-root'."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))


(defun cmp-pairs (fn x y)
  "Returns a list ((fn x1 y1) (fn x2 y2) ... (fn xn yn)) for X = (x1 x2 ... xn) and Y = (y1 y2 ... yn).
Both lists must have same length."
  (if (= (length x) (length y))
      (if x
          (cons (funcall fn (car x) (car y)) (cmp-pairs fn (cdr x) (cdr y)))
        '())
    (error "Lists must have same size.")))

;; smarter move to BOL:
;; upon C-a, move to first char in line, if pressed again, move to BOL
;; extracted from http://irreal.org/blog/?p=1946
(defadvice move-beginning-of-line (around smarter-bol activate)
  ;; Move to requested line if needed.
  (let ((arg (or (ad-get-arg 0) 1)))
    (when (/= arg 1)
      (forward-line (1- arg))))
  ;; Move to indentation on first call, then to actual BOL on second.
  (let ((pos (point)))
    (back-to-indentation)
    (when (= pos (point))
      ad-do-it)))

(defun eshell-kill-line ()
  (interactive)
  (eshell-bol)
  (kill-line))

(defun byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists.
Extracted from URL `http://ergoemacs.org/emacs/emacs_byte_compile.html'."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(add-hook 'after-save-hook 'byte-compile-current-buffer)

;; make long prefixes display as shorter prefixes
(defface prefix
  '((t (:foreground "grey50")))
  "Face for simplified prefixes.")

(defun simplify-prefix (prefix rep)
  "Replace PREFIX with REP visually on this buffer.

PREFIX is simply displayed as REP, but not actually replaced with REP.
Extracted from URL `http://yoo2080.wordpress.com/2013/09/22/how-to-choose-emacs-lisp-package-namespace-prefix/'."
  (interactive "sVisually replace this long prefix: \nsWith this short prefix: ")
  (font-lock-add-keywords
   nil `((
          ;; ;; not sure why these don't work
          ;; ,(rx-to-string `(group word-boundary ,prefix word-boundary))
          ;; ,(rx-to-string `(: word-boundary ,prefix word-boundary))
          ;; ,(rx-to-string `(: ,prefix))

          ,(rx-to-string `(group ,prefix))

          (0 (progn (put-text-property (match-beginning 0) (match-end 0)
                                       'display ,rep)
                    'prefix)))))
  (font-lock-fontify-buffer))

(defun backward-up-list+-1 ()
  "Go to left of closing parenthesis.
Extracted from URL `http://jaderholm.com/blog/programothesis-27-emacs-paredit-more-special-features'."
  (interactive)
  (backward-up-list)
  (forward-char))

(define-key lisp-mode-shared-map (kbd "C-M-9") 'backward-up-list+-1)

(defun up-list+-1 ()
  "Go to right of openning parenthesis.
Extracted from URL `http://jaderholm.com/blog/programothesis-27-emacs-paredit-more-special-features'."
  (interactive)
  (up-list)
  (backward-char))

(define-key lisp-mode-shared-map (kbd "C-M-0") 'up-list+-1)

(defun comment-tests ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil))       ; case-sensitive
      (while (search-forward "def test" nil t)
        (replace-match "def XXXtest")))
    ))


(defun uncomment-tests ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil))       ; case-sensitive
      (while (search-forward "def XXXtest" nil t)
        (replace-match "def test")))
    ))

(defun pdb-break ()
  (interactive)
  (insert "import pdb;pdb.set_trace()  # TODO remove\n"))


;; Toggle between python buffers and python shell
;; Extracted from
;; http://www.masteringemacs.org/articles/2011/02/23/toggling-python-buffers/

(defvar python-last-buffer nil
  "Name of the Python buffer that last invoked `toggle-between-python-buffers'")

(make-variable-buffer-local 'python-last-buffer)

(defun toggle-between-python-buffers ()
  "Toggles between a `python-mode' buffer and its inferior Python process

When invoked from a `python-mode' buffer it will switch the
active buffer to its associated Python process. If the command is
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

(defun json-format ()
  "JSON Pretty format for selected region."
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))

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
  "Swaps the quote symbols in a \\[python-mode] string
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

(defun make-helm-full-frame ()
  "Extracted from URL `http://emacs.stackexchange.com/questions/643/make-helm-window-the-only-window'"
  (interactive)
  (with-selected-window (helm-window)
    (delete-other-windows)))
)

(provide 'myfuncs)

;;; myfuncs.el ends here
