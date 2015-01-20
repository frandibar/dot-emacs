;;; package --- fd-misc.el

;;; Commentary:
;;; My custom miscellaneous functions

;; Package-Requires: ((names "0.5") (emacs "24") (cl-lib "1.0") (helm "20141210.919"))

;;; Code:

;;;###autoload
(define-namespace fd-misc-

(defun insert-date()
  (interactive)
  (insert (format-time-string "%a %b %d, %Y")))


(defun replace-enclosing-char (old new)
  "Replace the enclosing OLD char with NEW.
The cursor must be located in between the enclosing chars.
For empty strings, cursor should be on closing pair.

TODO: make it in such a way that parameter OLD is detected
automatically, and allow specifying it with prefix argument."
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

;; Display a popup window, useful for agenda notifications.
;; Requires zenity.
(defun popup (title msg)
  (shell-command (concat "zenity --warning --width=300 --title=\"" title "\" --text=\"" msg "\"")))

;; FIXME
(defun select-current-line ()
  "Select the current line.
Extracted from URL `http://ergoemacs.org/emacs/modernization_mark-word.html'."
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))

;; FIXME
(defun toggle-folding-level (level)
  "Toggle folding level to show/hide only lines indentation level LEVEL lines."
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

(defun scissors ()
  "Draw some scissors."
  (interactive)
  (insert "8<--------8<--------8<--------8<--------8<--------8<--------8<--------8<--------"))

;; OVERRIDES

(defun sauron-clear ()
  "Clear the sauron buffer."
  (interactive)
  (when
      (and sr-buffer (buffer-live-p sr-buffer))
    ;; I commented this out to avoid being asked
    ;;      (yes-or-no-p "Are you sure you want to clear the log? "))
    (with-current-buffer sr-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (message nil)
    ;; Added by me.
    (when (fboundp 'sr-hide)
      (sr-hide))))

(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.
Extracted from URL `http://emacsredux.com'."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

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

(defun byte-compile-current-buffer ()
  "`byte-compile' current buffer if it's emacs-lisp-mode and compiled file exists.
Extracted from URL `http://ergoemacs.org/emacs/emacs_byte_compile.html'."
  (interactive)
  (when (and (eq major-mode 'emacs-lisp-mode)
             (file-exists-p (byte-compile-dest-file buffer-file-name)))
    (byte-compile-file buffer-file-name)))

(add-hook 'after-save-hook #'byte-compile-current-buffer)

;; Make long prefixes display as shorter prefixes.
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

(defun make-helm-full-frame ()
  "Extracted from URL `http://emacs.stackexchange.com/questions/643/make-helm-window-the-only-window'"
  (interactive)
  (when (fboundp 'helm-window)
    (with-selected-window (helm-window)
      (delete-other-windows))))

)


;; The following are not namespaced because it didn't work correctly.

;; Search at point, similar to * in vim
;; http://www.emacswiki.org/emacs/SearchAtPoint
;; I-search with initial contents
(defvar fd-isearch-initial-string nil)

(defun fd-isearch-set-initial-string ()
  "Helper fn for `fd-isearch-forward-at-point'."
  (remove-hook 'isearch-mode-hook #'fd-isearch-set-initial-string)
  (setq isearch-string fd-isearch-initial-string)
  (isearch-search-and-update))

;; FIXME: fails when operating on last buffer occurrence
;; FIXME: broken
(defun fd-isearch-forward-at-point (&optional regexp-p no-recursive-edit)
  "Interactive search forward for the symbol at point."
  (interactive "P\np")
  (if regexp-p (isearch-forward regexp-p no-recursive-edit)
    (let* ((end (progn (skip-syntax-forward "w_") (point)))
           (begin (progn (skip-syntax-backward "w_") (point))))
      (if (eq begin end)
          (isearch-forward regexp-p no-recursive-edit)
        (setq isearch-initial-string (buffer-substring begin end))
        (add-hook 'isearch-mode-hook #'fd-isearch-set-initial-string)
        ;;(goto-char end)
        (isearch-forward regexp-p no-recursive-edit)))))


(provide 'fd-misc)
;;; fd-misc.el ends here
