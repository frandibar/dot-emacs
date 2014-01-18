;;; sgf-mode.el --- A major mode for sgf files

;; sgf specification: http://www.red-bean.com/sgf/proplist.html

;; DISCLAIMER: This mode is NOT complete, it only serves my needs.

;; Features:
;; - syntax coloring (not all keywords added)
;; - add move coordinates in a19 form, they get converted
;;   automatically to proper form (and the node gets named in original coordinates).
;; - adding a move in between other moves fixes forward moves so
;;   that the same player cannot appear twice in a row (doesn't work for variants)
;; TODO: allow performing rotate and mirror to file in dired mode
;; FIXME: when rotating comments are removed?

(defvar sgf-mode-hook nil)

(defgroup sgf-mode nil
  "Major mode for sgf files.")

(defcustom sgf/external-viewer "/usr/games/quarry"
  "External program for viewing sgf file."
  :type 'string
  :group 'sgf-mode)

(defcustom sgf/name-nodes t
  "When non-nil, name each node with the corresponding move."
  :type 'boolean
  :group 'sgf-mode)

;; open buffers with names ending in .sgf in sgf-mode
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sgf\\'" . sgf-mode))

;; keybindings
(defvar sgf-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'sgf/view-in-external-program)
    (define-key map (kbd "C-c C-h") 'sgf/insert-header)
    (define-key map (kbd "C-c C-m") 'sgf/insert-move)
    map)
  "Keymap for sgf major mode.")

;; syntax highlighting
;; using an optimized regex expression, obtained with
;; (regexp-opt '("GM" "FF" "RU" "CA" "US" "SZ" "KM" "HA" "PW" "PB" "DT" "RE" "N" "W" "B" "AP") t)
;; the regexp is wrapped in < and > to only match keywords if they are surrounded by either a space or a beginning/end-of-file.
(defvar sgf-font-lock-keywords
  (list '("\\<\\(AP\\|CA\\|DT\\|FF\\|GM\\|HA\\|KM\\|P[BW]\\|R[EU]\\|SZ\\|US\\|[BNW]\\)\\>" . font-lock-builtin-face))
  "Minimal highlighting expressions for sgf-mode.")

(define-derived-mode sgf-mode text-mode "sgf"
  "Major mode for editing sgf files."
  (set (make-local-variable 'font-lock-defaults) '(sgf-font-lock-keywords)))

(defun sgf/insert-header ()
  (interactive)
  (goto-char (point-min))
  (insert "(;GM[1]
FF[4]CA[UTF-8]
US[emacs-sgf-mode]
RU[Japanese]
SZ[19]
KM[6.50]
HA[]
PW[]
PB[]
DT[2013-12-31]
RE[]

)"))

(defun next-player ()
  "Returns the next player depending on the cursor position.
TODO: doesn't work when inside variations."
  (save-excursion
    (let ((pos (search-backward-regexp "[BW]\\[" nil t)))
      (if pos
          (if (string-equal (buffer-substring-no-properties pos (1+ pos)) "B") ?W ?B)
        ?B))))

(defmacro atomic-undo (fun)
  "Execute FUN in such a way that all changes can be reverted with only one undo operation."
  ;; buffer-undo-list is a list of undo entries in current buffer.
  ;; Recent changes come first; older changes follow newer.
  ;; Entries with value `nil' mark undo boundaries.  The undo command treats
  ;; the changes between two undo boundaries as a single step to be undone.
  '(undo-boundary)
  `(let ((bottom buffer-undo-list))
     ,(funcall fun)
     (let ((lst buffer-undo-list))
       (while (and (cdr lst)
                   (not (eq (cdr lst) bottom)))
         (if (null (cadr lst))
             (setcdr lst (cddr lst))
           (setq lst (cdr lst)))))
     ))

(defun sgf/fix-forward-moves ()
  (save-excursion
    (while (re-search-forward "\\<B\\[" nil t)
      (replace-match "XXX[" nil nil)))
  (save-excursion
    (while (re-search-forward "\\<W\\[" nil t)
      (replace-match "B[" nil nil)))
  (save-excursion
    (while (re-search-forward "\\<XXX\\[" nil t)
      (replace-match "W[" nil nil))))

(defun sgf/insert-move (coord)
  (interactive "sCoordinate (e.g. a15): ")
  (let ((new-coord (sgf/a1-to-sgf-coords coord)))
    (if new-coord
        (progn
          (insert (next-player) "[" new-coord "]N[" coord "];\n")
          (sgf/fix-forward-moves)
          ;; (atomic-undo sgf/fix-forward-moves))  ; FIXME macro not executing
      (message "Invalid move.")))))

(defun sgf/a1-to-sgf-coords (coords)
  "Returns a coordinate such as `a1' converted to sgf file format `as'"
  ;; a1->as, a19->aa, t1->ss, t19->sa
  (when (and (>= (length coords) 2)
             (<= (length coords) 3))
    (cl-flet ((col (c)
                   "[a..h]->[a..h], [j..t]->[i..s]"
                   (let ((dc (downcase c)))
                     (if (> dc ?h)
                         (1- dc)
                       dc)))
              (row (r)
                   "[1..19]->[s..a]"
                   (let ((nr (string-to-number r)))
                     (- ?t nr))))
      (string (col (aref coords 0))
              (row (substring coords 1))))))

(defun sgf/mirror-coord (coord)
  (+ ?a (- ?s coord)))

(defun sgf/mirror ()
  "Mirrors the board along the vertical axis."
  (interactive)
  (save-excursion
    (goto-char 1)
    (while (search-forward-regexp "\\[\\([a-s]\\)\\([a-s]\\)\\]" nil t)
      (replace-match (concat "["
                             (char-to-string (sgf/mirror-coord (string-to-char (match-string 1))))
                             (match-string 2)
                             "]") t nil))))

(defun sgf/rotate180 ()
  (interactive)
  (save-excursion
    (goto-char 1)
    (while (search-forward-regexp "\\[\\([a-s]\\)\\([a-s]\\)\\]" nil t)
      (replace-match (concat "["
                             (char-to-string (sgf/mirror-coord (string-to-char (match-string 1))))
                             (char-to-string (sgf/mirror-coord (string-to-char (match-string 2))))
                             "]") t nil))))

(defun sgf/view-in-external-program ()
  (interactive)
  (message (buffer-file-name (current-buffer)))
  (call-process-shell-command (concat sgf/external-viewer " " (buffer-file-name (current-buffer))) nil))

(provide 'sgf-mode)
