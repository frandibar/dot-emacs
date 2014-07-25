;;; package --- Summary

;;; Commentary:
;;; Helper functions for sgf files

;;; Code:

(defun parse-sgf-buffer ()
  "Parse buffer and return a hash with all sgf properties (enclosed between brackets and prefixed by a label)."
  (save-excursion
    (point-min)
    (let ((info))                       ; info is an assoc list with entries (key value)
      (while (re-search-forward "\\([a-zA-Z][a-zA-Z]\\)\\[\\([ \\/:\\+\\.(),a-zA-ZáéíóúÐäèêÅâãåè0-9_?\\-]+\\)\\]" nil t)
        (add-to-list 'info (list (match-string 1)
                                 (match-string 2)) 'append))
      info)))


(defun sgf-sanitize-prop (pc prop-name prop-value)
  "Remove and/or escape certain characters from PROP-VALUE.
PC is the sgf PC property.
PROP-NAME is the name of the property.
PROP-VALUE is it's value."
  (let ((val (s-trim prop-value)))
    (if (or (s-equals? prop-name "RE")
            (s-equals? prop-name "BR")
            (s-equals? prop-name "WR"))
        (s-replace "?" "q" (s-replace "+" "p" val))
      (if (s-equals? pc "Dragon Go Server: http://www.dragongoserver.net/")
          (cond ((or (s-equals? prop-name "PW")
                     (s-equals? prop-name "PB"))
                 ;; Extract the username between parentheses.
                 (s-replace ")" "" (s-replace "(" "" (replace-regexp-in-string ".*\\(\(.*\)\\).*" "\\1" val))))
                ((s-equals? prop-name "DT")
                 (s-replace "," "_" val))
                (t val))
        val))))

(defun sgf-get-prop (prop-name info)
  (sgf-sanitize-prop (second (assoc "PC" info))
                     prop-name
                     (second (assoc prop-name info))))


(defun sgf-get-filename (filename)
  "FILENAME is the file to rename."
  (let ((owm openwith-mode))
    (openwith-mode -1)
    (find-file filename)
    (let* ((info (parse-sgf-buffer))
           (black (sgf-get-prop "PB" info))
           (black-rank (sgf-get-prop "BR" info))
           (white (sgf-get-prop "PW" info))
           (white-rank (sgf-get-prop "WR" info))
           (date (sgf-get-prop "DT" info))
           (result (sgf-get-prop "RE" info)))
      (kill-buffer)
      (openwith-mode owm)
      (format "%s_%s_%s_B_vs_%s_%s_W_%s.sgf" date black black-rank white white-rank result))))


(defun sgf-rename-file-dired ()
  "In Dired mode, rename file under cursor."
  (interactive)
  (let* ((oldname (dired-file-name-at-point))
         (newname (sgf-get-filename oldname)))
    (rename-file oldname newname)
    (message (format "Renamed file %s to %s" oldname newname))
    (revert-buffer)
    (goto-char (point-min))
    (search-forward newname)))

(defun sgf-read-prop-norm (prop)
  "PROP is the sgf property name."
  (sgf-normalize-prop (sgf-read-property "PC")
                      prop
                      (sgf-read-property prop)))

(defun sgf-mark-wins ()
  "In Dired mode, mark games in which I've won."
  (interactive)
  (dired-mark-files-regexp "frandibar_B.*_Bp")
  (dired-mark-files-regexp "frandibar_W.*_Wp"))

(defun sgf-mark-lost ()
  "In Dired mode, mark games in which I've lost."
  (interactive)
  (dired-mark-files-regexp "frandibar_B.*_Wp")
  (dired-mark-files-regexp "frandibar_W.*_Bp"))

(provide 'sgf)
;;; sgf.el ends here
