;;; package --- Summary

;;; Commentary:
;;; Helper functions for sgf files

;;; Code:

(defun sgf-read-property (prop)
  (save-excursion
    (point-min)
    (re-search-forward (format "%s\\[\\([ \\/:\\+\\.(),a-zA-ZáéíóúÐäèêÅâãåè0-9_\\-]+\\)\\]" prop))
    (match-string 1)))

(defun sgf-rename-file-dired ()
  (interactive)
  (let* ((oldname (dired-file-name-at-point))
         (newname (sgf-rename-file oldname)))
    (rename-file oldname newname)
    (message (format "Renamed file %s to %s" oldname newname))
    (revert-buffer)
    (goto-char (point-min))
    (search-forward newname)))

(defun sgf-normalize-prop (pc prop-name prop-value)
  (if (s-equals? prop-name "RE")
      (s-replace "+" "p" prop-value)
    (if (s-equals? pc "Dragon Go Server: http://www.dragongoserver.net/")
        (cond ((or (s-equals? prop-name "PW")
                   (s-equals? prop-name "PB"))
               ;; Extract the username between parentheses.
               (s-replace ")" "" (s-replace "(" "" (replace-regexp-in-string ".*\\(\(.*\)\\).*" "\\1" prop-value))))
              ((s-equals? prop-name "DT")
               (s-replace "," "_" prop-value)))
      prop-value)))

(defun sgf-read-prop-norm (prop)
  (sgf-normalize-prop (sgf-read-property "PC")
                      prop
                      (sgf-read-property prop)))

(defun sgf-rename-file (filename)
  (let ((owm openwith-mode))
    (openwith-mode -1)
    (find-file filename)
    (let ((black (sgf-read-prop-norm "PB"))
          (white (sgf-read-prop-norm "PW"))
          (date (sgf-read-prop-norm "DT"))
          (result (sgf-read-prop-norm "RE")))
      (kill-buffer)
      (openwith-mode owm)
      (format "%s_%s_B_vs_%s_W_%s.sgf" date black white result))))

(defun sgf-mark-wins ()
  (interactive)
  (dired-mark-files-regexp "frandibar_B.*_Bp")
  (dired-mark-files-regexp "frandibar_W.*_Wp"))

(defun sgf-mark-lost ()
  (interactive)
  (dired-mark-files-regexp "frandibar_B.*_Wp")
  (dired-mark-files-regexp "frandibar_W.*_Bp"))

