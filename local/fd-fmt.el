;;; package --- fd-fmt.el

;;; Commentary:
;;; My custom miscellaneous functions

;; Package-Requires: ((names "0.5") (emacs "24") (cl-lib "1.0") (helm "20141210.919"))

;;; Code:

;;;###autoload
(define-namespace fd-fmt-

(defun json-format ()
  "JSON Pretty format for selected region."
  (interactive)
  (save-excursion
    (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)))

(defun xml-format ()
  "Reformat xml using xmllint."
  (interactive)
  ;; Use xmllint instead of sgml-pretty-print because it's output is nicer.
  (call-process-region (point-min) (point-max) "/usr/bin/xmllint" t t t "--format" "-")
  (when (fboundp 'web-mode)
    (web-mode)))

)

(provide 'fd-fmt)
;;; fd-fmt.el ends here
