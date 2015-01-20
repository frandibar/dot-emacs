;;; package --- fd-font.el

;;; Commentary:
;;; My custom font related functions.

;;; Code:

;;;###autoload
(define-namespace fd-font-

;; Use the following functions to specify a font for a mode.
;; i.e. (add-hook 'help-mode-hook #'use-proportional-font)
(defun use-proportional-font ()
  "Switch the current buffer to a proportional font."
  (face-remap-add-relative 'default '(:family "FreeSans")))

(defun use-monospace-font ()
  "Switch the current buffer to a monospace font."
  (face-remap-add-relative 'default '(:family "Monospace")))

)

(provide 'fd-font)
;;; fd-font.el ends here
