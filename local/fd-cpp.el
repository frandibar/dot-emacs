;;; package --- fd-cpp.el

;;; Commentary:
;;; My custom C++ related functions.

;;; Code:

;;;###autoload
(define-namespace fd-cpp-

(defun switch-header-file ()
  "Switch buffer to the corresponding header file (.h) if current
buffer is a `.cpp' file, and vice-versa. It assumes both files are in
the same path. If not, it creates a new file."
  (interactive)
  (defun alternate-file (cpp-or-h-file)
    (cond ((equal ".h" (substring cpp-or-h-file -2))
           (concat (substring cpp-or-h-file 0 (- (length cpp-or-h-file) 2)) ".cpp"))
          ((equal ".cpp" (substring cpp-or-h-file -4))
           (concat (substring cpp-or-h-file 0 (- (length cpp-or-h-file) 4)) ".h"))))
  (find-file (alternate-file (buffer-file-name (current-buffer)))))

)

(provide 'fd-cpp)
;;; fd-cpp.el ends here
