;; My personal emacs customization file

;; Custom miscellaneous functions

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

(defun backward-up-sexp (arg)
  "Added because existing function backward-up-list won't work when point is between double quotes.

Extracted from
http://stackoverflow.com/questions/5194417/how-to-mark-the-text-between-the-parentheses-in-emacs
"
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(global-set-key [remap backward-up-list] 'backward-up-sexp)

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
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

;; Copy/paste behavior

;; use C-x C-v C-c for copy/pasting
;(cua-mode t)

;; make system copy work with Emacs paste and Emacs copy work with system paste
(setq x-select-enable-clipboard t)

;; Appearance settings

;; set color theme
;; (require 'color-theme)
;; (eval-after-load "color-theme" 
;;   '(progn 
;;      (color-theme-initialize)
;; ;     (color-theme-charcoal-black)))
;; ;     (color-theme-cooper-dark)))  ; this doesn't work, should set Cooper Dark
;; ;     (color-theme-hober)))
;; ;     (color-theme-midnight)))
;;      (color-theme-dark-laptop)))

(setq load-path (append load-path (list "~/.emacs.d/color-themes")))
(require 'zenburn)
(zenburn)

;; highlight cursor line
;; (disabled because it causes confusion when selecting a region)
;(global-hl-line-mode 1)

;; show matching parenthesis
(show-paren-mode t)

;; hide tool bar and menu bar
(tool-bar-mode nil)
(menu-bar-mode t)

;; show line and column number in the mode line
(line-number-mode 1)
(column-number-mode 1)

;; set scroll bar to the right
(setq scroll-bar-mode-explicit t)
(set-scroll-bar-mode `right) 

;; hide splash screen
(setq inhibit-splash-screen t)

;; show line numbers
(global-linum-mode 1)

;; set font size
(set-face-attribute 'default nil :height 110)

;; highlight tabs and trailing spaces
(setq whitespace-style '(tabs trailing space-before-tab newline indentation empty space-after-tab tab-mark))
(whitespace-mode t)

;; show blank screen on startup
(setq initial-scratch-message nil)

;; Keybindings

;(global-set-key "\C-q" 'scroll-n-lines-down)
;(global-set-key "\C-z" 'scroll-n-lines-up)
;; reassign clobbered C-q
;(global-set-key "\C-x\C-q" 'quoted-insert)

(global-set-key "%" 'match-paren)

;; Misc

;; Turn off tab character
;; Emacs normally uses both tabs and spaces to indent lines. To use only spaces
;; set `indent-tabs-mode' to `nil'. This is a per-buffer variable;
;; altering the variable affects only the current buffer, but it can be
;; disabled for all buffers.
(setq-default indent-tabs-mode nil) 

;; disable backup
(setq backup-inhibited t)

;; disable auto save
(setq auto-save-default nil)

;; use 'y' or 'n' instead of 'yes' or 'no' for answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Specific settings

;; configure scheme
(setq scheme-program-name "racket")
(setq auto-mode-alist (cons '("\\.rkt$" . scheme-mode) auto-mode-alist))
;(setq auto-mode-alist (cons '("\.[rkt]\\>" . quack-pltfile-mode) auto-mode-alist))

;; open txt files in org-mode
;(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))

;; use chrome as default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")
