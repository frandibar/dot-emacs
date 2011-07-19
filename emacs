;; Custom miscellaneous functions

;; use chrome as default browser
;(setq browse-url-browser-function 'browse-url-generic
      ;browse-url-generic-program "google-chrome")

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

;; copy/paste behavior

;; use C-X C-V C-C for copy/pasting
;(cua-mode t)

;; make system copy work with Emacs paste and Emacs copy work with system paste
(setq x-select-enable-clipboard t)

;; File type modes
;(setq auto-mode-alist (cons '("\.[rkt]\\>" . quack-pltfile-mode) auto-mode-alist))

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

;; Appearance settings

;; set color theme
(require 'color-theme)
(eval-after-load "color-theme" 
  '(progn 
     (color-theme-initialize)
     (color-theme-charcoal-black)))

;; highlight cursor line
;; (disabled because it causes confusion when selecting a region)
;(global-hl-line-mode 1)

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

;; use 'y' or 'n' instead of 'yes' or 'no' for answers
(fset 'yes-or-no-p 'y-or-n-p)

;; Keybindings

(global-set-key "\C-q" 'scroll-n-lines-down)
(global-set-key "\C-z" 'scroll-n-lines-up)
;; reassign clobbered C-q
(global-set-key "\C-x\C-q" 'quoted-insert)

(global-set-key "%" 'match-paren)

;; Specific settings

;; configure scheme
(setq scheme-program-name "racket")
(setq auto-mode-alist (cons '("\\.rkt$" . scheme-mode) auto-mode-alist))

;; set font size
(set-face-attribute 'default nil :height 110)
