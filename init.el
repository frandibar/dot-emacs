;; MY PERSONAL EMACS CUSTOMIZATION FILE

;; load my functions
(add-to-list 'load-path (expand-file-name "~/.emacs.d") t)
(require 'myfuncs)

;; Copy/paste behavior
;; use C-x C-v C-c for copy/pasting
;(cua-mode t)
;; use cua mode for rectangle selection
(cua-selection-mode t)

;; make system copy work with Emacs paste and Emacs copy work with system paste
(setq x-select-enable-clipboard t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; APPEARANCE SETTINGS

;; set window title to buffer-file-name
(setq frame-title-format '("" "emacs - %b - " buffer-file-name))

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

(add-to-list 'load-path (expand-file-name "~/.emacs.d/color-themes") t)
(require 'zenburn)
(zenburn)

;; highlight cursor line
;; (disabled because it causes confusion when selecting a region)
;(global-hl-line-mode 1)

;; show matching parenthesis
(show-paren-mode t)

;; hide tool bar and menu bar
(tool-bar-mode -1)
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
;(global-whitespace-mode 1)

;; show blank screen on startup
(setq initial-scratch-message nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEYBINDINGS

;; Notice that they all start with C-c, and then control again to avoid race
;; condition between first and second keys.
;; This guarantees that they won't be overriden by any mode.

(global-set-key (kbd "C-c C-e") 'mine-scroll-n-lines-up)
(global-set-key (kbd "C-c C-d") 'mine-scroll-n-lines-down)

(global-set-key (kbd "C-c C-H") 'mine-point-to-top)
(global-set-key (kbd "C-c C-L") 'mine-point-to-bottom)

(global-set-key (kbd "C-c C-f") 'mine-advance-to)
(global-set-key (kbd "C-c C-b") 'mine-back-to)

(global-set-key (kbd "C-c C-$") 'mine-point-to-eol)
(global-set-key (kbd "C-c C-%") 'mine-match-paren)
(global-set-key (kbd "C-c C-^") 'mine-fast-buffer-switch)

(global-set-key (kbd "C-c C-a") 'mine-increment-number-at-point)
(global-set-key (kbd "C-c C-x") 'mine-decrement-number-at-point)

(global-set-key (kbd "C-c C-*") 'mine-isearch-forward-at-point)

(global-set-key (kbd "C-c C-Y") 'mine-current-line-to-clipboard)

(global-set-key (kbd "C-c C-h") 'mine-window-vertical-to-horizontal)
(global-set-key (kbd "C-c C-v") 'mine-window-horizontal-to-vertical)

(global-set-key [remap backward-up-list] 'mine-backward-up-sexp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MISC

;; turn off tab character
;; Emacs normally uses both tabs and spaces to indent lines. To use only spaces
;; set `indent-tabs-mode' to `nil'. This is a per-buffer variable;
;; altering the variable affects only the current buffer, but it can be
;; disabled for all buffers.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; disable backup
(setq backup-inhibited t)

;; disable auto save
(setq auto-save-default nil)

;; use 'y' or 'n' instead of 'yes' or 'no' for answers
(fset 'yes-or-no-p 'y-or-n-p)

;; enable fast file/buffer switching mode
(ido-mode t)

;; enable disabled functions by default
(put 'narrow-to-region 'disabled nil)

;; reload file if changed on disk
(global-auto-revert-mode 1)

;; display current function name in the mode line
(which-function-mode t)

;; set indentation style for c++-mode
(setq c-default-style "stroustrup"
      c-basic-offset 4)

;; enable ecb
;; (add-to-list 'load-path "~/.emacs.d/ecb-snap")
;; (require 'ecb)

;; open txt files in org-mode
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
;; org-mode clean view
(setq org-startup-indented t)
; in order to hide the leading stars, set the org-hide face color to background
;; (setq org-hide-leading-stars t)   ; this is already set with org-startup-indented

;; load initializations for this site
(let ((init-file "~/.emacs.d/init-local.el"))
  (if (file-exists-p init-file)
      (load-file init-file)))

;; In order open a file in an existing emacs from a shell, use
;; emacsclient -n [file]
;; TODO: avoid starting server if already started
(server-start)

(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file 'noerror)
