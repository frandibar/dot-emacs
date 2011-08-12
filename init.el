;; My personal emacs customization file

;; load my functions
(setq load-path (append load-path (list "~/.emacs.d")))
(require 'myfuncs)

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
(global-whitespace-mode 1)

;; show blank screen on startup
(setq initial-scratch-message nil)

;; Keybindings

;(global-set-key "\C-q" 'scroll-n-lines-down)
;(global-set-key "\C-z" 'scroll-n-lines-up)
;; reassign clobbered C-q
;(global-set-key "\C-x\C-q" 'quoted-insert)

(global-set-key "%" 'match-paren)

(global-set-key "\C-c\C-f" 'advance-to)
(global-set-key "\C-c\C-b" 'back-to)

(global-set-key [?\C-6] 'fast-buffer-switch)

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

;; enable fast file/buffer switching mode
(ido-mode t)

;; In order open a file in an existing emacs from a shell, use
;; emacsclient -n [file]
(server-start)

;; enable disabled functions by default
(put 'narrow-to-region 'disabled nil)

;; Specific settings

;; configure scheme
(setq scheme-program-name "racket")
(setq auto-mode-alist (cons '("\\.rkt$" . scheme-mode) auto-mode-alist))
;(setq auto-mode-alist (cons '("\.[rkt]\\>" . quack-pltfile-mode) auto-mode-alist))

;; open txt files in org-mode
;(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
;; org-mode clean view
(setq org-startup-indented t)

;; use chrome as default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;; set indentation style for c++-mode
(setq c-default-style "stroustrup"
      c-basic-offset 4)

(add-to-list 'load-path "~/.emacs.d/ecb-snap")
(require 'ecb)

(which-function-mode t)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(org-hide ((((background dark)) (:foreground "grey20"))))
 '(which-func ((((class color) (min-colors 88) (background dark)) (:foreground "cyan")))))
