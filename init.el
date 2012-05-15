;; MY PERSONAL EMACS CUSTOMIZATION FILE

;; load my functions
(add-to-list 'load-path "~/.emacs.d")

;; add the bundles directory and it's 1st level subdirectories to the load-path variable
(let ((base "~/.emacs.d/external"))
  (add-to-list 'load-path base)
  (dolist (f (directory-files base))
    (let ((name (concat base "/" f)))
      (when (and (file-directory-p name)
                 (not (equal f ".."))
                 (not (equal f ".")))
        (add-to-list 'load-path name)))))

(require 'myfuncs)

;; add the site-lisp directory recursively to the load-path variable (needed when compiled only)
(let ((default-directory  "/usr/share/emacs/site-lisp"))
     (normal-top-level-add-subdirs-to-load-path))

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

(require 'color-theme-zenburn)
(color-theme-zenburn)

;; highlight cursor line, commented out as it produces flickering in org mode
;; (global-hl-line-mode 1)

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

;; show blank screen on startup
(setq initial-scratch-message nil)

;; show line numbers
(global-linum-mode 1)

;; set font size
(set-face-attribute 'default nil :height 110)

;; highlight tabs and trailing spaces
(setq whitespace-style '(tabs trailing space-before-tab newline indentation empty space-after-tab tab-mark))
;(global-whitespace-mode 1)

;; the eshell directory holds alias definitions and history information
(setq eshell-directory-name "~/.emacs.d/eshell")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEYBINDINGS

;; When possible, they should start with C-c, and then control again to avoid race
;; condition between first and second keys.
;; This guarantees that they won't be overriden by any mode.

(global-set-key (kbd "C-c C-n") 'mine-scroll-n-lines-up)
(global-set-key (kbd "C-c C-p") 'mine-scroll-n-lines-down)

(global-set-key (kbd "C-c C-H") 'mine-point-to-top)
(global-set-key (kbd "C-c C-L") 'mine-point-to-bottom)

(global-set-key (kbd "C-c C-f") 'mine-advance-to)
(global-set-key (kbd "C-c C-b") 'mine-back-to)

(global-set-key (kbd "C-c C-a") 'mine-increment-number-at-point)
(global-set-key (kbd "C-c C-x") 'mine-decrement-number-at-point)

(global-set-key (kbd "C-c C-Y") 'mine-current-line-to-clipboard)
(global-set-key (kbd "C-c C-P") 'mine-copy-current-line)

(global-set-key (kbd "C-<prior>") 'mine-previous-user-buffer) ; Ctrl+PageDown
(global-set-key (kbd "C-<next>") 'mine-next-user-buffer) ; Ctrl+PageUp

;; just-one-space is mapped to M-SPC, but that opens the window menu
(global-set-key (kbd "C-x C-SPC") 'just-one-space)
(global-set-key (kbd "C-*") 'mine-isearch-forward-at-point)
(global-set-key (kbd "C-c C-$") 'mine-point-to-eol)
(global-set-key (kbd "C-c C-%") 'mine-match-paren)
(global-set-key (kbd "C-6") 'mine-fast-buffer-switch)
(global-set-key (kbd "C-x 9") 'mine-close-buffer-and-window)

(global-set-key [remap backward-up-list] 'mine-backward-up-sexp)

(global-set-key (kbd "C-c a") 'org-agenda-list)
(global-set-key (kbd "C-c c") 'org-capture)

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

;; enable functions that are disabled by default
(put 'narrow-to-region 'disabled nil)

;; reload file if changed on disk
(global-auto-revert-mode 1)

;; display current function name in the mode line
(which-function-mode t)

;; delete trailing whitespace upon saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++ MODE
;; set indentation style for c++-mode
(setq c-default-style "stroustrup"
      c-basic-offset 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG MODE
;; use org-mode for txt files
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
;; org-mode clean view
;; (setq org-startup-indented t)   ; commented out as it produces flickering
; in order to hide the leading stars, set the org-hide face color to background
(setq org-hide-leading-stars t)   ; this is also set with org-startup-indented
;; agenda view of next 14 days
(setq org-agenda-span 14)
(setq org-log-into-drawer t)

(setq org-capture-templates
      '(("m" "movilidad")
        ("ma" "auto" entry (file+headline "~/Dropbox/docs/agenda-personal.org" "auto")
         "* %^T %^{prompt}")
        ("mm" "moto" entry (file+headline "~/Dropbox/docs/agenda-personal.org" "moto")
         "* %^T %^{prompt}")
        ("mu" "monociclo" entry (file+headline "~/Dropbox/docs/agenda-personal.org" "monociclo")
         "* %^T %^{prompt}")
        ("mb" "bici" entry (file+headline "~/Dropbox/docs/agenda-personal.org" "bici")
         "* %^T %^{prompt}")
        ("p" "personal" entry (file+headline "~/Dropbox/docs/agenda-personal.org" "personal")
         "* %^T %^{prompt}")
        ("s" "compras" entry (file+headline "~/Dropbox/docs/agenda-personal.org" "compras")
         "* %^T %^{prompt}")
        ("x" "programming" entry (file+headline "~/Dropbox/docs/agenda-personal.org" "programming")
         "* %^T %^{prompt}")
        ("c" "core" entry (file+headline "~/Dropbox/core/agenda.org" "core")
         "* %^T %^{prompt}")
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LIBRARIES

;; enable ecb
;; (add-to-list 'load-path "~/.emacs.d/ecb-snap")
;; (require 'ecb)

(require 'yasnippet)
(yas/global-mode 1)

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THE FOLLOWING INSTRUCTIONS SHOULD BE PERFORMED LAST,
;; SO MAKE ADDITIONS BEFORE THIS LINE

;; Change the default naming of buffers to include parts of the file name (directory names) until the buffer names are unique.
(require 'uniquify)
    (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
    (setq uniquify-separator "/")
    (setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
    (setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers


;; load initializations for this site
(let ((init-file "~/.emacs.d/init-local.el"))
  (if (file-exists-p init-file)
      (load-file init-file)))

;; In order open a file in an existing emacs from a shell, use
;; emacsclient -n [file]
;; FIXME: avoid starting server if already started
;; the following code extracted from http://stackoverflow.com/questions/3704962/how-to-know-if-emacs-server-is-running dosn't seem to work
(unless (and (boundp 'server-process)
             (memq (process-status server-process) '(connect listen open run)))
  (server-start))

;; load custom file if present
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file 'noerror)
