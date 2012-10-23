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

;; add the site-lisp directory recursively to the load-path variable (needed when compiled only)
(let ((default-directory  "/usr/local/share/emacs/site-lisp"))
     (normal-top-level-add-subdirs-to-load-path))

;; Copy/paste behavior
;; use C-x C-v C-c for copy/pasting
;(cua-mode t)
;; use cua mode for rectangle selection
(cua-selection-mode t)

;; insert matching pairs of brackets
;; (electric-pair-mode)

;; make system copy work with Emacs paste and Emacs copy work with system paste
;; since v24.0 defaults to t
;;(setq x-select-enable-clipboard t)

;; show week number in calendar
(copy-face font-lock-constant-face 'calendar-iso-week-face)
(set-face-attribute 'calendar-iso-week-face nil
                    :height 0.7)
(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'calendar-iso-week-face))
(setq calendar-date-style 'european)         ; dd/mm/yyyy


(setq-default major-mode 'lisp-interaction-mode)

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
(global-hl-line-mode 1)

;; show matching parenthesis
(show-paren-mode t)
(when (require 'highlight-parentheses nil 'noerror)
  (add-hook 'lisp-mode-hook 'highlight-parentheses-mode)
  (add-hook 'lisp-interaction-mode-hook 'highlight-parentheses-mode)
  (add-hook 'scheme-mode-hook 'highlight-parentheses-mode))

(setq hl-paren-colors (quote ("turquoise1" "green" "yellow" "orange")))

;; highlight expression (instead of enclosing parens) when over an openning paren or after closing one
(setq show-paren-style 'expression)

;; hide tool bar and menu bar
(tool-bar-mode -1)
(menu-bar-mode t)

;; show line and column number in the mode line
(line-number-mode 1)
(column-number-mode 1)

;; set scroll bar to the right
;; (setq scroll-bar-mode-explicit t)
;; (set-scroll-bar-mode `right)
(set-scroll-bar-mode nil)

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

(global-set-key (kbd "<f12>") 'view-mode)
(global-set-key (kbd "<Scroll_Lock>") 'scroll-lock-mode)

(global-set-key (kbd "M-S-SPC") 'mine-select-current-line)

(global-set-key (kbd "C-c C-a") 'mine-increment-number-at-point)
(global-set-key (kbd "C-c C-x") 'mine-decrement-number-at-point)

(global-set-key (kbd "C-<prior>") 'mine-previous-user-buffer) ; Ctrl+PageDown
(global-set-key (kbd "C-<next>") 'mine-next-user-buffer)      ; Ctrl+PageUp

(global-set-key (kbd "C-6") 'mine-fast-buffer-switch)
(global-set-key (kbd "<esc>") 'keyboard-quit)
;(global-set-key (remap backward-up-list) 'mine-backward-up-sexp)

(global-set-key (kbd "M-n") 'View-scroll-line-forward)
(global-set-key (kbd "M-p") 'View-scroll-line-backward)

(global-set-key (kbd "C-c a") 'org-agenda-list)
(global-set-key (kbd "C-c c") 'org-capture)

(global-set-key (kbd "M-S-<up>") 'mine-move-text-up)
(global-set-key (kbd "M-S-<down>") 'mine-move-text-down)

(global-set-key (kbd "C-=") 'er/expand-region)

(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-*") 'mark-all-like-this)

(global-set-key (kbd "C-3") 'follow-delete-other-windows-and-split)

(global-set-key (kbd "C-c e") 'esk-eval-and-replace)

;; override backward-char and forward-char
(global-set-key (kbd "C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-f") 'ido-find-file)
;; override kill-sentence
(global-set-key (kbd "M-k") 'mine-close-buffer-and-window)


;; (global-set-key (kbd "<f2>") 'kill-region)    ; cut
;; (global-set-key (kbd "<f3>") 'kill-ring-save) ; copy
;; (global-set-key (kbd "<f4>") 'yank)           ; paste

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

;; more info: http://www.masteringemacs.org/articles/2012/05/29/compiling-running-scripts-emacs/
;; shut up compile saves
(setq compilation-ask-about-save nil)
;; don't save *anything*
(setq compilation-save-buffers-predicate '(lambda () nil))

(setq delete-by-moving-to-trash t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HOOKS

;; delete trailing whitespace upon saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'help-mode-hook '(lambda () (view-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired mode
;; to uncompress a .zip file, add "zip" to the variable 'dired-compress-file-suffixes
(eval-after-load "dired-aux"
   '(add-to-list 'dired-compress-file-suffixes
                 '("\\.zip\\'" ".zip" "unzip")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++ mode
;; set indentation style for c++-mode
(setq c-default-style "stroustrup"
      c-basic-offset 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG mode
;; use org-mode for txt files
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
;; org-mode clean view
;; (setq org-startup-indented t)   ; commented out as it produces flickering
; in order to hide the leading stars, set the org-hide face color to background
(setq org-hide-leading-stars t)   ; this is also set with org-startup-indented
;; agenda view of next 14 days
(setq org-agenda-span 14)
(setq org-log-into-drawer t)

(setq org-agenda-files (quote ("~/Dropbox/core/agenda-core.org"
                               "~/Dropbox/core/notas.org"
                               "~/Dropbox/docs/cumples.org"
                               "~/Dropbox/docs/agenda-personal.org")))

(setq org-capture-templates
      '(("m" "movilidad")
        ("ma" "auto" entry (file+headline "~/Dropbox/docs/agenda-personal.org" "auto")
         "* %^t %^{prompt}")
        ("mm" "moto" entry (file+headline "~/Dropbox/docs/agenda-personal.org" "moto")
         "* %^t %^{prompt}")
        ("mu" "monociclo" entry (file+headline "~/Dropbox/docs/agenda-personal.org" "monociclo")
         "* %^t %^{prompt}")
        ("mb" "bici" entry (file+headline "~/Dropbox/docs/agenda-personal.org" "bici")
         "* %^t %^{prompt}")
        ("p" "personal" entry (file+headline "~/Dropbox/docs/agenda-personal.org" "personal")
         "* %^t %^{prompt}")
        ("s" "compras" entry (file+headline "~/Dropbox/docs/agenda-personal.org" "compras")
         "* %^t %^{prompt}")
        ("x" "programming" entry (file+headline "~/Dropbox/docs/agenda-personal.org" "programming")
         "* %^t %^{prompt}")
        ("c" "core" entry (file+headline "~/Dropbox/core/agenda-core.org" "core")
         "* %^t %^{prompt}")
        ))

;; the appointment notification facility
;; based on http://emacs-fu.blogspot.com.ar/2009/11/showing-pop-ups.html
(setq appt-message-warning-time 15 ;; warn 15 min in advance
      appt-display-mode-line t     ;; show in the modeline
      appt-display-format 'window) ;; use our func
(appt-activate 1)              ;; active appt (appointment notification)
(display-time)                 ;; time display is required for this...

;; update appt each time agenda is opened
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

(defun mine-appt-display (min-to-app new-time msg)
  (mine-popup (format "Appointment in %s minute(s)" min-to-app) msg))
(setq appt-disp-window-function (function mine-appt-display))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LIBRARIES

;; enable ecb
;; (add-to-list 'load-path "~/.emacs.d/ecb-snap")
;; (require 'ecb)

(require 'yasnippet)
(yas/global-mode 1)

(require 'key-chord)
(key-chord-mode 1)
;; preferably, use upper case to avoid delay when typing
;; (key-chord-define-global "BB" 'ido-switch-buffer)

;; (key-chord-define-global "FF" 'ido-find-file)
;; (key-chord-define-global "KK" 'mine-close-buffer-and-window)

;; (key-chord-define-global "SS" 'save-buffer)
(key-chord-define-global "OO" 'other-window)

(key-chord-define-global "FG" 'mine-advance-to)
(key-chord-define-global "FD" 'mine-back-to)
;; (key-chord-define-global "FG" 'iy-go-to-char) ; leaves cursor after the char
;; (key-chord-define-global "FD" 'iy-go-to-char-backward) ; takes cursor to char, but when typing a key it moves to the right 1 char!

(key-chord-define-global "HH" 'mine-point-to-top)
(key-chord-define-global "MM" 'mine-point-to-middle)
(key-chord-define-global "LL" 'mine-point-to-bottom)

(key-chord-define-global "PP" 'mine-copy-current-line)

(key-chord-define-global "**" 'mine-isearch-forward-at-point)
(key-chord-define-global "%%" 'mine-match-paren)

(key-chord-define-global "WW" 'ace-jump-word-mode)
(key-chord-define-global "CC" 'ace-jump-char-mode)
(key-chord-define-global "NN" 'ace-jump-line-mode)

(key-chord-define-global "RR" 'point-to-register)
(key-chord-define-global "JJ" 'jump-to-register)

;; encryption settings
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
(setq org-crypt-key nil)

;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
(setq auto-save-default nil)
;; Auto-saving does not cooperate with org-crypt.el: so you need
;; to turn it off if you plan to use org-crypt.el quite often.
;; Otherwise, you'll get an (annoying) message each time you
;; start Org.
;; To turn it off only locally, you can insert this:
;;
;; # -*- buffer-auto-save-file-name: nil; -*-
;; Excluding the crypt tag from inheritance prevents already encrypted text being encrypted again.

;; latex settings
;; When adding a new environment with C-c C-s, the list will not only provide standard LaTeX environments,
;; but also take your `\documentclass' and `\usepackage' commands into account.
(setq Tex-parse-self t)

;; Enable workgroups if any saved workgroups are found
(require 'workgroups)
(setq wg-morph-on nil)
(let ((file "~/.emacs.d/workgroups"))
  (when (file-exists-p file)
    t))
    ;; (workgroups-mode 1)
    ;; (wg-load file)))

(require 'undo-tree)
(global-undo-tree-mode)

(require 'expand-region)

(require 'wrap-region)
(wrap-region-global-mode t)

(require 'iy-go-to-char)

(require 'mark-more-like-this)

(require 'ace-jump-mode)
(setq ace-jump-mode-case-fold nil)      ; case sensitive jump mode

(require 'starter-kit-defuns)
;; (require 'python-pep8)
;; (require 'python-pylint)

;; load sunrise commander
(require 'sunrise-commander)
(require 'sunrise-x-tree)
(require 'sunrise-x-buttons)

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
