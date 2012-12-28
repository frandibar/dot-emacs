;; MY PERSONAL EMACS CUSTOMIZATION FILE

;; Show the time it took to load this file
(defvar *emacs-load-start-time* (current-time))

(message "Loading my init file...")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ENVIRONMENT SETTINGS

;; Load my functions: `load-path' is a list of directories where Emacs
;; Lisp libraries (`.el' and `.elc' files) are installed.
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

;; Use `list-load-path-shadows' to display a list of external Emacs
;; Lisp files that shadow Emacs builtins (listing potential load path
;; problems).

;; add the site-lisp directory recursively to the load-path variable (needed when compiled only)
(let ((default-directory  "/usr/share/emacs/site-lisp"))
  (normal-top-level-add-subdirs-to-load-path))

;; add the site-lisp directory recursively to the load-path variable (needed when compiled only)
(let ((default-directory  "/usr/local/share/emacs/site-lisp"))
  (normal-top-level-add-subdirs-to-load-path))

;; The `use-package' declaration macro allows isolating package
;; configuration in a way that is performance-oriented and tidy.
(require 'use-package)   ; in order to use `use-package' instead of `require'

;; `use-package' loads a package only if it's available, if not, a
;; warning is logged in the *Messages* buffer. If it succeeds, a
;; message about "Loading foo" is logged, along with the time it took
;; to load if it was over 0.01s.

;; `:init' always happens in the case of deferred modules, `:config'
;; form only run after the module has been loaded by Emacs
;; `use-package' also adds `describe-personal-keybindings'

(setq calendar-date-style 'european)         ; dd/mm/yyyy

(setq-default major-mode 'lisp-interaction-mode)

;; the eshell directory holds alias definitions and history information
(setq eshell-directory-name "~/.emacs.d/eshell")
(setq eshell-cmpl-ignore-case t)
;; by default eshell does completion the Emacs way: cycle through all
;; the possible values. Bash instead complete as much as possible, and
;; then wait for the next charater. This makes eshell behave like Bash
(setq eshell-cmpl-cycle-completions nil)
;; behave more like a terminal
(setq eshell-scroll-to-bottom-on-output t)
;; use ansi-term for these commands, since eshell is not good at
;; ansi-colors and control
(add-hook 'eshell-first-time-mode-hook
          (lambda () (setq eshell-visual-commands
                      (append '("mutt" "vim" "screen" "lftp" "ipython" "telnet" "ssh" "htop")
                              eshell-visual-commands))))


;; this allows (among other things) entering unicode chars in the minibuffer
(setq enable-recursive-minibuffers t)
(setq minibuffer-depth-indicate-mode t)  ;; add depth count to minibuffer (useful when recursive)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEHAVIOR SETTINGS

;; Copy/paste behavior
;; use C-x C-v C-c for copy/pasting
;;(cua-mode t)
;; use cua mode for rectangle selection
(cua-selection-mode t)
(setq cua-keep-region-after-copy t)

;; insert matching pairs of brackets
;; (electric-pair-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; APPEARANCE SETTINGS

;; set window title to buffer-file-name
(setq frame-title-format '("" "emacs - %b - " buffer-file-name))

(use-package color-theme-zenburn
  :config
  (color-theme-zenburn))

;; highlight cursor line
(global-hl-line-mode 1)

;; show matching parentheses
(show-paren-mode t)

(use-package highlight-parentheses
  :config
  (progn
    (add-hook 'lisp-mode-hook 'highlight-parentheses-mode)
    (add-hook 'lisp-interaction-mode-hook 'highlight-parentheses-mode)
    (add-hook 'scheme-mode-hook 'highlight-parentheses-mode)
    ;; highlight expression (instead of enclosing parens)
    ;; when over an opening paren or after closing one
    (setq hl-paren-colors (quote ("turquoise1" "green" "yellow" "orange")))
    (setq show-paren-style 'expression)
    ))

;; hide tool bar and menu bar and scroll bar
(tool-bar-mode -1)
(menu-bar-mode t)
(set-scroll-bar-mode nil)

;; show line and column number in the mode line
(line-number-mode 1)
(column-number-mode 1)

;; show line numbers on left
(global-linum-mode 1)

;; hide splash screen
(setq inhibit-splash-screen t)

;; show blank screen on startup
(setq initial-scratch-message nil)

;; set font size
(set-face-attribute 'default nil :height 110)

;; highlight tabs and trailing spaces
(setq whitespace-style '(tabs trailing space-before-tab newline indentation empty space-after-tab tab-mark))
;;(global-whitespace-mode 1)

;; interactive name completion for describe-function, describe-variable, etc.
(icomplete-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEYBINDINGS

;; When possible, they should start with C-c, and then control again to avoid race
;; condition between first and second keys.
;; This guarantees that they won't be overriden by any mode.

(global-set-key (kbd "<f12>") 'view-mode)
(global-set-key (kbd "<Scroll_Lock>") 'scroll-lock-mode)

(global-set-key (kbd "<esc>") 'keyboard-quit)

(global-set-key (kbd "M-n") 'View-scroll-line-forward)
(global-set-key (kbd "M-p") 'View-scroll-line-backward)

(global-set-key (kbd "C-c a") 'org-agenda-list)
(global-set-key (kbd "C-c c") 'org-capture)

(global-set-key (kbd "C-3") 'follow-delete-other-windows-and-split)

;; override backward-char and forward-char
(global-set-key (kbd "C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-f") 'ido-find-file)

;; (global-set-key (kbd "<f2>") 'kill-region)    ; cut
;; (global-set-key (kbd "<f3>") 'kill-ring-save) ; copy
;; (global-set-key (kbd "<f4>") 'yank)           ; paste

;; by default, emacs binds M-z to zap-to-char. I prefer binding it to
;; zap-up-to-char, but the latter is not loaded by default (it's in
;; misc.el instead of simple.el)
(autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)


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
;;(setq backup-inhibited t)

;; write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; make backups of files, even when they're in version control
(setq vc-make-backup-files t)

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

;; ignore case when reading a file name completion
(setq read-file-name-completion-ignore-case t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired mode
;; to uncompress a .zip file, add "zip" to the variable 'dired-compress-file-suffixes
(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
                '("\\.zip\\'" ".zip" "unzip")))

;; if there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer.
(setq dired-dwim-target t)

;; enable disabled function
(put 'dired-find-alternate-file 'disabled nil)

;; make Enter and ^ (parent dir) to use the same buffer
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "<return>")
              'dired-find-alternate-file) ; was dired-advertised-find-file
            (define-key dired-mode-map (kbd "^")
              (lambda () (interactive) (find-alternate-file "..")))
                                        ; was dired-up-directory
            ))

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
;; EXTERNAL LIBRARIES

;; FIXME: had to use require, if not only the keybinded functions are loaded
(require 'myfuncs)
(use-package myfuncs
  :defer nil                           ; this doesn't work
  :bind (("M-S-SPC" . mine-select-current-line)
         ("M-S-<up>" . mine-move-text-up)
         ("M-S-<down>" . mine-move-text-down)

         ("C-c C-a" . mine-increment-number-at-point)
         ("C-c C-x" . mine-decrement-number-at-point)

         ("C-<prior>" . mine-previous-user-buffer) ; Ctrl+PageDown
         ("C-<next>" . mine-next-user-buffer)      ; Ctrl+PageUp
         ("C-6" . mine-fast-buffer-switch)
         ("M-k" . mine-close-buffer-and-window) ; override kill-sentence

         ("C-c e" . mine-eval-and-replace)
         ("M-C" . mine-toggle-case)
         ("C-x $" . mine-toggle-folding-level)  ;; overrides set-selective-display
         )
  :init
  (progn
    ;; view greek letter lambda
    (add-hook 'emacs-lisp-mode-hook 'mine-greek-lambda)
    (add-hook 'python-mode-hook 'mine-greek-lambda)
    ))

(use-package yasnippet
  :disabled t                ; takes too long to load and I don't use it
  :commands yasnippet
  :config
  (yas/global-mode 1))

(use-package key-chord
  :config
  (progn
    (key-chord-mode 1)
    ;; preferably, use upper case to avoid delay when typing
    (key-chord-define-global "OO" 'other-window)

    (key-chord-define-global "FG" 'mine-advance-to)
    (key-chord-define-global "FD" 'mine-back-to)

    (key-chord-define-global "HH" 'mine-point-to-top)
    (key-chord-define-global "MM" 'mine-point-to-middle)
    (key-chord-define-global "LL" 'mine-point-to-bottom)

    (key-chord-define-global "PP" 'mine-copy-current-line)

    (key-chord-define-global "**" 'mine-isearch-forward-at-point)
    (key-chord-define-global "%%" 'mine-match-paren)

    (key-chord-define-global "RR" 'point-to-register)
    (key-chord-define-global "JJ" 'jump-to-register)
    ))

;; encryption settings
(use-package org-crypt
  :commands (org-decrypt-entries
             org-encrypt-entries
             org-crypt-use-before-save-magic)
  :config
  (progn
    (org-crypt-use-before-save-magic)
    (setq org-tags-exclude-from-inheritance (quote ("crypt")))
    (setq org-crypt-key nil)
    ))

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
(use-package workgroups
  :disabled t                           ; not using it
  :config
  (progn
    (setq wg-morph-on nil)
    (let ((file "~/.emacs.d/workgroups"))
      (when (file-exists-p file)
        (workgroups-mode 1)
        (wg-load file)))
    ))

(use-package undo-tree
  :config
  (global-undo-tree-mode))

;; Expand region increases the selected region by semantic units.
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Wrap Region is a minor mode for Emacs that wraps a region with
;; punctuations. For example select a region and press `(' to wrap it
;; around parentheses.  For "tagged" markup modes, such as HTML and
;; XML, it wraps with tags.
(use-package wrap-region
  :config
  (wrap-region-global-mode t))

;; An extension that lets you mark several regions at once.
(use-package mark-more-like-this
  :bind (("C-<" . mark-previous-like-this)
         ("C->" . mark-next-like-this)
         ("C-*" . mark-all-like-this)))

;; Ace jump mode is a minor mode of emacs, which help you to move the
;; cursor to **ANY** position (across window and frame) using at most
;; 4 key presses.
;; key-bindings:
;;   C-. jump to word
;;   C-u C-. jump to char
;;   C-u C-u C-. jump to line
(use-package ace-jump-mode
  :bind ("C-." . ace-jump-mode)
  :config
  (setq ace-jump-mode-case-fold nil))      ; case sensitive jump mode

(use-package ipython
  :disabled t                           ; TODO
  :config
  (progn
    (use-package starter-kit-defuns)
    (use-package python-pep8)
    (use-package python-pylint)))

;; load sunrise commander, a mix between dired and midnight commander.
(use-package sunrise-commander
  :commands sunrise
  :config
  (progn
    (use-package sunrise-x-tree)
    (use-package sunrise-x-buttons)))

;; show bookmarks on startup
(use-package bookmark
  :config
  (progn
    (bookmark-bmenu-list)
    (switch-to-buffer "*Bookmark List*")
    ))

;; Change the default naming of buffers to include parts of the file name (directory names) until the buffer names are unique.
(use-package uniquify
  :config
  (progn
    (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
    (setq uniquify-separator "/")
    (setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
    (setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
    ))

;; toggles between the shell buffer and current buffer
(use-package shell-toggle-patched
  :config
  (progn
    (autoload 'shell-toggle "shell-toggle"
      "Toggles between the shell buffer and whatever buffer you are editing." t)
    (autoload 'shell-toggle-cd "shell-toggle"
      "Pops up a shell-buffer and insert a \"cd <file-dir>\" command." t)
    (setq shell-toggle-launch-shell 'shell-toggle-eshell)
    (global-set-key (kbd "<f5>") 'shell-toggle)
    ))

;; slime for common lisp
(use-package slime
  :disabled t                           ; not using it
  :commands slime-setup
  :config
  (progn
    (setq inferior-lisp-program "/usr/bin/clisp")
    (slime-setup '(slime-fancy))
    ))

;; load initializations for this site
(use-package init-local)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HOOKS

;; delete trailing whitespace upon saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; view-mode
(add-hook 'help-mode-hook '(lambda () (view-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THE FOLLOWING INSTRUCTIONS SHOULD BE PERFORMED LAST,
;; SO MAKE ANY ADDITIONS BEFORE THIS LINE

;; In order open a file in an existing emacs from a shell, use
;; emacsclient -n [file]
;; FIXME: avoid starting server if already started
;; the following code extracted from http://stackoverflow.com/questions/3704962/how-to-know-if-emacs-server-is-running dosn't seem to work

;; (unless (and (boundp 'server-process)
;;              (memq (process-status server-process) '(connect listen open run)))
;;   (server-start))

;; allow editing text boxes from clients such as google chrome (requires "Edit with emacs" extension)
(use-package edit-server
  :if window-system
  :init
  (progn
    (add-hook 'after-init-hook 'server-start t)
    (add-hook 'after-init-hook 'edit-server-start t)))

;; load custom file if present
(setq custom-file "~/.emacs.d/emacs-custom.el")
(load custom-file 'noerror)

;; show load time in *Messages* buffer
(message "My init file loaded in %ds" (destructuring-bind (hi lo ms) (current-time)
                                        (- (+ hi lo) (+ (first *emacs-load-start-time*) (second *emacs-load-start-time*)))))
