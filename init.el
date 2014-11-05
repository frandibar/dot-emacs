;;; package --- init.el

;;; Commentary:
;; My Emacs customization file.

;;; Code:

(message "Loading init file...")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ENVIRONMENT SETTINGS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Load my functions: `load-path' is a list of directories where Emacs
;; Lisp libraries (`.el' and `.elc' files) are installed.
;(add-to-list 'load-path user-emacs-directory)

(let ((default-directory user-emacs-directory))
  (normal-top-level-add-subdirs-to-load-path))

;; Use `list-load-path-shadows' to display a list of external Emacs
;; Lisp files that shadow Emacs builtins (listing potential load path
;; problems).

;; Package Manager
;; See ~Cask~ file for its configuration.
;; https://github.com/cask/cask
;; On OSX it's necessary to manually add the .cask directory
(let ((default-directory (concat user-emacs-directory ".cask")))
  (normal-top-level-add-subdirs-to-load-path))

(require 'cask)
(cask-initialize)

;; The `use-package' declaration macro allows isolating package
;; configuration in a way that is performance-oriented and tidy.
;; (require 'use-package) ; in order to use `use-package' instead of
;; `require'

;; `use-package' loads a package only if it's available, if not, a
;; warning is logged in the *Messages* buffer. If it succeeds, a
;; message about "Loading foo" is logged, along with the time it took
;; to load if it was over 0.01s.

;; `:init' always happens in the case of deferred modules, `:config'
;; form only run after the module has been loaded by Emacs. You should
;; keep `:init' forms as simple as possible, and put as much as you
;; can get away with on the `:config' side.

;; `use-package' also adds `describe-personal-keybindings'.

(require 'use-package)

;; Keep Cask file in sync with the packages.
;; installed/uninstalled via `list-packages'.
(use-package pallet)


;; OS specific settings.

(setq shell-file-name "/bin/bash")
(setq explicit-shell-file-name "/bin/bash")

;; On OS X the $PATH environment variable and `exec-path' used by a
;; windowed Emacs instance will usually be the system-wide default
;; path, rather than that seen in a terminal window.
(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :config
    (progn
      (exec-path-from-shell-initialize))))

(defvar calendar-date-style 'european)  ; dd/mm/yyyy

;; These options are specific to Emacs Mac Port
;; Extracted from URL `https://gist.github.com/railwaycat/3498096'

;; mac switch meta key
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

;; Standard emacs has these keybindings already defined.
(global-set-key [(super a)] 'mark-whole-buffer)
(global-set-key [(super v)] 'yank)
(global-set-key [(super c)] 'kill-ring-save)
(global-set-key [(super x)] 'kill-region)
(global-set-key [(super s)] 'save-buffer)
(global-set-key [(super l)] 'goto-line)
(global-set-key [(super w)]
                (lambda () (interactive) (delete-window)))
(global-set-key [(super z)] 'undo)
(global-set-key [(super k)] 'kill-this-buffer)
(global-set-key [(super q)] 'save-buffers-kill-terminal)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEHAVIOR SETTINGS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use cua mode for rectangle selection.
(cua-selection-mode t)

;; Sentences end with single space, so this fixes sentence navigation
;; commands.
(setq sentence-end-double-space nil)

;; Turn on winner mode, to allow restoring window configuration with
;; C-c ← and C-c →
(winner-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; APPEARANCE SETTINGS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Hide tool bar and scroll bar, show the menu bar.
(menu-bar-mode t)
(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

;; Hide splash screen.
(setq inhibit-splash-screen t)

;; Set window title to buffer-file-name.
(setq frame-title-format '("" "Emacs - %b - " buffer-file-name))

;; Highlight cursor line.
(global-hl-line-mode 1)

;; Flash the frame upon C-g.
(setq visible-bell t)
(setq ring-bell-function #'ignore)      ; avoid beep

;; Show matching parentheses.
(show-paren-mode t)

;; Show line and column number in the mode line.
(line-number-mode 1)
(column-number-mode 1)

;; Show blank screen on startup (*Scratch* buffer).
(setq initial-scratch-message nil)

;; Set font size.
(set-face-attribute 'default nil :height 110)

;; Highlight tabs and trailing spaces.
(defvar whitespace-style '(tabs trailing space-before-tab newline indentation empty space-after-tab tab-mark))
(global-whitespace-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; KEYBINDINGS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; When possible, they should start with C-c, and then C- again to avoid race
;; condition between first and second keys.
;; This guarantees that they won't be overriden by any mode.

(global-set-key (kbd "s-n") 'scroll-up-line)
(global-set-key (kbd "s-p") 'scroll-down-line)

(global-set-key (kbd "C-c a") 'org-agenda-list)
(global-set-key (kbd "C-c c") 'org-capture)

;; Useful for jumping to function definitions in buffer.
(global-set-key (kbd "s-i") 'imenu)

;; By default, emacs binds M-z to `zap-to-char'. I prefer binding it to
;; `zap-up-to-char', but the latter is not loaded by default (it's in
;; misc.el instead of simple.el).
(autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

;; Same thing with `forward-to-word'.
(autoload 'forward-to-word "misc" "Move forward until encountering the beginning of a word.")
(global-set-key (kbd "M-f") 'forward-to-word)  ; TODO: use subword variant (subword-forward leaves cursor before word)
;(global-set-key (kbd "M-f") 'subword-forward)

;; Use these keybindings for window switching since default ones
;; (windmove-default-keybindings) S-{up,down,left,right}
;; interfere with org and calendar.
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

;; Change default behavior of RET to indent according to major mode.
(global-set-key (kbd "RET") 'newline-and-indent)

;; On OSX, s-x is mapped to `kill-region' so there is no need to remap.
(global-set-key (kbd "C-w") 'backward-kill-word) ; override `kill-region'

(use-package python
  :init
  (progn
    (defvar inferior-python-mode-map)
    ;; Behave like in a terminal.
    (define-key inferior-python-mode-map (kbd "C-S-p") 'comint-previous-input)
    (define-key inferior-python-mode-map (kbd "C-S-n") 'comint-next-input)
    ;; for some unknown reason using :bind doesn't work
    (define-key python-mode-map (kbd "C-M-<") 'python-indent-shift-left)
    (define-key python-mode-map (kbd "C-M->") 'python-indent-shift-right)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MISC
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Turn off tab character.
;; Emacs normally uses both tabs and spaces to indent lines. To use only spaces
;; set `indent-tabs-mode' to `nil'. This is a per-buffer variable;
;; altering the variable affects only the current buffer, but it can be
;; disabled for all buffers.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Write backup files to own directory.
;; To disable backups, use (setq backup-inhibited t).
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control.
;;(setq vc-make-backup-files t)

;; Disable auto save.
(setq auto-save-default nil)

;; Use 'y' or 'n' instead of 'yes' or 'no' for answers.
(fset 'yes-or-no-p 'y-or-n-p)

;; Enable functions that are disabled by default.
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Reload file if changed on disk.
(global-auto-revert-mode 1)

;; Display current function name in the mode line.
(which-function-mode t)

;; Shut up compile saves.
;; More info: http://www.masteringemacs.org/articles/2012/05/29/compiling-running-scripts-emacs/
(setq compilation-ask-about-save nil)
;; Don't save *anything*.
(defvar compilation-save-buffers-predicate '(lambda () nil))

(setq delete-by-moving-to-trash t)

;; Ignore case when reading a file name completion.
(setq read-file-name-completion-ignore-case t)

;; Settings for all programming modes.

;; Show line numbers.
(add-hook 'prog-mode-hook (lambda () (linum-mode 1)))
;; Adjust line number width.
;;(defvar linum-format "%4d")

;; Enable separation of camel case words.
(add-hook 'prog-mode-hook 'subword-mode)

;; Emacs frontend to GNU Global source code tagging system.
(use-package ggtags
  :diminish " §"
  :config
  (add-hook 'prog-mode-hook 'ggtags-mode))

;; Set mode for unknown Fish shell configuration files.
(add-to-list 'auto-mode-alist '("\\.fish\\'" . conf-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired mode
(use-package dired
  :config
  (progn
    ;; Show all, long, no group, human readable file size.
    ;; If there is a dired buffer displayed in the next window, use
    ;; its current subdir as target, instead of the current subdir of
    ;; this dired buffer.
    (setq dired-dwim-target t)

    ;; Allow dired to be able to delete or copy a whole dir.  "always"
    ;; means no asking. "top" means ask once. Any other symbol means ask
    ;; each and every time for a dir and subdir.
    (setq dired-recursive-copies (quote always))
    (setq dired-recursive-deletes (quote top))

    ;; ;; make Enter and ^ (parent dir) to use the same buffer
    ;; (add-hook 'dired-mode-hook
    ;;           (lambda ()
    ;;             (define-key dired-mode-map (kbd "<return>")
    ;;               'dired-find-alternate-file)               ; was dired-advertised-find-file
    ;;             (define-key dired-mode-map (kbd "^")
    ;;               (lambda () (interactive) (find-alternate-file ".."))) ; was dired-up-directory
    ;;             ))


    ;; use M-o to toggle viewing files
    (setq dired-omit-files
          (rx (or (seq bol (? ".") "#")         ;; emacs autosave files
                  (seq bol "." (not (any "."))) ;; dot-files
                  (seq bol ".pyc" eol)          ;; python compiled
                  (seq bol ".pyo" eol)          ;; python object
                  (seq bol ".o" eol))))         ;; object files

    (setq dired-listing-switches "-alh")))

(use-package dired-x)  ; makes dired-jump available with C-x C-j from the start
;; Extensions to Dired.
;; (use-package dired+)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG mode
;; Outline-based notes management and organizer.
(use-package org
  :config
  (progn
    ;; use org-mode for txt files
    (add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
    ;; org-mode clean view
    ;; (setq org-startup-indented t)   ; commented out as it produces flickering
                                       ; in order to hide the leading stars, set the org-hide face color to background
    (setq org-hide-leading-stars t)   ; this is also set with org-startup-indented
    ;; agenda view of next 14 days
    (defvar org-agenda-span 14)
    (setq org-log-into-drawer t)

    (setq org-agenda-files (quote ("~/Dropbox/docs/cumples.org"
                                   "~/Dropbox/docs/agenda-personal.org"
                                   "~/xapo/agenda.org")))

    (defvar org-capture-templates
          '(("m" "movilidad")
            ("ma" "auto" entry (file+headline "~/Dropbox/docs/agenda-personal.org" "auto")
             "* %^t %^{prompt}")
            ("mm" "moto" entry (file+headline "~/Dropbox/docs/agenda-personal.org" "moto")
             "* %^t %^{prompt}")
            ("p" "personal" entry (file+headline "~/Dropbox/docs/agenda-personal.org" "personal")
             "* %^t %^{prompt}")
            ("h" "salud" entry (file+headline "~/Dropbox/docs/agenda-personal.org" "salud")
             "* %^t %^{prompt}")
            ("x" "xapo" entry (file+headline "~/xapo/agenda.org" "xapo")
             "* %^t %^{")
            ))

    ;;     ;; the appointment notification facility
    ;;     ;; based on http://emacs-fu.blogspot.com.ar/2009/11/showing-pop-ups.html
    ;;     (setq appt-message-warning-time 15 ; warn 15 min in advance
    ;;           appt-display-interval 5      ; repeat every 5 min
    ;;           appt-display-mode-line t     ; show in the modeline
    ;;           appt-display-format 'window) ; use our func
    ;;     (appt-activate 1)              ; active appt (appointment notification)
    ;;     (display-time)                 ; time display is required for this...

    ;;     ;; update appt each time agenda is opened
    ;;     (add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)
    ))

;; Show function arglist or variable docstring in echo area.
(use-package eldoc
  :diminish eldoc-mode
  :config
  (progn
    (dolist (mode '(lisp-interaction-mode-hook
                    emacs-lisp-mode-hook))
      (add-hook mode 'turn-on-eldoc-mode))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; EXTERNAL LIBRARIES
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; FIXME: had to use require, if not only the keybinded functions are loaded
(require 'myfuncs)
(use-package myfuncs
  :defer nil                                       ; this doesn't work
  :bind (("s-S-SPC" . mine-select-current-line)

         ("C-c C-a" . mine-increment-number-at-point)
         ("C-c C-x" . mine-decrement-number-at-point)

         ("C-<prior>" . mine-previous-user-buffer) ; Ctrl+PageDown
         ("C-<next>" . mine-next-user-buffer)      ; Ctrl+PageUp
         ("C-5" . mine-match-paren)                ; similar to vim's %
         ("C-6" . mine-fast-buffer-switch)         ; similar to vim's ^
         ("C-8" . mine-isearch-forward-at-point)   ; similar to vim's *
         ;; ("M-k" . mine-close-buffer-and-window)    ; override kill-sentence
         ("M-C" . mine-toggle-case)
         ;; ("C-x $" . mine-toggle-folding-level)  ;; overrides set-selective-display
         ))

(use-package starter-kit-defuns
  :bind (("C-c e" . esk-eval-and-replace)))

;; Encryption settings.
(use-package org-crypt
  ;; create autoloads
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
;; # -*- buffer-auto-save-file-name: nil; -*- Excluding the crypt tag
;; from inheritance prevents already encrypted text being encrypted
;; again.

;; Treat undo history as a tree.
(use-package undo-tree
  :diminish " τ"
  :idle
  ;; enable mode
  (global-undo-tree-mode))

;; Increase selected region by semantic units.
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Wrap text with punctation or tag.
;; For example select a region and press left paren to wrap it around
;; parentheses. For "tagged" markup modes, such as HTML and XML, it
;; wraps with tags.
(use-package wrap-region
  :diminish " ω"
  :config
  (wrap-region-global-mode t))

;; Move the cursor to **ANY** position (across window and frame) using
;; at most 4 key presses.
;; A quick cursor location minor mode.
(use-package ace-jump-mode
  :bind ("<f11>" . ace-jump-word-mode)
  :config
  (setq ace-jump-mode-case-fold nil))   ; case sensitive jump mode

(use-package auto-complete
  :diminish " ac")

;; Modern on-the-fly syntax checking.
(use-package flycheck
  :diminish " f"
  :config
  (global-flycheck-mode))

;; TODO: this should go inside the use-package
(add-hook 'python-mode-hook
          (lambda ()
            (flycheck-mode)
            (flycheck-set-checker-executable 'python-flake8 "/usr/local/bin/flake8")
            (flycheck-select-checker 'python-flake8)))

;; Emacs Python Development Environment.
(use-package elpy
  ;; Default bindings are overriden by smartscan
  :bind (("M-C-n" . elpy-nav-forward-definition)    ; default M-n
         ("M-C-p" . elpy-nav-backward-definition))  ; default M-p
  :config
  (progn
    (setq elpy-rpc-backend "jedi")
    (elpy-use-ipython)
    (setq python-shell-interpreter-args "-i --colors Linux")
    (setq python-shell-interpreter "/usr/local/bin/ipython")
    (setq elpy-default-minor-modes '(eldoc-mode
                                     yas-minor-mode
                                     auto-complete-mode))))
(elpy-enable)

;; Change the default naming of buffers to include parts of the file
;; name (directory names) until the buffer names are unique.
(use-package uniquify
  :config
  (progn
    (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
    (setq uniquify-separator "/")
    (setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
    (setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers
    ))

;; Make M-. and M-, work in elisp like they do in slime.
;; Slime allows navigation to the symbol at point (using M-.), and the
;; ability to pop back to previous marks (using M-,).  This plugin
;; provides similar navigation for Emacs Lisp, supporting navigation
;; to the definitions of variables, functions, libraries and faces.
(use-package elisp-slime-nav
  :diminish " s"
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t)))
    ))

;; Minor mode for editing parentheses.
(use-package paredit
  :diminish " ρ"
  :config
  (progn
    (dolist (mode '(clojure-mode-hook
                    hy-mode-hook
                    emacs-lisp-mode-hook
                    lisp-interaction-mode-hook))
      (add-hook mode 'paredit-mode))))

;; Control Git from Emacs.
(use-package magit
  :bind (("M-g s" . magit-status)))

;; git-gutter.el does not work with linum-mode but
;; git-gutter-fringe.el can work with linum-mode. In contrast,
;; git-gutter-fringe.el does not work in tty frame(emacs -nw), but
;; git-gutter.el can work in tty frame.
(if window-system                       ; it's nil in console
    (use-package git-gutter-fringe
      :diminish git-gutter-mode
      :config
      ;;(setq git-gutter-fr:side 'right-fringe)
      (global-git-gutter-mode 1))
  (use-package git-gutter
    :diminish git-gutter-mode
    :config
    (global-git-gutter-mode 1)))

;; Writable grep buffer and apply the changes to files.
(use-package wgrep)

;; In dired mode, matching files are opened in external program.
;; (use-package openwith
;;   :init
;;   (progn
;;     (setq openwith-associations
;;           '(("\\.pdf\\'" "evince" (file))
;;             ("\\.mp3\\'" "xmms" (file))
;;             ("\\.sgf\\'" "quarry" (file))
;;             ("\\.\\(?:mpe?g\\|avi\\|wmv\\|flv\\|mp4\\|mov\\|3gp\\|ogv\\)\\'" "mplayer" ("-idx" file))
;;             ("\\.\\(?:jp?g\\|png\\)\\'" "display" (file)))
;;           )
;;     (add-hook 'dired-mode-hook (lambda () (openwith-mode t)))
;;     ;(openwith-mode t)
;;     ))

;; Manage and navigate projects in Emacs easily.
;; Enable project management for all modes.
;; Root dir must have a file named .projectile to be considered a project
;; except for git repos. This file has entries with patterns to ignore files.
(use-package projectile
  :diminish " j"
  :config
  (projectile-global-mode)
  :init
  ;; Yet another front-end for ack.
  (use-package ack-and-a-half
    :config
    (setq ack-and-a-half-executable "/usr/local/bin/ack"))
  )

;; Move the region in 4 directions, in a way similar to Eclipse's.
(use-package shift-text
  :bind (("M-S-<up>" . shift-text-up)
         ("M-S-<down>" . shift-text-down)
         ("M-S-<right>" . shift-text-right)
         ("M-S-<left>" . shift-text-left)))

(use-package multiple-cursors
  :bind (("M-m" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-;" . mc/mark-all-like-this)) ; binding used by iedit
  )

;; Major mode for editing html templates.
(use-package web-mode
  :config
  (progn
    (dolist (mode '("\\.html?\\'"
                    "\\.xml\\'"))
      (add-to-list 'auto-mode-alist `(,mode . web-mode)))))

;; Tools and minor mode to trim whitespace on text lines.
(use-package ws-trim
  :diminish ws-trim-mode
  :config
  (progn
    (setq-default ws-trim-level 1)   ; only modified lines are trimmed
    (global-ws-trim-mode 1))
  )

;; Helm is an Emacs incremental and narrowing framework.
(use-package helm
  :defer t
  :bind (("<f12>" . helm-mini))
  :init
  (progn
    (use-package helm-projectile)
;    (use-package helm-git)

    ;; GNU GLOBAL helm interface.
    (use-package helm-gtags
      :diminish helm-gtags-mode
      :config
      (progn
        (add-hook 'prog-mode-hook 'helm-gtags-mode))
      :init
      (progn
        (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
        (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
        (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
        (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
        (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
        (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
        (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)))

    (use-package helm-config
      :config
      (progn
        ;;(autoload 'helm-mode "helm-config")
        (define-key global-map [remap execute-extended-command] 'helm-M-x)
        (define-key global-map [remap find-file] 'helm-find-files)
        (define-key global-map [remap occur] 'helm-occur)
        (define-key global-map [remap list-buffers] 'helm-buffers-list)
        (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
        (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point)

        ;; Instead of using full screen all the time, accomplished with (setq helm-full-frame t),
        ;; Type s-t in helm mode to expand helm buffer.
        (define-key helm-map [(super t)] 'mine-make-helm-full-frame)

        ;; In OSX, use mdfind instead of locate.
        (if (eq system-type 'darwin)
            (setq helm-locate-command "mdfind %s %s"))
        ;; Ignore some files...
        (cl-loop for ext in '("\\.elc$" "\\.pyc$")
              do (add-to-list 'helm-boring-file-regexp-list ext))
        (global-set-key [(super t)] 'helm-for-files)

        ;; Avoid showing up when using winner mode.
        (add-hook 'helm-before-initialize-hook #'(lambda () (winner-mode -1)))
        (add-hook 'helm-cleanup-hook #'(lambda () (winner-mode 1)))
        ))))

;; Edit a single list.
(use-package edit-list)

;; Building Regexps with visual feedback.
(use-package re-builder
  :config
  (progn
    ;; Avoid double backlash escaping when editing regexp.
    ;; See http://www.masteringemacs.org/articles/2011/04/12/re-builder-interactive-regexp-builder/
    (setq reb-re-syntax 'string)))

;; Jumps between other symbols found at point.
(use-package smartscan
  :config
  (global-smartscan-mode 1))

;; Customize the mode line.
(use-package powerline
  :config
  (progn
    ;; Extra mode line faces.
    (make-face 'mode-line-read-only-face)
    (make-face 'mode-line-modified-face)
    (make-face 'mode-line-folder-face)
    (make-face 'mode-line-filename-face)
    (make-face 'mode-line-position-face)
    (make-face 'mode-line-mode-face)
    (make-face 'mode-line-minor-mode-face)
    (make-face 'mode-line-process-face)
    (make-face 'mode-line-80col-face)

    (set-face-attribute 'mode-line-inactive nil
                        :foreground "gray60" :background "gray20"
                        :inverse-video nil
                        :box '(:line-width 6 :color "gray20" :style nil))
    (set-face-attribute 'mode-line nil
                        :foreground "gray80" :background "gray40"
                        :inverse-video nil
                        :box '(:line-width 6 :color "gray40" :style nil))

    (set-face-attribute 'mode-line-read-only-face nil
                        :inherit 'mode-line-face
                        :foreground "#4271ae"
                        :box '(:line-width 2 :color "#4271ae"))
    (set-face-attribute 'mode-line-modified-face nil
                        :inherit 'mode-line-face
                        :foreground "#c82829"
                        :background "#ffffff"
                        :box '(:line-width 2 :color "#c82829"))
    (set-face-attribute 'mode-line-folder-face nil
                        :inherit 'mode-line-face
                        :foreground "gray60")
    (set-face-attribute 'mode-line-filename-face nil
                        :inherit 'mode-line-face
                        :foreground "#eab700"
                        :weight 'bold)
    (set-face-attribute 'mode-line-position-face nil
                        :inherit 'mode-line-face
                        :family "Menlo" :height 100)
    (set-face-attribute 'mode-line-mode-face nil
                        :inherit 'mode-line-face
                        :foreground "gray80")
    (set-face-attribute 'mode-line-minor-mode-face nil
                        :inherit 'mode-line-mode-face
                        :foreground "gray40"
                        :height 110)
    (set-face-attribute 'mode-line-process-face nil
                        :inherit 'mode-line-face
                        :foreground "#718c00")
    (set-face-attribute 'mode-line-80col-face nil
                        :inherit 'mode-line-position-face
                        :foreground "black" :background "#eab700")

    (setq powerline-default-separator 'zigzag)

    (defun mine-powerline-theme ()
      "Theme based on powerline-default-theme"
      (interactive)
      (setq-default mode-line-format
                    '("%e"
                      (:eval
                       (let* ((active (powerline-selected-window-active))
                              (mode-line (if active 'mode-line 'mode-line-inactive))
                              (face1 (if active 'powerline-active1 'powerline-inactive1))
                              (face2 (if active 'powerline-active2 'powerline-inactive2))
                              (separator-left (intern (format "powerline-%s-%s"
                                                              powerline-default-separator
                                                              (car powerline-default-separator-dir))))
                              (separator-right (intern (format "powerline-%s-%s"
                                                               powerline-default-separator
                                                               (cdr powerline-default-separator-dir))))
                              (lhs (list (powerline-raw "%*" nil 'l)  ; print flags % if read only, * if modified
                                         ;(powerline-raw mode-line-mule-info nil 'l)
                                         ;; line : column
                                         (powerline-raw " %l:%c " face2)
                                         (funcall separator-left face2 face1)
                                         (powerline-buffer-id face1 'l)
                                         (funcall separator-left face1 face2)
                                         (when (and (boundp 'which-func-mode) which-func-mode)
                                           (powerline-raw (nth 1 which-func-format) nil 'l))  ; use nth to remove brackets
                                         (powerline-raw " ")
                                         (funcall separator-left mode-line face1)
                                         (when (boundp 'erc-modified-channels-object)
                                           (powerline-raw erc-modified-channels-object face1 'l))
                                         (powerline-major-mode face1 'l)
                                         (powerline-process face1)
                                         (powerline-minor-modes face1 'l)
                                         (powerline-narrow face1 'l)
                                         (powerline-raw " " face1)
                                         (funcall separator-left face1 face2)
                                         (powerline-vc face2 'r)
                                         (powerline-raw " " face2)
                                         (powerline-raw global-mode-string) ; this is to show messages filter in mu4e
                                         ))
                              (rhs (list (powerline-raw "%p" nil 'r)
                                         (powerline-buffer-size nil 'l)
                                         (powerline-raw " ")
                                         ;; TODO: I want to display message icon but not the time
                                        ;(powerline-raw display-time-string)  ; to show new message icon
                                         )))
                         (concat (powerline-render lhs)
                                 (powerline-fill face2 (powerline-width rhs))
                                 (powerline-render rhs)))))))
    (mine-powerline-theme)
    ))

;; Discover more of Emacs.
(use-package discover
  :config
  (global-discover-mode 1))

;; Guide the following key bindings automatically and dynamically.
(use-package guide-key
  :diminish guide-key-mode
  :config
  (progn
    (guide-key-mode 1)
    (setq guide-key/idle-delay 0.5)
    (setq guide-key/recursive-key-sequence-flag t)
    (setq guide-key/guide-key-sequence
          '("C-c" "C-x"
            (org-mode "C-c C-x")))))

;; A regexp/replace command for Emacs with interactive visual feedback.
(use-package visual-regexp
  :config
  (progn
    (define-key global-map (kbd "M-%") 'vr/query-replace))) ; override default keybinding for query-replace

;; Prettify tabs for tabbar-mode.
;; (use-package tabbar-ruler
;;   ;; These bindings are for mac: M-s stand for left hand (alt + command)
;;   :bind (("M-s-<right>" . tabbar-ruler-forward)
;;          ("M-s-<left>" . tabbar-ruler-backward)
;;          ("M-s-<up>" . tabbar-ruler-up)
;;          ;; Alternatively use tabbar-ruler-move.
;;          )
;;   :config
;;   (progn
;;     (setq tabbar-ruler-global-tabbar t)
;;     (setq tabbar-use-images nil)        ; speed up

;;     (setq tabbar-ruler-swap-faces t)    ; revert colors for active/inactive tab
;;     (tabbar-install-faces)         ; if they where already installed, the previous line was set too late
;;     ))

;; Emacs mode for filesystem navigation.
(use-package nav)

;; Color identifiers based on their names.
;; Unique coloring of symbols instead of keywords.
(use-package color-identifiers-mode
  :diminish color-identifiers-mode
  :config
  (global-color-identifiers-mode))

;; Show number of matches in mode-line while searching.
(use-package anzu
  :diminish anzu-mode
  :config
  (progn
    (global-anzu-mode)))

;; Diminished modes are minor modes with no modeline display.
(use-package diminish
  :config
  (progn
    (diminish 'global-whitespace-mode " _")
    ))

;; A tree plugin like NerdTree for Vim.
(use-package neotree
  :bind (("<f8>" . neotree-toggle)))

;; Efficiently hopping squeezed lines powered by helm interface.
(use-package helm-swoop
  :bind (("M-i" . helm-swoop)))

;; My sgf library for handling Smart Go Format files
(use-package sgf)

;(set-face-attribute 'fringe nil :background "dark slate gray")
;(set-face-attribute 'linum nil :background "dark slate gray")

;; Show keybindings in a nice format for major mode.
;; Use as M-x discover-my-major
(use-package discover-my-major)

(use-package aggressive-indent
  :config
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

;; Load initializations for this site.
(use-package init-local)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; THE FOLLOWING INSTRUCTIONS SHOULD BE PERFORMED LAST,
;; SO MAKE ANY ADDITIONS BEFORE THIS LINE

;; In order open a file in an existing emacs from a shell, use
;; emacsclient -n [file]
;; FIXME: avoid starting server if already started
;; The following code extracted from http://stackoverflow.com/questions/3704962/how-to-know-if-emacs-server-is-running dosn't seem to work.

;; (unless (and (boundp 'server-process)
;;              (memq (process-status server-process) '(connect listen open run)))
;;   (server-start))

;; Allow editing text boxes from clients such as google chrome
;; (requires "Edit with emacs" extension).
(use-package edit-server
  :if window-system
  :init
  (progn
    (add-hook 'after-init-hook 'server-start t)
    (add-hook 'after-init-hook 'edit-server-start t)))

;; Load custom file if present.
(setq custom-file (concat user-emacs-directory "emacs-custom.el"))
(load custom-file 'noerror)

(provide 'init)
;;; init.el ends here
(put 'narrow-to-page 'disabled nil)
