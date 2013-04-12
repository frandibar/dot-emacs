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
(let ((default-directory "/usr/share/emacs/site-lisp"))
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
;; form only run after the module has been loaded by Emacs.
;; You should keep `:init' forms as simple as possible, and put
;; as much as you can get away with on the `:config' side.

;; `use-package' also adds `describe-personal-keybindings'

(setq calendar-date-style 'european)         ; dd/mm/yyyy

(setq-default major-mode 'lisp-interaction-mode)

(use-package eshell
  :config
  (progn
    ;; the eshell directory holds alias definitions and history information
    (setq eshell-history-size 1280)
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
    ))

;; this allows (among other things) entering unicode chars in the minibuffer
(setq enable-recursive-minibuffers t)
(setq minibuffer-depth-indicate-mode t)  ;; add depth count to minibuffer (useful when recursive)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BEHAVIOR SETTINGS

;; Copy/paste behavior
;; use C-x C-v C-c for copy/pasting (only when a region is selected) and C-z for undo
;(cua-mode t)
;; use cua mode for rectangle selection
(cua-selection-mode t)
;;(setq cua-keep-region-after-copy t)

;; insert matching pairs of brackets
;; (electric-pair-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; APPEARANCE SETTINGS

;; set window title to buffer-file-name
(setq frame-title-format '("" "emacs - %b - " buffer-file-name))

;; (use-package color-theme-zenburn
;;   :config
;;   (color-theme-zenburn))

;; highlight cursor line
(global-hl-line-mode 1)

;; flash the frame upon C-g
(setq visible-bell t)

;; show matching parentheses
(show-paren-mode t)

;; (use-package highlight-parentheses
;;   :config
;;   (progn
;;     (add-hook 'lisp-mode-hook 'highlight-parentheses-mode)
;;     (add-hook 'lisp-interaction-mode-hook 'highlight-parentheses-mode)
;;     (add-hook 'scheme-mode-hook 'highlight-parentheses-mode)
;;     ;; highlight expression (instead of enclosing parens)
;;     ;; when over an opening paren or after closing one
;;     (setq hl-paren-colors (quote ("turquoise1" "green" "yellow" "orange")))
;;     (setq show-paren-style 'expression)
;;     ))

;; hide tool bar and menu bar and scroll bar
(tool-bar-mode -1)
(menu-bar-mode t)
(set-scroll-bar-mode nil)

;; show line and column number in the mode line
(line-number-mode 1)
(column-number-mode 1)

;; hide splash screen
(setq inhibit-splash-screen t)

;; show blank screen on startup
(setq initial-scratch-message nil)

;; show visual feedback upon C-g
(setq visible-bell t)

;; set font size
(set-face-attribute 'default nil :height 110)

;; highlight tabs and trailing spaces
(setq whitespace-style '(tabs trailing space-before-tab newline indentation empty space-after-tab tab-mark))
;;(global-whitespace-mode 1)

;; interactive name completion for describe-function, describe-variable, etc.
(icomplete-mode 1)

;; turn on winner mode, to allow restoring window configuration with C-c ← and C-c →
(winner-mode 1)

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

;; key to show line numbers
;(global-set-key (kbd "C-c C-g") 'global-linum-mode)

;; by default, emacs binds M-z to zap-to-char. I prefer binding it to
;; zap-up-to-char, but the latter is not loaded by default (it's in
;; misc.el instead of simple.el)
(autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

;; use these keybindings for window switching since default ones
;; (windmove-default-keybindings) S-{up,down,left,right}
;; interfere with org and calendar
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

;; keybindings for navigating lisp code
(global-set-key (kbd "M-<right>") 'forward-sexp)  ; defaults to right-word, same as C-<right>
(global-set-key (kbd "M-<left>") 'backward-sexp)  ; defaults to left-word, same as C-<left>
(global-set-key (kbd "M-<up>") 'backward-up-list)
(global-set-key (kbd "M-<down>") 'down-list)

;; Use a minor mode that makes my keybindings globally override and
;; take precedence over all other bindings for that key, that is,
;; override all major/minor mode maps and make sure my binding is
;; always in effect.  In order to avoid precedence over other minor
;; modes, it should go first on the list minor-mode-map-alist.
;; Extracted from
;; http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs/5340797
;; (defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")
;; (define-key my-keys-minor-mode-map (kbd "C-i") 'some-function)
;; (define-minor-mode my-keys-minor-mode
;;   "A minor mode so that my key settings override annoying major modes."
;;   t " my-keys" 'my-keys-minor-mode-map)

;; (my-keys-minor-mode 1)

;; ;; but turn off the overridings in the minibuffer
;; (add-hook 'minibuffer-setup-hook (lambda() (my-keys-minor-mode 0)))
;; ;; Make my keybindings retain precedence, even if subsequently-loaded
;; ;; libraries bring in new keymaps of their own. Because keymaps can be
;; ;; generated at compile time, load seemed like the best place to do
;; ;; this.
;; (defadvice load (after give-my-keybindings-priority)
;;   "Try to ensure that my keybindings always have priority."
;;   (if (not (eq (car (car minor-mode-map-alist)) 'my-keys-minor-mode))
;;       (let ((mykeys (assq 'my-keys-minor-mode minor-mode-map-alist)))
;;         (assq-delete-all 'my-keys-minor-mode minor-mode-map-alist)
;;         (add-to-list 'minor-mode-map-alist mykeys))))
;; (ad-activate 'load)

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

;; adjust line number width
(setq linum-format "%4d")

;; enable functions that are disabled by default
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

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

(use-package ibuffer
  :config
  (progn
    ;; show buffers into the following groups
    (setq ibuffer-saved-filter-groups
          '(("default"
             ("dired" (mode . dired-mode))
             ("clojure" (mode . clojure-mode))
             ("python" (mode . python-mode))
             ("elisp" (mode . emacs-lisp-mode))
             ("org" (mode . org-mode))
             ("sql" (mode . sql-mode))
             ("mu4e" (or
                      (mode . mu4e-compose-mode)
                      (mode . mu4e-headers-mode)
                      (mode . mu4e-view-mode)
                      (mode . mu4e-main-mode)))
             ("gnus" (or
                      (mode . message-mode)
                      (mode . bbdb-mode)
                      (mode . mail-mode)
                      (mode . gnus-group-mode)
                      (mode . gnus-summary-mode)
                      (mode . gnus-article-mode)
                      (name . "^\\.bbdb$")
                      (name . "^\\.newsrc-dribble")))
             ("man" (name . "^*Man "))
             ("emacs" (name . "^*"))
             ("xml" (mode . nxml-mode)))))

    ;; don't show empty groups
    (setq ibuffer-show-empty-filter-groups nil)

    ;; use ibuffer instead of list-buffers which I find useless
    (defalias 'list-buffers 'ibuffer)

    ;; use the defined groups when entering ibuffer-mode
    (add-hook 'ibuffer-mode-hook
              (lambda () (ibuffer-switch-to-saved-filter-groups "default")))

    ;; use human readable Size column instead of original one
    ;; extracted from http://www.emacswiki.org/emacs/IbufferMode
    (define-ibuffer-column size-h
      (:name "Size" :inline t)
      (cond
       ((> (buffer-size) 1000) (format "%7.3fk" (/ (buffer-size) 1000.0)))
       ((> (buffer-size) 1000000) (format "%7.3fM" (/ (buffer-size) 1000000.0)))
       (t (format "%8d" (buffer-size)))))
    )

  ;; modify the default ibuffer-formats
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 18 18 :left :elide) " "
                (size-h 9 -1 :right) " "
                (mode 16 16 :left :elide) " "
                filename-and-process))))

;; add additional package repositories.
;; the default elpa.gnu.org are all FSF signed.
(use-package package
  :config
  (progn
    (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
    (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/") t)
    (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
    (package-initialize)
    (when (not package-archive-contents)
      (package-refresh-contents))

    ;; here go the packages that should be installed
    (defvar prelude-packages
      '(
        ace-jump-mode                   ; quick cursor location
        auto-complete
        auto-highlight-symbol           ; automatic highlighting current symbol minor mode
        dired+                          ; extensions to dired
        elisp-slime-nav                 ; make M-. and M-, work in elisp like they do in slime
        eshell-manual                   ; an updated manual for Eshell
        expand-region                   ; increase selected region by semantic units
        ggtags                          ; GNU Global source code tagging system
        git-gutter                      ; show git changes in left margin
        graphviz-dot-mode               ; mode for the dot-language used by graphviz
        highlight-parentheses           ; highlight surrounding parentheses
        key-chord                       ; map pairs of simultaneously pressed keys to commands
        magit                           ; control git from emacs
        minimap                         ; view code from far away
        openwith                        ; open files with external programs
        paredit                         ; minor mode for editing parentheses
        projectile                      ; project management
        sauron                          ; notification of events (org, mail, etc)
        smex                            ; ido like behavior for M-x
        typing                          ; a game for fast typers
        undo-tree                       ; treat undo history as a tree
        wgrep                           ; writable grep buffer and apply the changes to files
        yasnippet                       ; a template system
        zenburn-theme                   ; low contrast color theme (not zenburn-emacs)

        ;; python setup
        flymake-python-pyflakes         ; a filemake handler for python-mode using pyflakes
        ipython                         ; add support for ipython in python-mode
        pyflakes                        ; run python pyflakes checker and output to grep buffer
        python-pep8                     ; minor mode for running `pep8'
        python-pylint                   ; minor mode for running `pylint'
        epc                             ; an RPC stack for the Emacs Lisp (needed for jedi)

        ;; clojure setup
        clojure-mode
        clojure-test-mode
        elein                           ; running leiningen commands from emacs
        nrepl                           ; Client for Clojure nREPL
        slime                           ; Superior Lisp Interaction Mode for Emacs
        slime-clj                       ;         slime extensions for swank-clj
        slime-repl                      ; Read-Eval-Print Loop written in Emacs Lisp

        ;; once used but fell in disuse
        ;; iy-go-to-char                   ; advance to char like `f' in vim (using my funcs instead)
        ;; highlight-symbol
        ;; workgroups
        ;; wrap-region
        ;; mark-multiple                   ; A library that sorta lets you mark several regions at once
        ;; mark-more-like-this             ; Mark additional regions in buffer matching current region.
        ;; dot-mode                        ; minor mode to repeat typing or commands

        "A list of packages to ensure are installed at launch.")

      (dolist (p prelude-packages)
        (unless (package-installed-p p)
          (package-install p))))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired mode
(use-package dired-x)  ; makes dired-jump available with C-x C-j from the start
(use-package dired-aux
  :init
  ;; to uncompress a .zip file, add "zip" to the variable
  ;; 'dired-compress-file-suffixes
  (add-to-list 'dired-compress-file-suffixes '("\\.zip\\'" ".zip" "unzip"))

  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir as target, instead of the current subdir of this dired buffer.
  (setq dired-dwim-target t)

  ;; allow dired to be able to delete or copy a whole dir.  “always”
  ;; means no asking. “top” means ask once. Any other symbol means ask
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

  ;; show all, long, no group, human readable file size
  (setq dired-listing-switches "-algh")

  ;; use M-o to toggle viewing files
  (setq dired-omit-files
        (rx (or (seq bol (? ".") "#")         ;; emacs autosave files
                (seq bol "." (not (any "."))) ;; dot-files
                (seq bol ".pyc" eol)          ;; python compiled
                (seq bol ".pyo" eol)          ;; python object
                (seq bol ".o" eol))))         ;; object files
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++ mode
;; set indentation style for c++-mode
(setq c-default-style "stroustrup"
      c-basic-offset 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG mode
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
            ("b" "banco" entry (file+headline "~/Dropbox/docs/agenda-personal.org" "banco")
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
    (setq appt-message-warning-time 15 ; warn 15 min in advance
          appt-display-interval 5      ; repeat every 5 min
          appt-display-mode-line t     ; show in the modeline
          appt-display-format 'window) ; use our func
    (appt-activate 1)              ; active appt (appointment notification)
    (display-time)                 ; time display is required for this...

    ;; update appt each time agenda is opened
    (add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)

    ;; commented out because I prefer using sauron buffer instead of a popup window.
    ;; (defun mine-appt-display (min-to-app new-time msg)
    ;;   (mine-popup (format "Appointment in %s minute(s)" min-to-app) msg))
    ;; (setq appt-disp-window-function (function mine-appt-display))
    ))

(use-package eldoc
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)))


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
         ;; FIXME: doesn't override calc-copy-as-kill in calc-mode
         ("M-k" . mine-close-buffer-and-window) ; override kill-sentence

         ("M-C" . mine-toggle-case)
         ("C-x $" . mine-toggle-folding-level)  ;; overrides set-selective-display

         ("S-<return>" . mine-smart-open-line)
         )
  :init
  (progn
    ;; view greek letter lambda
    (add-hook 'emacs-lisp-mode-hook 'mine-greek-lambda)
    (add-hook 'python-mode-hook 'mine-greek-lambda)
    (add-hook 'python-mode-hook (lambda () (linum-mode 1)))
    (add-hook 'c++-mode-hook (lambda () (linum-mode 1)))
    ))

(use-package starter-kit-defuns
  :bind (("C-c e" . esk-eval-and-replace)))

(use-package minimap
  :bind (("<f11>" . mine-minimap-toggle))
  )

;; ido-like behavior for M-x
(use-package smex
  :bind (("M-x" . smex)                 ; overrides default execute-extended-command
         ("M-X" . smex-major-mode-commands)
         ("C-c C-c M-x" . execute-extended-command))) ; this is the old M-x

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
;; (use-package mark-more-like-this
;;   :bind (("C-<" . mark-previous-like-this)
;;          ("C->" . mark-next-like-this)
;;          ("C-*" . mark-all-like-this)))

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

;; ;; python autocompletion
;; (use-package jedi
;;   :config
;;   (progn
;;     ;; (add-hook 'python-mode-hook 'jedi:setup)     ; FIXME: epc not working
;;     (add-hook 'python-mode-hook 'jedi:ac-setup)     ; alternatively, only if autocompletion needed
;;     (setq jedi:setup-keys t)
;;   ))

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

;; Slime allows navigation to the symbol at point (using M-.), and the
;; ability to pop back to previous marks (using M-,).  This plugin
;; provides similar navigation for Emacs Lisp, supporting navigation
;; to the definitions of variables, functions, libraries and faces.
(use-package elisp-slime-nav
  :config
  (progn
    (add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t)))
    ))

;; slime for common lisp
(use-package slime
  :disabled t                           ; not using it
  :commands slime-setup
  :config
  (progn
    (setq inferior-lisp-program "/usr/bin/clisp")
    (slime-setup '(slime-fancy))

    ;; enable paredit in slime repl
    (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
    ))

;; git within emacs
(use-package magit)

;; allows changing files directly from grep buffer
(use-package wgrep)

;; cd with bookmark navigation
;; use M-x cv RET or directly from shell
(use-package cdargs)

(use-package auto-complete-config
  :config
  (ac-config-default)
  (setq ac-auto-start 4))               ; only offer when 4 chars have been typed

(use-package openwith
  :init
  (progn
    (setq openwith-associations
          '(("\\.pdf\\'" "evince" (file))
            ("\\.mp3\\'" "xmms" (file))
            ("\\.sgf\\'" "quarry" (file))
            ("\\.\\(?:mpe?g\\|avi\\|wmv\\|flv\\|mp4\\|mov\\)\\'" "mplayer" ("-idx" file))
            ("\\.\\(?:jp?g\\|png\\)\\'" "display" (file)))
          )
    (openwith-mode t)))

;; enable project management for all modes
;; root dir must have a file named .projectile to be considered a project
;; except for git repos. This file has entries with patterns to ignore files
(use-package projectile
  :config
  (projectile-global-mode))

;; mu4e mail client
(use-package init-mail)

(use-package sauron
  :bind (("C-c s" . sauron-toggle-hide-show)
         ("C-c t" . sauron-clear))
  :init
  (progn
    (setq sauron-separate-frame nil)
    ;; allow the script to find the D-Bus session bus, even when running outside its session.
    (setq sauron-dbus-cookie t)
    ;; pop up window when an event occurs
    (add-hook 'sauron-event-added-functions
              (lambda (origin prio msg &optional props)
                (sr-show)))
    (sauron-start)
    (sr-hide)
    ))

(use-package auto-highlight-symbol
  :init
  (global-auto-highlight-symbol-mode t))

;; load initializations for this site
(use-package init-local)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HOOKS

;; delete trailing whitespace upon saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; view-mode
(add-hook 'help-mode-hook '(lambda () (view-mode t)))

;; avoid being asked when opening not so large files
;; i.e. video files are handled by external program
(setq large-file-warning-threshold nil)

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

;; FIXME: check why I get an error when using emacs-init-time
;; show load time in *Messages* buffer
;; FIXME: broke when upgrading to 24.3
;; (message "My init file loaded in %ds" (destructuring-bind (hi lo ms) (current-time)
;;                                         (- (+ hi lo) (+ (first *emacs-load-start-time*) (second *emacs-load-start-time*)))))
;; (message (concat "My init file loaded in " (emacs-init-time)))
