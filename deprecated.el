;; Copy/paste behavior
;; use C-x C-v C-c for copy/pasting (only when a region is selected) and C-z for undo
;;(cua-mode t)

;;(setq cua-keep-region-after-copy t)

;; insert matching pairs of brackets
;; (electric-pair-mode)

;; don't let the cursor go into minibuffer prompt (default behavior allows copying prompt)
;; (setq minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))


;; (use-package eshell
;;   :config
;;   (progn
;;     ;; the eshell directory holds alias definitions and history information
;;     (setq eshell-history-size 1280)
;;     (setq eshell-directory-name "~/.emacs.d/eshell")
;;     ;; expand '.' and '..' (removing them from default value)
;;     (setq eshell-cmpl-dir-ignore "\\`\\(CVS\\)/\\'")
;;     (setq eshell-cmpl-ignore-case t)
;;     ;; by default eshell does completion the Emacs way: cycle through all
;;     ;; the possible values. Bash instead complete as much as possible, and
;;     ;; then wait for the next charater. This makes eshell behave like Bash
;;     (setq eshell-cmpl-cycle-completions nil)
;;     ;; behave more like a terminal
;;     (setq eshell-scroll-to-bottom-on-output t)
;;     ;; use ansi-term for these commands, since eshell is not good at
;;     ;; ansi-colors and control
;;     (add-hook 'eshell-first-time-mode-hook
;;               (lambda () (setq eshell-visual-commands
;;                           (append '("mutt" "vim" "screen" "lftp" "ipython" "telnet" "ssh" "htop")
;;                                   eshell-visual-commands))
;;                 (local-set-key (kbd "C-u") 'mine-eshell-kill-line)))
;;     ))


;; this allows (among other things) entering unicode chars in the minibuffer
;; (setq enable-recursive-minibuffers t)
;; (setq minibuffer-depth-indicate-mode t)  ;; add depth count to minibuffer (useful when recursive)


;; (global-set-key (kbd "<Scroll_Lock>") 'scroll-lock-mode)
;; (global-set-key (kbd "<esc>") 'keyboard-quit)
;; (global-set-key (kbd "C-3") 'follow-delete-other-windows-and-split)

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

;; (use-package ibuffer
;;   :config
;;   (progn
;;     ;; show buffers into the following groups
;;     (setq ibuffer-saved-filter-groups
;;           '(("default"
;;              ("clojure" (mode . clojure-mode))
;;              ("dired" (mode . dired-mode))
;;              ("grep" (mode . grep-mode)) ; must go before emacs rule
;;              ("emacs" (name . "^*"))
;;              ("elisp" (mode . emacs-lisp-mode))
;;              ("javascript" (mode . js-mode))
;;              ("c/c++" (or
;;                       (mode . c++-mode)
;;                       (mode . c-mode)))
;;              ("gnus" (or
;;                       (mode . message-mode)
;;                       (mode . bbdb-mode)
;;                       (mode . mail-mode)
;;                       (mode . gnus-group-mode)
;;                       (mode . gnus-summary-mode)
;;                       (mode . gnus-article-mode)
;;                       (name . "^\\.bbdb$")
;;                       (name . "^\\.newsrc-dribble")))
;;              ("man" (name . "^*Man "))
;;              ("mu4e" (or
;;                       (mode . mu4e-compose-mode)
;;                       (mode . mu4e-headers-mode)
;;                       (mode . mu4e-view-mode)
;;                       (mode . mu4e-main-mode)))
;;              ("org" (mode . org-mode))
;;              ("python" (mode . python-mode))
;;              ("sql" (mode . sql-mode))
;;              ("xml" (mode . nxml-mode)))))

;;     ;; don't show empty groups
;;     (setq ibuffer-show-empty-filter-groups nil)

;;     ;; use ibuffer instead of list-buffers which I find useless
;;     (defalias 'list-buffers 'ibuffer)

;;     ;; use the defined groups when entering ibuffer-mode
;;     (add-hook 'ibuffer-mode-hook
;;               (lambda () (ibuffer-switch-to-saved-filter-groups "default")))

;;     ;; use human readable Size column instead of original one
;;     ;; extracted from http://www.emacswiki.org/emacs/IbufferMode
;;     (define-ibuffer-column size-h
;;       (:name "Size" :inline t)
;;       (cond
;;        ((> (buffer-size) 1000) (format "%7.3fk" (/ (buffer-size) 1000.0)))
;;        ((> (buffer-size) 1000000) (format "%7.3fM" (/ (buffer-size) 1000000.0)))
;;        (t (format "%8d" (buffer-size)))))

;;     ;; modify the default ibuffer-formats
;;     (setq ibuffer-formats
;;           '((mark modified read-only " "
;;                   (name 50 50 :left :elide) " "
;;                   (size-h 9 -1 :right) " "
;;                   (mode 16 16 :left :elide) " "
;;                   filename-and-process)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++ mode
;; (use-package cc-mode
;;   :config
;;   (progn
;;     (dolist (mode-map '(c-mode-map c++-mode-map))
;;       (define-key (symbol-value mode-map) (kbd "M-s") 'mine-switch-cpp-h-file))
;;     ;; set indentation style for c++-mode
;;     (setq c-default-style "stroustrup"
;;           c-basic-offset 4)))


    ;;     ;; commented out because I prefer using sauron buffer instead of a popup window.
    ;;     ;; (defun mine-appt-display (min-to-app new-time msg)
    ;;     ;;   (mine-popup (format "Appointment in %s minute(s)" min-to-app) msg))
    ;;     ;; (setq appt-disp-window-function (function mine-appt-display))



;; ;; (use-package smartparens
;; ;;   :config
;; ;;   (progn
;; ;;     (smartparens-global-mode nil)))     ; interferes with paredit

;; (use-package yasnippet
;;   :disabled t                ; takes too long to load and I don't use it
;;   :commands yasnippet
;;   :config
;;   (yas/global-mode 1))



;; latex settings
;; When adding a new environment with C-c C-s, the list will not only provide standard LaTeX environments,
;; but also take your `\documentclass' and `\usepackage' commands into account.
;; (setq Tex-parse-self t)



;; load sunrise commander, a mix between dired and midnight commander.
;; (use-package sunrise-commander
;;   :commands sunrise
;;   :config
;;   (progn
;;     (use-package sunrise-x-tree)
;;     (use-package sunrise-x-buttons)))

;; show bookmarks on startup
;; (use-package bookmark
;;   :config
;;   (progn
;;     ;(bookmark-bmenu-list)
;;     ;(switch-to-buffer "*Bookmark List*")
;;     ))


;; ;; toggles between the shell buffer and current buffer
;; (use-package shell-toggle-patched
;;   :bind (("<f5>" . shell-toggle))
;;   :config
;;   (progn
;;     (autoload 'shell-toggle "shell-toggle"
;;       "Toggles between the shell buffer and whatever buffer you are editing." t)
;;     (autoload 'shell-toggle-cd "shell-toggle"
;;       "Pops up a shell-buffer and insert a \"cd <file-dir>\" command." t)
;;     (setq shell-toggle-launch-shell 'shell-toggle-eshell)))


;; ;; slime for common lisp
;; (use-package slime
;;   :disabled t                           ; not using it
;;   :commands slime-setup
;;   :config
;;   (progn
;;     (setq inferior-lisp-program "/usr/bin/clisp")
;;     (slime-setup '(slime-fancy))

;;     ;; enable paredit in slime repl
;;     (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
;;     ))


;; ;; cd with bookmark navigation
;; ;; use M-x cv RET or directly from shell
;; ;(use-package cdargs)  ; not using it

;; (use-package auto-complete-config
;;   :config
;;   (progn
;;     (ac-config-default)
;;     (setq ac-auto-start 4)))              ; only offer when 4 chars have been typed


;; (use-package sauron
;;   :bind (("C-c s" . sauron-toggle-hide-show)
;;          ("C-c t" . sauron-clear))
;;   :init
;;   (progn
;;     (setq sauron-separate-frame nil)
;;     ;; allow the script to find the D-Bus session bus, even when running outside its session.
;;     (setq sauron-dbus-cookie t)
;;     (setq sauron-max-line-length nil)   ; don't truncate lines
;;     (setq sauron-column-alist '((timestamp . 20)
;;                                 (message)))
;;     ;; pop up window when an event occurs
;;     (add-hook 'sauron-event-added-functions
;;               (lambda (origin prio msg &optional props)
;;                 (sr-show)))
;;     (sauron-start)
;;     (sr-hide)

;;     (defun sauron-clear ()
;;       "Override original sauron-clear. This version avoids asking, and hides buffer automatically.
;; TODO: use defadvice instead."
;;       (interactive)
;;       (when
;;           (and sr-buffer (buffer-live-p sr-buffer))
;;         (with-current-buffer sr-buffer
;;           (let ((inhibit-read-only t))
;;             (erase-buffer)))
;;         (message nil)
;;         (sr-hide)))
;;     ))


;; ;; mu4e mail client
;; (use-package init-mail)


;; commented out since keybindings interfere with shift-text
;; (use-package auto-highlight-symbol
;;   :init
;;   (progn
;;     (global-auto-highlight-symbol-mode t)))



;; (use-package key-chord
;;   :config
;;   (progn
;;     (key-chord-mode 1)
;;     ;; preferably, use upper case to avoid delay when typing
;;     (key-chord-define-global "FF" 'jump-char-forward)
;;     (key-chord-define-global "DD" 'jump-char-backward)

;;     (key-chord-define-global "HH" 'mine-point-to-top)
;;     (key-chord-define-global "MM" 'mine-point-to-middle)
;;     (key-chord-define-global "LL" 'mine-point-to-bottom)

;;     (key-chord-define-global "PP" 'mine-copy-current-line)

;;     (key-chord-define-global "RR" 'point-to-register)
;;     (key-chord-define-global "JJ" 'jump-to-register)
;;     ))


;; visual navigation through mark rings
;; bindings C-x C-<left>/<right> for global mark ring
;; bindings C-x <left>/<right> for buffer local mark ring
;; TODO: disabled because keybindings clash with windmove
;; (use-package back-button
;;   :config
;;   (back-button-mode 1))



;; (use-package nurumacs
;;   :config
;;   ;(setq nurumacs-map nil)  ; disable minimap
;;   )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HOOKS

;; view-mode
;; (add-hook 'help-mode-hook '(lambda () (view-mode t)))

;; avoid being asked when opening large files
;; i.e. video files are handled by external program
;; (setq large-file-warning-threshold nil)
