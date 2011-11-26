;; configure scheme
(setq scheme-program-name "racket")
(setq auto-mode-alist (cons '("\\.rkt$" . scheme-mode) auto-mode-alist))
;(setq auto-mode-alist (cons '("\.[rkt]\\>" . quack-pltfile-mode) auto-mode-alist))

;; use chrome as default browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")
