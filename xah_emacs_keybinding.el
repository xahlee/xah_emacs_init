;; -*- coding: utf-8 -*-

;; xah-fly-keys move to http://ergoemacs.org/misc/ergoemacs_vi_mode.html

(global-set-key (kbd "<C-tab>") 'xah-next-user-buffer)
(global-set-key (kbd "<C-S-iso-lefttab>") 'xah-previous-user-buffer)

(define-key key-translation-map (kbd "<f17>") (kbd "C-g"))
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

(global-set-key (kbd "<f14>") 'xah-close-current-buffer)

(global-set-key (kbd "C-w") 'xah-close-current-buffer)

(setq xfk-major-mode-lead-key (kbd "<delete>"))

(progn
  (define-key xah-help-keymap (kbd "2") 'xah-lookup-google)
  (define-key xah-help-keymap (kbd "1") 'xah-lookup-wikipedia)
  (define-key xah-help-keymap (kbd "9") 'xah-lookup-word-definition)
  (define-key xah-help-keymap (kbd "0") 'xah-lookup-all-dictionaries))

(global-set-key (kbd "<S-prior>") 'scroll-down) ; page up
(global-set-key (kbd "<S-next>") 'scroll-up) ; page down

;; keys for moving to prev/next code section (form feed; ^L)
(global-set-key (kbd "<C-M-prior>") 'backward-page) ; Ctrl+Alt+PageUp
(global-set-key (kbd "<C-M-next>") 'forward-page)   ; Ctrl+Alt+PageDown

;; 'xah-cycle-font-2
;; 'xah-cycle-font-next
;; 'xah-cycle-font-previous
