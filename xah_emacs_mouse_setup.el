;; -*- coding: utf-8; lexical-binding: t; -*-
;; mouse settings.
;; Xah Lee
;; created: 2011-11-13

;; Emacs: How to Set Mouse Buttons
;; http://ergoemacs.org/emacs/emacs_mouse_wheel_config.html

;; HHH___________________________________________________________________
;; mouse

;; (setq mouse-yank-at-point t)

;; 2020-08-15 make mouse wheel scroll just 1 line. this is great for logitech's spin wheel
(when (>= emacs-major-version 27)
  (customize-set-variable
   'mouse-wheel-scroll-amount
   '(1)))

;; 2020-08-15 this is great for logitech's spin wheel
(setq mouse-wheel-progressive-speed nil)

(defun xah-add-key-to-Info-mode ()
  "Add keybinding to `Info-mode'.
URL `http://ergoemacs.org/emacs/emacs_adding_browser_keys.html'
Version 2020-09-01"
  (when (string-equal system-type "darwin") ; macOS
    (local-set-key (kbd "s-[") 'Info-history-back)))

(add-hook 'Info-mode-hook 'xah-add-key-to-Info-mode)
