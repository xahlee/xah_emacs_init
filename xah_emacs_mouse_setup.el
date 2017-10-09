;; -*- coding: utf-8; lexical-binding: t; -*-
;; mouse settings.
;; Xah Lee
;; created: 2011-11-13

;; Emacs: How to Set Mouse Buttons ＆ Wheel
;; http://ergoemacs.org/emacs/emacs_mouse_wheel_config.html


;; mouse

;; Emacs: How to Set Mouse Buttons ＆ Wheel
;; http://ergoemacs.org/emacs/emacs_mouse_wheel_config.html


;; mouse

;; set the “forward button” (5th button) to close.
(cond
 ((string-equal system-type "gnu/linux")
  (progn

    (global-set-key (kbd "<S-mouse-4>") 'xah-previous-user-buffer)
    (global-set-key (kbd "<S-mouse-5>") 'xah-next-user-buffer)

    (global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
    (global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)

    (global-set-key (kbd "<mouse-6>") 'xah-beginning-of-line-or-block)
    (global-set-key (kbd "<mouse-7>") 'xah-end-of-line-or-block)))

 ((string-equal system-type "windows-nt") ; Windows
  (progn
    nil))
 ((string-equal system-type "darwin") ; Mac
  (progn nil)))
