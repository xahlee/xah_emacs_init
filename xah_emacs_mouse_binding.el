;; -*- coding: utf-8 -*-
;; mouse settings.
;; Xah Lee
;; created: 2011-11-13


;; mouse

;; emacs mouse numbering changes depending on {OS, mouse, driver}.
;; http://xahlee.info/kbd/X11_mouse_button_numbering.html

;; set the “forward button” (5th button) to close.
(cond
 ((string-equal system-type "windows-nt") ; Windows
  (global-set-key (kbd "<mouse-5>") 'ergoemacs-close-current-buffer)
  (global-set-key (kbd "<mouse-4>") 'describe-char)
  )
 ((string-equal system-type "gnu/linux")
  (global-set-key (kbd "<mouse-9>") 'ergoemacs-close-current-buffer)

  (global-set-key (kbd "<C-mouse-4>") 'ergoemacs-previous-user-buffer) ; control wheel up
  (global-set-key (kbd "<C-mouse-5>") 'ergoemacs-next-user-buffer) ; control wheel down

  (global-set-key (kbd "<S-mouse-4>") 'ergoemacs-previous-emacs-buffer) ; shift + wheel up
  (global-set-key (kbd "<S-mouse-5>") 'ergoemacs-next-emacs-buffer) ; shift + wheel down

  (global-set-key (kbd "<C-down-mouse-2>") 'ergoemacs-close-current-buffer) ; control middle click

  (global-set-key (kbd "<C-S-mouse-4>") 'text-scale-increase) ; control+shift wheel up
  (global-set-key (kbd "<C-S-mouse-5>") 'text-scale-decrease) ; control+shift wheel down
  (global-set-key (kbd "<mouse-3>") 'describe-char) ; right button

  )
 ((string-equal system-type "darwin") ; Mac
  (global-set-key (kbd "<mouse-5>") 'ergoemacs-close-current-buffer) ) )
