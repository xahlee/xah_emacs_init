;; -*- coding: utf-8 -*-
;; mouse settings.
;; Xah Lee
;; created: 2011-11-13

;; Emacs: How to Set Mouse Buttons ＆ Wheel
;; http://ergoemacs.org/emacs/emacs_mouse_wheel_config.html


;; mouse

;; set the “forward button” (5th button) to close.
(cond
 ((string-equal system-type "windows-nt") ; Windows
  (global-set-key (kbd "<mouse-5>") 'xah-close-current-buffer)
  ;; (global-set-key (kbd "<C-wheel-up>") 'text-scale-increase) ; control wheel up
  ;; (global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease) ; control wheel down
  )

 ((string-equal system-type "gnu/linux")

  (global-set-key (kbd "<mouse-3>") 'xah-click-describe-char) ; right button

  (global-set-key (kbd "<mouse-4>") 'mwheel-scroll) ; wheel up
  (global-set-key (kbd "<mouse-5>") 'mwheel-scroll) ; wheel down

  (global-set-key (kbd "<S-mouse-4>") 'xah-previous-user-buffer)
  (global-set-key (kbd "<S-mouse-5>") 'xah-next-user-buffer)

  (global-set-key (kbd "<C-mouse-4>") 'xah-beginning-of-line-or-block)
  (global-set-key (kbd "<C-mouse-5>") 'xah-end-of-line-or-block)

  ;; (global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
  ;; (global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)

  )

 ((string-equal system-type "darwin") ; Mac
  (global-set-key (kbd "<mouse-5>") 'xah-close-current-buffer) ) )
