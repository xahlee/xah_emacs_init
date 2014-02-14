;; -*- coding: utf-8 -*-
;; mouse settings.
;; Xah Lee
;; created: 2011-11-13

;; Emacs Mouse Wheel Config
;; http://ergoemacs.org/emacs/emacs_mouse_wheel_config.html

;; emacs mouse numbering changes depending on {OS, mouse, driver}.
;; http://xahlee.info/kbd/X11_mouse_button_numbering.html


;; mouse

;; set the “forward button” (5th button) to close.
(cond
 ((string-equal system-type "windows-nt") ; Windows
  (global-set-key (kbd "<mouse-5>") 'xah-close-current-buffer)

  (global-set-key (kbd "<C-wheel-up>") 'text-scale-increase) ; control+shift wheel up
  (global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease) ; control+shift wheel down

  )

 ((string-equal system-type "gnu/linux")

(global-set-key (kbd "<mouse-3>") 'describe-char) ; right button

  (global-set-key (kbd "<mouse-9>") 'xah-close-current-buffer) ; next page button

  (global-set-key (kbd "<mouse-4>") 'mwheel-scroll) ; wheel up
  (global-set-key (kbd "<mouse-5>") 'mwheel-scroll) ; wheel down

  ;; ;; no accelerated scroll, but works well with logitech spin wheel http://xahlee.info/kbd/mouse_with_spinning_flywheel.html
  ;; (global-set-key (kbd "<mouse-4>") 'xah-scroll-down-10-lines) ; wheel up
  ;; (global-set-key (kbd "<mouse-5>") 'xah-scroll-up-10-lines) ; wheel down

  (global-set-key (kbd "<C-mouse-4>") 'xah-backward-block) ;
  (global-set-key (kbd "<C-mouse-5>") 'xah-forward-block) ;

  (global-set-key (kbd "<S-mouse-4>") 'xah-cursor-up-10-lines) ;
  (global-set-key (kbd "<S-mouse-5>") 'xah-cursor-down-10-lines) ;

  (global-set-key (kbd "<M-mouse-4>") 'xah-backward-close-bracket) ;
  (global-set-key (kbd "<M-mouse-5>") 'xah-forward-close-bracket) ;

  (global-set-key (kbd "<s-mouse-4>") 'text-scale-increase ) ;
  (global-set-key (kbd "<s-mouse-5>") 'text-scale-decrease ) ;

  (global-set-key (kbd "<C-S-mouse-4>") nil) ;
  (global-set-key (kbd "<C-S-mouse-5>") nil) ;

  )

 ((string-equal system-type "darwin") ; Mac
  (global-set-key (kbd "<mouse-5>") 'xah-close-current-buffer) ) )

