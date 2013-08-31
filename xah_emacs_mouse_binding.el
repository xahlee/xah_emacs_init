;; -*- coding: utf-8 -*-
;; mouse settings.
;; Xah Lee
;; created: 2011-11-13


;; mouse

;; emacs mouse numbering changes depending on {OS, mouse, driver}.
;; http://xahlee.info/kbd/X11_mouse_button_numbering.html

  (global-set-key (kbd "<mouse-3>") 'describe-char) ; right button

;; set the “forward button” (5th button) to close.
(cond
 ((string-equal system-type "windows-nt") ; Windows
  (global-set-key (kbd "<mouse-5>") 'ergoemacs-close-current-buffer)

  (global-set-key (kbd "<C-wheel-up>") 'text-scale-increase) ; control+shift wheel up
  (global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease) ; control+shift wheel down

  )

 ((string-equal system-type "gnu/linux")
  (global-set-key (kbd "<mouse-9>") 'ergoemacs-close-current-buffer) ; next page button

  ;; 'text-scale-increase

;; 'mwheel-scroll
  ;; forward-word,
  ;; 'ergoemacs-backward-open-bracket
  ;; 'ergoemacs-forward-block

  (global-set-key (kbd "<mouse-4>") 'mwheel-scroll) ; wheel up
  (global-set-key (kbd "<mouse-5>") 'mwheel-scroll) ; wheel down

  (global-set-key (kbd "<C-mouse-4>") 'ergoemacs-backward-block ) ;
  (global-set-key (kbd "<C-mouse-5>") 'ergoemacs-forward-block) ;

  (global-set-key (kbd "<S-mouse-4>") 'ergoemacs-backward-close-bracket) ;
  (global-set-key (kbd "<S-mouse-5>") 'ergoemacs-forward-open-bracket) ;

  (global-set-key (kbd "<M-mouse-4>") 'ergoemacs-backward-open-bracket ) ;
  (global-set-key (kbd "<M-mouse-5>") 'ergoemacs-forward-close-bracket ) ;

  ;; (global-set-key (kbd "<mouse-4>") (lambda () (interactive) (forward-line -2))) ; wheel up
  ;; (global-set-key (kbd "<mouse-5>") (lambda () (interactive) (forward-line 2))) ; wheel down

  ;; (global-set-key (kbd "<mouse-4>") 'mwheel-scroll) ; wheel up
  ;; (global-set-key (kbd "<mouse-5>") 'mwheel-scroll) ; wheel down

  )

 ((string-equal system-type "darwin") ; Mac
  (global-set-key (kbd "<mouse-5>") 'ergoemacs-close-current-buffer) ) )
