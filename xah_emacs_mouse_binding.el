;; -*- coding: utf-8 -*-
;; mouse settings.
;; Xah Lee
;; created: 2011-11-13

;; Emacs Mouse Wheel Config
;; http://ergoemacs.org/emacs/emacs_mouse_wheel_config.html

;; emacs mouse numbering changes depending on {OS, mouse, driver}.
;; http://xahlee.info/kbd/X11_mouse_button_numbering.html


;; mouse

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

  (global-set-key (kbd "<mouse-4>") 'mwheel-scroll) ; wheel up
  (global-set-key (kbd "<mouse-5>") 'mwheel-scroll) ; wheel down

  (global-set-key (kbd "<C-mouse-4>") 'ergoemacs-backward-block) ; wheel up
  (global-set-key (kbd "<C-mouse-5>") 'ergoemacs-forward-block) ; wheel down

  (global-set-key (kbd "<S-mouse-4>") 'ergoemacs-previous-emacs-buffer) ;
  (global-set-key (kbd "<S-mouse-5>") 'ergoemacs-next-emacs-buffer) ;

  (global-set-key (kbd "<M-mouse-4>") 'ergoemacs-backward-open-bracket ) ;
  (global-set-key (kbd "<M-mouse-5>") 'ergoemacs-forward-close-bracket ) ;

  (global-set-key (kbd "<C-S-mouse-4>") 'text-scale-increase) ;
  (global-set-key (kbd "<C-S-mouse-5>") 'text-scale-decrease) ;

  )

 ((string-equal system-type "darwin") ; Mac
  (global-set-key (kbd "<mouse-5>") 'ergoemacs-close-current-buffer) ) )

(defun scroll-up-10-lines ()
  "Scroll up 10 lines"
  (interactive)
  (scroll-up 10))

(defun scroll-down-10-lines ()
  "Scroll down 10 lines"
  (interactive)
  (scroll-down 10))

(defun cursor-down-some-lines ()
  "Move cursor down 10 logical lines"
  (interactive)
  (forward-line 10)
)

(defun cursor-up-some-lines ()
  "Move cursor up 10 logical lines"
  (interactive)
  (forward-line -10))
