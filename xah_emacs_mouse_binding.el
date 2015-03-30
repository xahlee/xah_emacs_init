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

  (global-set-key (kbd "<C-mouse-4>") 'text-scale-increase)
  (global-set-key (kbd "<C-mouse-5>") 'text-scale-decrease)

  )

 ((string-equal system-type "darwin") ; Mac
  (global-set-key (kbd "<mouse-5>") 'xah-close-current-buffer) ) )

(defun xah-set-mouse-wheel-mode ()
  "DOCSTRING"
  (interactive)
  (let (myMode
        (myList
         '(
           "3 normal"
           "4 code block"
           "7 by 30 lines"
           )))
    (setq myMode (ido-completing-read "set wheel mode to:" myList))
    (cond
     ((string-equal myMode "3 normal") 
      (progn
        (global-set-key (kbd "<mouse-4>") 'mwheel-scroll)
        (global-set-key (kbd "<mouse-5>") 'mwheel-scroll)))
     ((string-equal myMode "4 code block") 
      (progn
        (global-set-key (kbd "<mouse-4>") 'xah-beginning-of-line-or-block)
        (global-set-key (kbd "<mouse-5>") 'xah-end-of-line-or-block)))

     ((string-equal myMode "7 by 30 lines") 
      (progn
        (global-set-key (kbd "<mouse-4>") (lambda () (interactive) (scroll-down 30)))
        (global-set-key (kbd "<mouse-5>") (lambda () (interactive) (scroll-up 30)))))

     (t (error "%s" "u fked")))))


