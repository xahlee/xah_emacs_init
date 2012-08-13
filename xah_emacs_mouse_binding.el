;; -*- coding: utf-8 -*-
;; mouse keybinding.
;; • http://ergoemacs.org/emacs/xah_emacs_mouse_binding.el
;; • http://ergoemacs.org/emacs/xah_emacs_keybinding.el
;; Xah Lee
;; created: 2011-11-13


;; § ----------------------------------------
;; mouse

;; emacs mouse numbering changes depending on {OS, mouse, driver}.
;; http://xahlee.org/kbd/X11_mouse_button_numbering.html

;; set the “forward button” (5th button) to close.
(cond
 ((string-equal system-type "windows-nt") ; Windows
  (global-set-key (kbd "<mouse-5>") 'close-current-buffer)
  (global-set-key (kbd "<mouse-4>") 'describe-char)
  )
 ((string-equal system-type "gnu/linux")
  (global-set-key (kbd "<mouse-9>") 'close-current-buffer)
  )
 ((string-equal system-type "darwin") ; Mac
  (global-set-key (kbd "<mouse-5>") 'close-current-buffer) ) )

(defun xah-ibuffer-keys ()
  "Modify keymaps used by `ibuffer'."
  (local-set-key (kbd "<mouse-1>") 'ibuffer-visit-buffer-other-window)
  )

(add-hook 'ibuffer-hook 'xah-ibuffer-keys)
(remove-hook 'ibuffer-hook 'xah-ibuffer-keys)

;; (defun flyspell-mode-changes ()
;;   "Some changes to be added to `flyspell-mode-hook'."
;;   (define-key flyspell-mouse-map (kbd "<mouse-4>") 'flyspell-correct-word)
;;   )

;; (add-hook 'flyspell-mode-hook 'flyspell-mode-changes)

