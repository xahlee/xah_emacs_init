;; -*- coding: utf-8 -*-
;; 2013-01-09
;; Xah Lee


 
(defun toggle-menu-key ()
  "toggle the value of `w32-apps-modifier' between 'meta and 'nil"
  (interactive)
  (if (eq w32-apps-modifier 'meta)
        (progn (setq w32-apps-modifier 'nil))
      (progn (setq w32-apps-modifier 'meta) )
      ))

(setq w32-apps-modifier 'meta) ; Menu key
(setq w32-apps-modifier 'nil) ; Menu key

;; find a key for smex
(global-set-key (kbd "<f7> x") 'execute-extended-command)

;;; enhanced execute-extended-command
;; make the menu key call smex's M-x. However, for some reason this doesn't work. Works only when this code is manually executed after emacs start.
(require 'smex)
(smex-initialize)
(when (member 'smex features)
  (global-set-key (kbd "M-a") 'smex)  )


;; need f11 f12 to switch tabs
;;  need a key to close window.

(global-set-key (kbd "<kp-6>") 'repeat-complex-command)

(global-set-key (kbd "<C-kp-4>") 'cycle-font-backward)
(global-set-key (kbd "<C-kp-5>") 'cycle-font-2)
(global-set-key (kbd "<C-kp-6>") 'cycle-font-forward)

