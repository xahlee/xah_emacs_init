;;-*- coding: utf-8 -*-
;; emacs customization for setting up hyper and super keys
;; 〈Emacs: How to define Hyper ＆ Super Keys〉 http://ergoemacs.org/emacs/emacs_hyper_super_keys.html
;; 〈Emacs: Remapping Keys Using key-translation-map〉 http://ergoemacs.org/emacs/emacs_key-translation-map.html

;; Xah Lee
;; 2011-05
;; ∑ http://xahlee.org/



;;;; set Hyper and Super key
;; (cond
;;  ((string-equal system-type "windows-nt") ; Windows

;;   ;; setting the PC keyboard's various keys to
;;   ;; Super or Hyper, for emacs running on Windows.
;;   (setq
;;    w32-pass-lwindow-to-system nil
;;    w32-pass-rwindow-to-system nil
;;    w32-pass-apps-to-system nil
;;    w32-lwindow-modifier 'super ; Left Windows key
;;    w32-rwindow-modifier 'hyper ; Right Windows key
;;    w32-apps-modifier 'hyper)   ; Menu key
;;   )

;;  ((string-equal system-type "darwin") ; Mac
;;   ;; Carbon Emacs variables
;;   ;;    (setq mac-command-key-is-meta t)
;;   ;;    (setq mac-option-key-is-meta nil)

;;   ;; 2009-10-01, 2011-05-31 works for emacs 23.1 built for ns
;;   (setq mac-control-modifier 'control)
;;   (setq mac-command-modifier 'meta)
;;   (setq mac-option-modifier 'hyper)
;;   ;; (setq mac-option-modifier 'super)

;;   ;; Carbon emacs. disable the Mac shortcut convention of cmd-h to hide current application.
;;   (setq mac-pass-command-to-system nil)
;;   )

;;  ((string-equal system-type "gnu/linux")
;;   nil ; do nothing. You should set Super and Hyper from your OS
;;   )
;;  )
