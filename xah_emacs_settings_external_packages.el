;; -*- coding: utf-8 -*-
;; settings for packages that's not bundled with emacs
;;   Xah Lee
;; ∑ http://xahlee.org/

(when (fboundp 'math-symbol-input-mode)
    (math-symbol-input-mode 1)
)

;; (eval-when-compile
;;   (when (boundp 'tabbar-mode)
;;     (tabbar-mode 0)
;;     ))

;; (setq yas/indent-line nil)
;; (yas-global-mode 0)

(when (fboundp 'global-auto-complete-mode)
    (global-auto-complete-mode 0)
)

;; (when (boundp 'ido-vertical-mode)
;;     (ido-vertical-mode 1)
;; )

;; (defun turn-spell-checking-on ()
;;   "Turn speck-mode or flyspell-mode on."
;;   (speck-mode 1)
;; ;  (flyspell-mode 1)
;;   )

;; (add-hook 'text-mode-hook 'turn-spell-checking-on)
;; (remove-hook 'text-mode-hook 'turn-spell-checking-on)
