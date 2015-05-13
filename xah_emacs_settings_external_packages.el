;; -*- coding: utf-8 -*-
;; settings for packages that's not bundled with emacs
;;   Xah Lee
;; ∑ http://xahlee.org/

;; (when (fboundp 'undo-tree-redo)
;;   ;; enhanced execute-extended-command
;;   (defalias 'redo 'undo-tree-redo)
;;   (global-undo-tree-mode 1))

(when (fboundp 'smex)
  ;; enhanced execute-extended-command
  (require 'smex)
  (smex-initialize))

;; ;;; make the formfeed char (^L) display as a line
;; (require 'page-break-lines)
;; (global-page-break-lines-mode 1)

(when (fboundp 'xah-math-input-mode)
  (xah-math-input-mode 1))

(when (fboundp 'global-auto-complete-mode)
  (global-auto-complete-mode 0))

(when (fboundp 'global-company-mode)
  (global-company-mode nil)
  (setq company-idle-delay 2))

(when (fboundp 'keyfreq-mode)
  ;; record command call statistics
  (require 'keyfreq)
  (setq keyfreq-file "~/.emacs.d/.emacs.keyfreq")
  (setq keyfreq-file-lock "~/.emacs.d/.emacs.keyfreq.lock")
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; (when (fboundp 'projectile-global-mode)
;;     (projectile-global-mode)
;; )

;; (eval-when-compile
;;   (when (boundp 'tabbar-mode)
;;     (tabbar-mode 0)
;;     ))

;; (setq yas/indent-line nil)
;; (yas-global-mode 0)

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
