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

(when (fboundp 'xah-math-input-mode)
  (xah-math-input-mode 1))

(when (fboundp 'keyfreq-mode)
  ;; record command call statistics
  (require 'keyfreq)
  (setq keyfreq-file "~/.emacs.d/.emacs.keyfreq")
  (setq keyfreq-file-lock "~/.emacs.d/.emacs.keyfreq.lock")
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(when (fboundp 'global-page-break-lines-mode)
  ;; make the formfeed char (^L) display as a line
  (global-page-break-lines-mode 1)

  (setq page-break-lines-modes '(emacs-lisp-mode lisp-mode scheme-mode compilation-mode outline-mode help-mode xah-elisp-mode
)))

;; (eval-when-compile
;;   (when (boundp 'tabbar-mode)
;;     (tabbar-mode 0)
;;     ))

;; (setq yas/indent-line nil)
;; (yas-global-mode 0)

;; (defun turn-spell-checking-on ()
;;   "Turn speck-mode or flyspell-mode on."
;;   (speck-mode 1)
;; ;  (flyspell-mode 1)
;;   )

;; (add-hook 'text-mode-hook 'turn-spell-checking-on)
;; (remove-hook 'text-mode-hook 'turn-spell-checking-on)
