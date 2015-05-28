;; -*- coding: utf-8 -*-
;; settings for packages that's not bundled with emacs
;;   Xah Lee
;; ∑ http://xahlee.org/

(when (fboundp 'global-undo-tree-mode)
  (global-undo-tree-mode 1)
  (defalias 'redo 'undo-tree-redo))

(when (fboundp 'xah-lookup-google)
  (when (or
         (string-equal system-type "gnu/linux")
         (string-equal system-type "darwin"))
    (require 'eww)
    (setq xah-lookup-dictionary-browser-function 'eww)))

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
  (setq
   page-break-lines-modes
   '(
     emacs-lisp-mode lisp-mode scheme-mode compilation-mode outline-mode help-mode
     fundamental-mode text-mode org-mode ruby-mode python-mode html-mode nxml-mode
     xah-html-mode xah-elisp-mode
     )))

(when (fboundp 'htmlize-region)
  ;; htmlize.el
  ;; make htmlize generate unicode directly instead of html entities
  (setq htmlize-convert-nonascii-to-entities nil)
  ; make the output html use utf-8 charset
  (setq htmlize-html-charset "utf-8"))
