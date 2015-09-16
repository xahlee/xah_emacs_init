;; -*- coding: utf-8 -*-
;; emacs customization for command aliases
;; Xah Lee ; 2007-06

; shortening of often used commands

(defalias 'yes-or-no-p 'y-or-n-p)

(defalias 'lcd 'list-colors-display)
(defalias 'sl 'sort-lines)
(defalias 'ds 'desktop-save)
(defalias 'dt 'desktop-save)
(defalias 'dsm 'desktop-save-mode)

(defalias 'elm 'emacs-lisp-mode)
(defalias 'hm 'html-mode)

(defalias 'ssm 'shell-script-mode)

(defalias 'tpu-edt 'forward-char)
(defalias 'tpu-edt-on 'forward-char) ; fuck tpu-edt


;; xah personal

(defalias '1w 'xah-words-new-word-entry)
(defalias '1d 'xah-words-add-definition)
(defalias '1s 'xah-words-add-source)
(defalias '1c 'xah-words-add-comment)
