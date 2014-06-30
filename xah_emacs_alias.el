;; -*- coding: utf-8 -*-
;; emacs customization for command aliases
;;
;; Xah Lee ; 2007-06

; shortening of often used commands

(defalias 'fd 'find-dired)
(defalias 'gf 'grep-find)
(defalias 'lcd 'list-colors-display)
(defalias 'rb 'revert-buffer)
(defalias 'rs 'replace-string)
(defalias 'sl 'sort-lines)
(defalias 'snf 'sort-numeric-fields)
(defalias 'g 'grep)
(defalias 'ds 'desktop-save)
(defalias 'dt 'desktop-save)
(defalias 'dtm 'desktop-mode)
(defalias 'dsm 'delete-selection-mode)

(defalias 'elm 'emacs-lisp-mode)
(defalias 'hm 'html-mode)
(defalias 'acm 'auto-complete-mode)
(defalias 'cm 'company-mode)
(defalias 'jsm 'js-mode)
(defalias 'js2 'js2-mode)

(defalias 'ssm 'shell-script-mode)

(defalias 'tpu-edt 'forward-char)
(defalias 'tpu-edt-on 'forward-char) ; fuck tpu-edt


;; xah personal

(defalias '8w 'xwe-new-word-entry)
(defalias '8d 'xwe-add-definition)
(defalias '8s 'xwe-add-source)
(defalias '8c 'xwe-add-comment)

(defalias 'ip 'insert-php-tag)

(defalias 'wc 'xah-count-words-region-or-line)
;; (defalias 'cp 'xah-copy-to-register-1)
;; (defalias 'pt 'insert-register-content-1)

