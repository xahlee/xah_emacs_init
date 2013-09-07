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
(defalias 'wm 'whitespace-mode)
(defalias 'dsm 'delete-selection-mode)

(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)

(defalias 'elm 'emacs-lisp-mode)
(defalias 'hm 'html-mode)
(defalias 'dnml 'delete-non-matching-lines)

(defalias 'ssm 'shell-script-mode)

(defalias 'c 'toggle-case-fold-search)
(defalias 'hp 'highlight-phrase)
(defalias 'hl 'highlight-lines-matching-regexp)


;; xah personal

(defalias '8w 'xwe-new-word-entry)
(defalias '8d 'xwe-add-definition)
(defalias '8s 'xwe-add-source)
(defalias '8c 'xwe-add-comment)

(defalias 'ip 'insert-php-tag)

(defalias 'ft 'fix-datetimestamp)
(defalias 'wc 'count-words-region-or-line)
;; (defalias 'cp 'copy-to-register-1)
;; (defalias 'pt 'insert-register-content-1)
