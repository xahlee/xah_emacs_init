;; -*- coding: utf-8 -*-
;; emacs customization for command aliases
;; 
;; Xah Lee ; 2007-06
;;  http://ergoemacs.org/emacs/xah_emacs_abbr.el

; shortening of often used commands
(defalias 'o 'occur)
(defalias 'm 'man)
(defalias 'd 'delete-matching-lines)
(defalias 'fd 'find-dired)
(defalias 'gf 'grep-find)
(defalias 'lcd 'list-colors-display)
(defalias 'rb 'revert-buffer)
(defalias 'rs 'replace-string)
(defalias 'sl 'sort-lines)

(defalias 'ssm 'shell-script-mode)

(defalias 'tc 'toggle-case-fold-search)
(defalias 'hp 'highlight-phrase)
(defalias 'hl 'highlight-lines-matching-regexp)


;; § ----------------------------------------
;; xah personal

(defalias '8w 'xwe-new-word-entry)
(defalias '8d 'xwe-add-definition)
(defalias '8s 'xwe-add-source)
(defalias '8c 'xwe-add-comment)

(defalias 'c 'xah-cite)
(defalias 'ip 'insert-php-tag)

(defalias 'ft 'fix-timestamp)
(defalias 'wc 'count-words-region-or-line)
;; (defalias 'cp 'copy-to-register-1)
;; (defalias 'pt 'insert-register-content-1)
