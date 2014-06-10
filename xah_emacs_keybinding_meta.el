;; -*- coding: utf-8 -*-
;; 2013-11-09
;; meta key bindings
;; http://ergoemacs.org/emacs/ergonomic_emacs_keybinding.html

(global-set-key (kbd "M-'") 'xah-compact-uncompact-block)
(global-set-key (kbd "M-,") 'xah-shrink-whitespaces)
(global-set-key (kbd "M-.") 'backward-kill-word)

(global-set-key (kbd "M-6") 'xah-select-current-block)
(global-set-key (kbd "M-7") 'xah-select-current-line)
(global-set-key (kbd "M-8") 'xah-extend-selection)
(global-set-key (kbd "M-9") 'xah-select-text-in-quote)

(global-set-key (kbd "M-SPC") 'set-mark-command)
(global-set-key (kbd "M-\"") 'xah-compact-uncompact-Block)
(global-set-key (kbd "M-`") 'other-frame)
(global-set-key (kbd "M-b") 'xah-toggle-letter-case)
(global-set-key (kbd "M-c") 'previous-line)
(global-set-key (kbd "M-d") 'xah-beginning-of-line-or-block)
(global-set-key (kbd "M-e") 'delete-backward-char)
(global-set-key (kbd "M-f") 'isearch-forward)
(global-set-key (kbd "M-g") 'backward-word)
(global-set-key (kbd "M-h") 'backward-char)
(global-set-key (kbd "M-i") 'kill-line)
(global-set-key (kbd "M-j") 'xah-copy-line-or-region)
(global-set-key (kbd "M-k") 'yank)
(global-set-key (kbd "M-l") 'recenter-top-bottom)
(global-set-key (kbd "M-m") 'hippie-expand)
(global-set-key (kbd "M-n") 'forward-char)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-p") 'kill-word)
(global-set-key (kbd "M-q") 'xah-cut-line-or-region)
(global-set-key (kbd "M-r") 'forward-word)
(global-set-key (kbd "M-t") 'next-line)
(global-set-key (kbd "M-u") 'delete-char)

