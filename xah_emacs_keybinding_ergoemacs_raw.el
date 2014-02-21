;; -*- coding: utf-8 -*-
;; 2013-11-09
;; raw setup, similar to ergoemacs-mode
;; http://ergoemacs.org/emacs/ergonomic_emacs_keybinding.html

(global-set-key (kbd "M-\"") 'xah-compact-uncompact-Block)
(global-set-key (kbd "M-9") 'xah-select-text-in-quote)

(global-set-key (kbd "M-SPC") 'set-mark-command)
(global-set-key (kbd "M-b") 'xah-toggle-letter-case)

(global-set-key (kbd "M-g") 'backward-word)
(global-set-key (kbd "M-r") 'forward-word)

(global-set-key (kbd "M-h") 'backward-char)
(global-set-key (kbd "M-n") 'forward-char)
(global-set-key (kbd "M-t") 'next-line)
(global-set-key (kbd "M-c") 'previous-line)

(global-set-key (kbd "M-e") 'delete-backward-char)
(global-set-key (kbd "M-u") 'delete-char)
(global-set-key (kbd "M-.") 'backward-kill-word)
(global-set-key (kbd "M-p") 'kill-word)

(global-set-key (kbd "M-i") 'kill-line)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-d") 'xah-beginning-of-line-or-block)
(global-set-key (kbd "M-q") 'xah-cut-line-or-region)
(global-set-key (kbd "M-j") 'xah-copy-line-or-region)
(global-set-key (kbd "M-k") 'yank)

(global-set-key (kbd "M-,") 'xah-shrink-whitespaces) ;5852    0.36%  xah-shrink-whitespaces
(global-set-key (kbd "M-'") 'xah-compact-uncompact-block) ;1037    0.06%  xah-compact-uncompact-block

(global-set-key (kbd "M-6") 'xah-select-current-block) ; 3107    0.19%  xah-select-current-block
(global-set-key (kbd "M-7") 'xah-select-current-line) ; 2526    0.16%  xah-select-current-line
(global-set-key (kbd "M-8") 'xah-extend-selection) ; 3332    0.21%  xah-extend-selection
(global-set-key (kbd "M-9") 'xah-select-text-in-quote) ; 4603    0.28%  xah-select-text-in-quote

(global-set-key (kbd "M-f") 'isearch-forward)

(global-set-key (kbd "M-m") 'hippie-expand)

(global-set-key (kbd "M-`") 'other-frame)

(global-set-key (kbd "M-l") nil)

