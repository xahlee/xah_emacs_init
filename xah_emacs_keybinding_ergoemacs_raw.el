;; -*- coding: utf-8 -*-

;; 2013-11-09
;; raw setup, based on ergoemacs-mode

(global-set-key (kbd "M-\"") 'xah-compact-uncompact-Block)
(global-set-key (kbd "M-2") 'delete-window)
(global-set-key (kbd "M-9") 'xah-select-text-in-quote)
(global-set-key (kbd "M-s") 'xah-toggle-letter-case)

(global-set-key (kbd "M-SPC") 'set-mark-command)

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

;(global-set-key (kbd "<escape>") 'keyboard-quit)
(global-set-key (kbd "<f2>") 'xah-cut-line-or-region)
(global-set-key (kbd "<f3>") 'xah-copy-line-or-region)
(global-set-key (kbd "<f4>") 'yank)
(global-set-key (kbd "<C-f4>") 'yank-pop)
(global-set-key (kbd "<f5>") 'undo)
(global-set-key (kbd "<C-f5>") 'redo)

(global-set-key (kbd "<C-S-iso-lefttab>") 'xah-previous-user-buffer)
(global-set-key (kbd "<C-tab>") 'xah-next-user-buffer)

(global-set-key (kbd "C-S-t") 'xah-open-last-closed) ; 832    0.05%  xah-open-last-closed

(global-set-key (kbd "C-w") 'xah-close-current-buffer) ; 19318    1.20%  xah-close-current-buffer
(global-set-key (kbd "C-z") 'comment-dwim) ; 1214    0.08%  comment-dwim



