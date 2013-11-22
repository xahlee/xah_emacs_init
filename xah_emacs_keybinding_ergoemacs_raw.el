;; -*- coding: utf-8 -*-

;; 2013-11-09
;; raw setup, based on ergoemacs-mode

(global-set-key (kbd "M-\"") 'xah-compact-uncompact-Block)
(global-set-key (kbd "M-2") 'delete-window)
(global-set-key (kbd "M-9") 'ergoemacs-select-text-in-quote)
(global-set-key (kbd "M-s") 'ergoemacs-toggle-letter-case)


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
(global-set-key (kbd "M-d") 'ergoemacs-beginning-of-line-or-block)
(global-set-key (kbd "M-q") 'ergoemacs-cut-line-or-region)
(global-set-key (kbd "M-j") 'ergoemacs-copy-line-or-region)
(global-set-key (kbd "M-k") 'yank)


(global-set-key (kbd "M-,") 'ergoemacs-shrink-whitespaces) ;5852    0.36%  ergoemacs-shrink-whitespaces
(global-set-key (kbd "M-'") 'ergoemacs-compact-uncompact-block) ;1037    0.06%  ergoemacs-compact-uncompact-block

(global-set-key (kbd "M-6") 'ergoemacs-select-current-block) ; 3107    0.19%  ergoemacs-select-current-block
(global-set-key (kbd "M-7") 'ergoemacs-select-current-line) ; 2526    0.16%  ergoemacs-select-current-line
(global-set-key (kbd "M-8") 'ergoemacs-extend-selection) ; 3332    0.21%  ergoemacs-extend-selection
(global-set-key (kbd "M-9") 'ergoemacs-select-text-in-quote) ; 4603    0.28%  ergoemacs-select-text-in-quote

(global-set-key (kbd "M-f") 'isearch-forward)


;(global-set-key (kbd "<escape>") 'keyboard-quit)
(global-set-key (kbd "<f2>") 'ergoemacs-cut-line-or-region)
(global-set-key (kbd "<f3>") 'ergoemacs-copy-line-or-region)
(global-set-key (kbd "<f4>") 'yank)
(global-set-key (kbd "<C-f4>") 'yank-pop)
(global-set-key (kbd "<f5>") 'undo)
(global-set-key (kbd "<C-f5>") 'redo)


(global-set-key (kbd "<C-S-iso-lefttab>") 'ergoemacs-previous-user-buffer)
(global-set-key (kbd "<C-tab>") 'ergoemacs-next-user-buffer)

(global-set-key (kbd "C-S-t") 'ergoemacs-open-last-closed) ; 832    0.05%  ergoemacs-open-last-closed

(global-set-key (kbd "C-w") 'ergoemacs-close-current-buffer) ; 19318    1.20%  ergoemacs-close-current-buffer
(global-set-key (kbd "C-z") 'comment-dwim) ; 1214    0.08%  comment-dwim


(defun ergoemacs-forward-open-bracket (&optional number)
  "Move cursor to the next occurrence of left bracket or quotation mark.

With prefix NUMBER, move forward to the next NUMBER left bracket or quotation mark.

With a negative prefix NUMBER, move backward to the previous NUMBER left bracket or quotation mark."
  (interactive "p")
  (if (and number
           (> 0 number))
      (ergoemacs-backward-open-bracket (- 0 number))
    (forward-char 1)
    (search-forward-regexp
     (eval-when-compile
       (regexp-opt
        '("(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「" "『" "“" "‘" "‹" "«"))) nil t number)
    (backward-char 1)))

(defun ergoemacs-backward-open-bracket (&optional number)
  "Move cursor to the previous occurrence of left bracket or quotation mark.
With prefix argument NUMBER, move backward NUMBER open brackets.
With a negative prefix NUMBER, move forward NUMBER open brackets."
  (interactive "p")
  (if (and number
           (> 0 number))
      (ergoemacs-forward-open-bracket (- 0 number))
    (search-backward-regexp
   (eval-when-compile
     (regexp-opt
      '("(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「" "『" "“" "‘" "‹" "«"))) nil t number)))

(defun ergoemacs-forward-close-bracket (&optional number)
  "Move cursor to the next occurrence of right bracket or quotation mark.
With a prefix argument NUMBER, move forward NUMBER closed bracket.
With a negative prefix argument NUMBER, move backward NUMBER closed brackets."
  (interactive "p")
  (if (and number
           (> 0 number))
      (ergoemacs-backward-close-bracket (- 0 number))
    (search-forward-regexp
     (eval-when-compile
       (regexp-opt '(")" "]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»"))) nil t number)))

(defun ergoemacs-backward-close-bracket (&optional number)
  "Move cursor to the previous occurrence of right bracket or quotation mark.
With a prefix argument NUMBER, move backward NUMBER closed brackets.
With a negative prefix argument NUMBER, move forward NUMBER closed brackets."
  (interactive "p")
  (if (and number
           (> 0 number))
      (ergoemacs-forward-close-bracket (- 0 number))
    (backward-char 1)
    (search-backward-regexp
     (eval-when-compile
       (regexp-opt '(")" "]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»"))) nil t number)
    (forward-char 1)))

(defun ergoemacs-forward-quote (&optional number)
  "Move cursor to the next occurrence of ASCII quotation mark, single or double.

With prefix NUMBER, move forward to the next NUMBER quotation mark.

With a negative prefix NUMBER, move backward to the previous NUMBER quotation mark."
  (interactive "p")
  (if (and number (> 0 number))
      (ergoemacs-forward-quote (- 0 number))
    (search-forward-regexp (eval-when-compile (regexp-opt '("\"" "'"))) nil t number)
    ))

(defun ergoemacs-backward-quote (&optional number)
  "Move cursor to the previous occurrence of ASCII quotation mark, single or double.
With prefix argument NUMBER, move backward NUMBER quotation mark.
With a negative prefix NUMBER, move forward NUMBER quotation mark."
  (interactive "p")
  (if (and number (> 0 number)) (ergoemacs-backward-quote (- 0 number))
    (search-backward-regexp (eval-when-compile (regexp-opt '("\"" "'"))) nil t number)))

