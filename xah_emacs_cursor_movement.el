;; -*- coding: utf-8 -*-
;; some general cursor movement commands
;; 2011-05-27
;;   Xah Lee
;; ∑ http://xahlee.org/


;; (defun forward-section ()
;;   "Move cursor forward to next occurrence of the SECTION SIGN § char (unicode 167)."
;;   (interactive)
;;   (when (not (search-forward-regexp "§" nil t))
;;     (goto-char (point-max)) ) )

;; (defun backward-section ()
;;   "Move cursor forward to previous occurrence of the SECTION SIGN § char (unicode 167)."
;;   (interactive)
;;   (when (not (search-backward-regexp "§" nil t))
;;     (goto-char (point-min)) ) )


;; (defun goto-point-min ()
;;   "Goto the beginning of buffer.
;; This is different from `beginning-of-buffer'
;; because that marks the previous position."
;;   (interactive)
;;   (goto-char (point-min))
;; )

;; (defun goto-point-max ()
;;   "Goto the end of buffer.
;; This is different from `end-of-buffer'
;; because that marks the previous position."
;;   (interactive)
;;   (goto-char (point-max))
;; )

(defun beginning-of-line-or-block ()
  "Move cursor to beginning of line, or beginning of current or previous text block.
 (a text block is separated by empty lines)"
  (interactive)
  (if (or (equal last-command this-command )
          (equal last-command 'end-of-line-or-block ) )
      (ergoemacs-backward-block)
    (beginning-of-line)
    ))

(defun end-of-line-or-block ()
  "Move cursor to end of line, or end of current or next text block.
 (a text block is separated by empty lines)"
  (interactive)
  (if (or (equal last-command this-command )
          (equal last-command 'beginning-of-line-or-block ) )
      (ergoemacs-forward-block)
    (end-of-line)
    ))

(defun forward-quote-symbol (&optional εnumber)
  "Move cursor to the next occurrence ASCII double quote symbol
With a negative prefix argument NUMBER, move backward NUMBER closed brackets."
  (interactive "p")
  (if (and εnumber (> 0 εnumber))
      (forward-quote-symbol (- 0 εnumber))
    (search-forward-regexp (eval-when-compile (regexp-opt '("\""))) nil t εnumber)))
