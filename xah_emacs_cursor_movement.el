;; -*- coding: utf-8 -*-
;; some general cursor movement commands
;; 2011-05-27
;;   Xah Lee
;; ∑ http://xahlee.org/

(defun xah-scroll-down-10-lines ()
  "scroll down 10 lines"
  (interactive)
  (scroll-down 10))

(defun xah-scroll-up-10-lines ()
  "scroll up 10 lines"
  (interactive)
  (scroll-up 10))

(defun xah-cursor-down-10-lines ()
  "Move cursor down 10 logical lines"
  (interactive)
  (forward-line 10)
)

(defun xah-cursor-up-10-lines ()
  "Move cursor up 10 logical lines"
  (interactive)
  (forward-line -10))

(defun xah-forward-block (&optional number)
  "Move cursor forward to the beginning of next text block.
A text block is separated by 2 empty lines (or line with just whitespace).
In most major modes, this is similar to `forward-paragraph', but this command's behavior is the same regardless of syntax table.

With a prefix argument NUMBER, move forward NUMBER blocks.
With a negative prefix argument NUMBER, move backward NUMBER blocks."
  (interactive "p")
  (if (and number (> 0 number))
      (xah-backward-block (- 0 number))
  (if (search-forward-regexp "\n[[:blank:]\n]*\n+" nil "NOERROR" number)
      (progn (backward-char))
    (progn (goto-char (point-max))))))

(defun xah-backward-block (&optional number)
  "Move cursor backward to previous text block.
See: `xah-forward-block'"
  (interactive "p")
  (if (and number
           (> 0 number))
      (xah-forward-block (- 0 number))
    (if (search-backward-regexp "\n[\t\n ]*\n+" nil "NOERROR" number)
        (progn
          (skip-chars-backward "\n\t ")
          (forward-char 1))
      (progn (goto-char (point-min))))))

(defun xah-beginning-of-line-or-block ()
  "Move cursor to beginning of line, or beginning of current or previous text block.
 (a text block is separated by empty lines)"
  (interactive)
  (if (or (equal last-command this-command )
          (equal last-command 'xah-end-of-line-or-block ) )
      (xah-backward-block)
    (beginning-of-line)
    ))

(defun xah-end-of-line-or-block ()
  "Move cursor to end of line, or end of current or next text block.
 (a text block is separated by empty lines)"
  (interactive)
  (if (or (equal last-command this-command )
          (equal last-command 'xah-beginning-of-line-or-block ) )
      (xah-forward-block)
    (end-of-line)
    ))

(setq xah-brackets-open '("(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「" "『" "“" "‘" "‹" "«") )
(setq xah-brackets-close '(")" "]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»") )
(setq xah-ascii-quotes '("'" "\"") )

(defun xah-forward-all-bracket (&optional number)
  "Move cursor to the next occurrence of left bracket or quotation mark.
With prefix NUMBER, move forward to the next NUMBER left bracket or quotation mark.
With a negative prefix NUMBER, move backward to the previous NUMBER left bracket or quotation mark."
  (interactive "p")
  (if (and number (> 0 number))
      (xah-backward-open-bracket (- 0 number))
    (forward-char 1)
    (search-forward-regexp (eval-when-compile (regexp-opt (append xah-brackets-open xah-brackets-close xah-ascii-quotes ))) nil t number)
    (backward-char 1)))

(defun xah-backward-all-bracket (&optional number)
  "Move cursor to the previous occurrence of left bracket or quotation mark.
With prefix argument NUMBER, move backward NUMBER open brackets.
With a negative prefix NUMBER, move forward NUMBER open brackets."
  (interactive "p")
  (if (and number (> 0 number))
      (xah-forward-open-bracket (- 0 number))
    (search-backward-regexp (eval-when-compile (regexp-opt (append xah-brackets-open xah-brackets-close xah-ascii-quotes ))) nil t number)))

(defun xah-forward-open-bracket (&optional number)
  "Move cursor to the next occurrence of left bracket or quotation mark.
With prefix NUMBER, move forward to the next NUMBER left bracket or quotation mark.
With a negative prefix NUMBER, move backward to the previous NUMBER left bracket or quotation mark."
  (interactive "p")
  (if (and number (> 0 number))
      (xah-backward-open-bracket (- 0 number))
    (forward-char 1)
    (search-forward-regexp (eval-when-compile (regexp-opt xah-brackets-open)) nil t number)
    (backward-char 1)))

(defun xah-backward-open-bracket (&optional number)
  "Move cursor to the previous occurrence of left bracket or quotation mark.
With prefix argument NUMBER, move backward NUMBER open brackets.
With a negative prefix NUMBER, move forward NUMBER open brackets."
  (interactive "p")
  (if (and number (> 0 number))
      (xah-forward-open-bracket (- 0 number))
    (search-backward-regexp (eval-when-compile (regexp-opt xah-brackets-open)) nil t number)))

(defun xah-forward-close-bracket (&optional number)
  "Move cursor to the next occurrence of right bracket or quotation mark.
With a prefix argument NUMBER, move forward NUMBER closed bracket.
With a negative prefix argument NUMBER, move backward NUMBER closed brackets."
  (interactive "p")
  (if (and number
           (> 0 number))
      (xah-backward-close-bracket (- 0 number))
    (search-forward-regexp
     (eval-when-compile
       (regexp-opt xah-brackets-close)) nil t number)))

(defun xah-backward-close-bracket (&optional number)
  "Move cursor to the previous occurrence of right bracket or quotation mark.
With a prefix argument NUMBER, move backward NUMBER closed brackets.
With a negative prefix argument NUMBER, move forward NUMBER closed brackets."
  (interactive "p")
  (if (and number (> 0 number))
      (xah-forward-close-bracket (- 0 number))
    (backward-char 1)
    (search-backward-regexp (eval-when-compile (regexp-opt xah-brackets-close)) nil t number)
    (forward-char 1)))

(defun xah-forward-quote (&optional number)
  "Move cursor to the next occurrence of ASCII quotation mark, single or double.
With prefix NUMBER, move forward to the next NUMBER quotation mark.
With a negative prefix NUMBER, move backward to the previous NUMBER quotation mark."
  (interactive "p")
  (if (and number (> 0 number))
      (xah-forward-quote (- 0 number))
    (search-forward-regexp (eval-when-compile (regexp-opt xah-ascii-quotes)) nil t number)
    ))

(defun xah-backward-quote (&optional number)
  "Move cursor to the previous occurrence of ASCII quotation mark, single or double.
With prefix argument NUMBER, move backward NUMBER quotation mark.
With a negative prefix NUMBER, move forward NUMBER quotation mark."
  (interactive "p")
  (if (and number (> 0 number)) (xah-backward-quote (- 0 number))
    (search-backward-regexp (eval-when-compile (regexp-opt xah-ascii-quotes)) nil t number)))

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
