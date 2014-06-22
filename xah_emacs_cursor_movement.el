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

(defun xah-forward-block (&optional φnumber)
  "Move cursor forward to the beginning of next text block.
A text block is separated by 2 empty lines (or line with just whitespace).
In most major modes, this is similar to `forward-paragraph', but this command's behavior is the same regardless of syntax table.

With a prefix argument ΦNUMBER, move forward ΦNUMBER blocks.
With a negative prefix argument ΦNUMBER, move backward ΦNUMBER blocks."
  (interactive "p")
  (if (and φnumber (> 0 φnumber))
      (xah-backward-block (- 0 φnumber))
  (if (search-forward-regexp "\n[[:blank:]\n]*\n+" nil "NOERROR" φnumber)
      (progn (backward-char))
    (progn (goto-char (point-max))))))

(defun xah-backward-block (&optional φnumber)
  "Move cursor backward to previous text block.
See: `xah-forward-block'"
  (interactive "p")
  (if (and φnumber
           (> 0 φnumber))
      (xah-forward-block (- 0 φnumber))
    (if (search-backward-regexp "\n[\t\n ]*\n+" nil "NOERROR" φnumber)
        (progn
          (skip-chars-backward "\n\t ")
          (forward-char 1))
      (progn (goto-char (point-min))))))

(defun xah-beginning-of-line-or-block ()
  "Move cursor to beginning of line, or beginning of current or previous text block.
 (a text block is separated by blank lines)"
  (interactive)
  (if (equal (point) (line-beginning-position))
      (xah-backward-block)
    (beginning-of-line)
    ))

(defun xah-end-of-line-or-block ()
  "Move cursor to end of line, or end of current or next text block.
 (a text block is separated by blank lines)"
  (interactive)
  (if (equal (point) (line-end-position))
      (xah-forward-block)
    (end-of-line)))

(defvar xah-brackets-open nil "list of open bracket chars.")
(setq xah-brackets-open '("(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「" "『" "“" "‘" "‹" "«") )

(defvar xah-brackets-close nil "list of close bracket chars.")
(setq xah-brackets-close '(")" "]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»") )

(defvar xah-ascii-quotes nil "list of quotation chars.")
(setq xah-ascii-quotes '("'" "\"") )

(defvar xah-punctuations nil "list of punctuation chars for easy jump. Typically exclude things that are too common, such as underscore or slash.")
(setq xah-punctuations '("=" "$" "#" "+" "*" ";" "." "," "\\" "&" "@" "%" "!" "?" "^" "`" "~") )

(defvar xah-punctuation-regex nil "a regex string for the purpose of jumping to punctuations in programing modes.")
(setq xah-punctuation-regex "[=.*+,#$%&:;<>@^`~!\?\|]+")

(defun xah-forward-punct (&optional φnumber)
  "Move cursor to the next occurrence of punctuation.

The list of punctuations to jump to is defined by `xah-punctuations'"
  (interactive "p")
  (if (and φnumber (> 0 φnumber))
      (xah-backward-punct (- 0 φnumber))
    (forward-char 1)
    (search-forward-regexp xah-punctuation-regex nil t φnumber)
    (backward-char 1)))

(defun xah-backward-punct (&optional φnumber)
  "Move cursor to the previous occurrence of punctuation.

The list of punctuations to jump to is defined by `xah-punctuations'"
  (interactive "p")
  (if (and φnumber (> 0 φnumber))
      (xah-forward-punct (- 0 φnumber))
    (search-backward-regexp xah-punctuation-regex nil t φnumber)))

;; (eval-when-compile (regexp-opt xah-punctuations))

(defun xah-forward-all-bracket (&optional φnumber)
  "Move cursor to the next occurrence of left bracket or quotation mark.
With prefix ΦNUMBER, move forward to the next ΦNUMBER left bracket or quotation mark.
With a negative prefix ΦNUMBER, move backward to the previous ΦNUMBER left bracket or quotation mark.

The list of brackets to jump to is defined by `xah-brackets-open' and `xah-brackets-close' and `xah-ascii-quotes'"

  (interactive "p")
  (if (and φnumber (> 0 φnumber))
      (xah-backward-open-bracket (- 0 φnumber))
    (forward-char 1)
    (search-forward-regexp (eval-when-compile (regexp-opt (append xah-brackets-open xah-brackets-close xah-ascii-quotes ))) nil t φnumber)
    (backward-char 1)))

(defun xah-backward-all-bracket (&optional φnumber)
  "Move cursor to the previous occurrence of left bracket or quotation mark.
With prefix argument ΦNUMBER, move backward ΦNUMBER open brackets.
With a negative prefix ΦNUMBER, move forward ΦNUMBER open brackets.

The list of brackets to jump to is defined by `xah-brackets-open' and `xah-brackets-close' and `xah-ascii-quotes'"
  (interactive "p")
  (if (and φnumber (> 0 φnumber))
      (xah-forward-open-bracket (- 0 φnumber))
    (search-backward-regexp (eval-when-compile (regexp-opt (append xah-brackets-open xah-brackets-close xah-ascii-quotes ))) nil t φnumber)))

(defun xah-forward-open-bracket (&optional φnumber)
  "Move cursor to the next occurrence of left bracket or quotation mark.
With prefix ΦNUMBER, move forward to the next ΦNUMBER left bracket or quotation mark.
With a negative prefix ΦNUMBER, move backward to the previous ΦNUMBER left bracket or quotation mark."
  (interactive "p")
  (if (and φnumber (> 0 φnumber))
      (xah-backward-open-bracket (- 0 φnumber))
    (forward-char 1)
    (search-forward-regexp (eval-when-compile (regexp-opt xah-brackets-open)) nil t φnumber)
    (backward-char 1)))

(defun xah-backward-open-bracket (&optional φnumber)
  "Move cursor to the previous occurrence of left bracket or quotation mark.
With prefix argument ΦNUMBER, move backward ΦNUMBER open brackets.
With a negative prefix ΦNUMBER, move forward ΦNUMBER open brackets.

The list of brackets to jump to is defined by `xah-brackets-open'."
  (interactive "p")
  (if (and φnumber (> 0 φnumber))
      (xah-forward-open-bracket (- 0 φnumber))
    (search-backward-regexp (eval-when-compile (regexp-opt xah-brackets-open)) nil t φnumber)))

(defun xah-forward-close-bracket (&optional φnumber)
  "Move cursor to the next occurrence of right bracket or quotation mark.
With a prefix argument ΦNUMBER, move forward ΦNUMBER closed bracket.
With a negative prefix argument ΦNUMBER, move backward ΦNUMBER closed brackets.

The list of brackets to jump to is defined by `xah-brackets-close'."
  (interactive "p")
  (if (and φnumber
           (> 0 φnumber))
      (xah-backward-close-bracket (- 0 φnumber))
    (search-forward-regexp
     (eval-when-compile
       (regexp-opt xah-brackets-close)) nil t φnumber)))

(defun xah-backward-close-bracket (&optional φnumber)
  "Move cursor to the previous occurrence of right bracket or quotation mark.
With a prefix argument ΦNUMBER, move backward ΦNUMBER closed brackets.
With a negative prefix argument ΦNUMBER, move forward ΦNUMBER closed brackets.

The list of brackets to jump to is defined by `xah-brackets-close'."
  (interactive "p")
  (if (and φnumber (> 0 φnumber))
      (xah-forward-close-bracket (- 0 φnumber))
    (backward-char 1)
    (search-backward-regexp (eval-when-compile (regexp-opt xah-brackets-close)) nil t φnumber)
    (forward-char 1)))

(defun xah-forward-quote (&optional φnumber)
  "Move cursor to the next occurrence of ASCII quotation mark, single or double.
With prefix ΦNUMBER, move forward to the next ΦNUMBER quotation mark.
With a negative prefix ΦNUMBER, move backward to the previous ΦNUMBER quotation mark.

The list of quotes to jump to is defined by `xah-ascii-quotes'."
  (interactive "p")
  (if (and φnumber (> 0 φnumber))
      (xah-forward-quote (- 0 φnumber))
    (search-forward-regexp (eval-when-compile (regexp-opt xah-ascii-quotes)) nil t φnumber)
    ))

(defun xah-backward-quote (&optional φnumber)
  "Move cursor to the previous occurrence of ASCII quotation mark, single or double.
With prefix argument ΦNUMBER, move backward ΦNUMBER quotation mark.
With a negative prefix ΦNUMBER, move forward ΦNUMBER quotation mark.

The list of quotes to jump to is defined by `xah-ascii-quotes'."
  (interactive "p")
  (if (and φnumber (> 0 φnumber)) (xah-backward-quote (- 0 φnumber))
    (search-backward-regexp (eval-when-compile (regexp-opt xah-ascii-quotes)) nil t φnumber)))

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
