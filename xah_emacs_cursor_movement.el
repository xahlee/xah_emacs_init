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
  (forward-line 10))

(defun xah-cursor-up-10-lines ()
  "Move cursor up 10 logical lines"
  (interactive)
  (forward-line -10))

(defun xah-forward-block (&optional φn)
  "Move cursor forward to the beginning of next text block.
A text block is separated by 2 empty lines (or line with just whitespace).
In most major modes, this is similar to `forward-paragraph', but this command's behavior is the same regardless of syntax table."
  (interactive "p")
  (if (search-forward-regexp "\n[[:blank:]\n]*\n+" nil "NOERROR" φn)
      (progn (point))
    (progn (goto-char (point-max)))))

(defun xah-backward-block (&optional φn)
  "Move cursor backward to previous text block.
See: `xah-forward-block'"
  (interactive)
  (if (search-backward-regexp "\n[\t\n ]*\n+" nil "NOERROR" φn)
      (progn
        (skip-chars-backward "\n\t "))
    (progn (goto-char (point-min)))))

(defun xah-beginning-of-line-or-block (&optional φn)
  "Move cursor to beginning of line, or beginning of current or previous text block.
 (a text block is separated by blank lines)"
  (interactive "p")
  (if (equal φn 1)
      (if (or (equal (point) (line-beginning-position))
              (equal last-command this-command )
              (equal last-command 'xah-end-of-line-or-block ))
          (xah-backward-block)
        (beginning-of-line))
    (xah-backward-block φn)))

(defun xah-end-of-line-or-block (&optional φn)
  "Move cursor to end of line, or end of current or next text block.
 (a text block is separated by blank lines)"
  (interactive "p")
  (if (equal φn 1)
      (if (or (equal (point) (line-end-position))
              (equal last-command this-command )
              (equal last-command 'xah-beginning-of-line-or-block ))
          (xah-forward-block)
        (end-of-line))
    (progn (xah-forward-block φn))
    ))

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

(defun xah-forward-punct (&optional φn)
  "Move cursor to the next occurrence of punctuation.

The list of punctuations to jump to is defined by `xah-punctuations'"
  (interactive "p")
  (if (and φn (> 0 φn))
      (xah-backward-punct (- 0 φn))
    (forward-char 1)
    (search-forward-regexp xah-punctuation-regex nil t φn)
    (backward-char 1)))

(defun xah-backward-punct (&optional φn)
  "Move cursor to the previous occurrence of punctuation.

The list of punctuations to jump to is defined by `xah-punctuations'"
  (interactive "p")
  (if (and φn (> 0 φn))
      (xah-forward-punct (- 0 φn))
    (search-backward-regexp xah-punctuation-regex nil t φn)))

;; (eval-when-compile (regexp-opt xah-punctuations))

(defun xah-forward-all-bracket (&optional φn)
  "Move cursor to the next occurrence of left bracket or quotation mark.
With prefix ΦN, move forward to the next ΦN left bracket or quotation mark.
With a negative prefix ΦN, move backward to the previous ΦN left bracket or quotation mark.

The list of brackets to jump to is defined by `xah-brackets-open' and `xah-brackets-close' and `xah-ascii-quotes'"

  (interactive "p")
  (if (and φn (> 0 φn))
      (xah-backward-open-bracket (- 0 φn))
    (forward-char 1)
    (search-forward-regexp (eval-when-compile (regexp-opt (append xah-brackets-open xah-brackets-close xah-ascii-quotes ))) nil t φn)
    (backward-char 1)))

(defun xah-backward-all-bracket (&optional φn)
  "Move cursor to the previous occurrence of left bracket or quotation mark.
With prefix argument ΦN, move backward ΦN open brackets.
With a negative prefix ΦN, move forward ΦN open brackets.

The list of brackets to jump to is defined by `xah-brackets-open' and `xah-brackets-close' and `xah-ascii-quotes'"
  (interactive "p")
  (if (and φn (> 0 φn))
      (xah-forward-open-bracket (- 0 φn))
    (search-backward-regexp (eval-when-compile (regexp-opt (append xah-brackets-open xah-brackets-close xah-ascii-quotes ))) nil t φn)))

(defun xah-forward-open-bracket (&optional φn)
  "Move cursor to the next occurrence of left bracket or quotation mark.
With prefix ΦN, move forward to the next ΦN left bracket or quotation mark.
With a negative prefix ΦN, move backward to the previous ΦN left bracket or quotation mark."
  (interactive "p")
  (if (and φn (> 0 φn))
      (xah-backward-open-bracket (- 0 φn))
    (forward-char 1)
    (search-forward-regexp (eval-when-compile (regexp-opt xah-brackets-open)) nil t φn)
    (backward-char 1)))

(defun xah-backward-open-bracket (&optional φn)
  "Move cursor to the previous occurrence of left bracket or quotation mark.
With prefix argument ΦN, move backward ΦN open brackets.
With a negative prefix ΦN, move forward ΦN open brackets.

The list of brackets to jump to is defined by `xah-brackets-open'."
  (interactive "p")
  (if (and φn (> 0 φn))
      (xah-forward-open-bracket (- 0 φn))
    (search-backward-regexp (eval-when-compile (regexp-opt xah-brackets-open)) nil t φn)))

(defun xah-forward-close-bracket (&optional φn)
  "Move cursor to the next occurrence of right bracket or quotation mark.
With a prefix argument ΦN, move forward ΦN closed bracket.
With a negative prefix argument ΦN, move backward ΦN closed brackets.

The list of brackets to jump to is defined by `xah-brackets-close'."
  (interactive "p")
  (if (and φn
           (> 0 φn))
      (xah-backward-close-bracket (- 0 φn))
    (search-forward-regexp
     (eval-when-compile
       (regexp-opt xah-brackets-close)) nil t φn)))

(defun xah-backward-close-bracket (&optional φn)
  "Move cursor to the previous occurrence of right bracket or quotation mark.
With a prefix argument ΦN, move backward ΦN closed brackets.
With a negative prefix argument ΦN, move forward ΦN closed brackets.

The list of brackets to jump to is defined by `xah-brackets-close'."
  (interactive "p")
  (if (and φn (> 0 φn))
      (xah-forward-close-bracket (- 0 φn))
    (backward-char 1)
    (search-backward-regexp (eval-when-compile (regexp-opt xah-brackets-close)) nil t φn)
    (forward-char 1)))

(defun xah-forward-quote (&optional φn)
  "Move cursor to the next occurrence of ASCII quotation mark, single or double.
With prefix ΦN, move forward to the next ΦN quotation mark.
With a negative prefix ΦN, move backward to the previous ΦN quotation mark.

The list of quotes to jump to is defined by `xah-ascii-quotes'."
  (interactive "p")
  (if (and φn (> 0 φn))
      (xah-forward-quote (- 0 φn))
    (search-forward-regexp (eval-when-compile (regexp-opt xah-ascii-quotes)) nil t φn)
    ))

(defun xah-backward-quote (&optional φn)
  "Move cursor to the previous occurrence of ASCII quotation mark, single or double.
With prefix argument ΦN, move backward ΦN quotation mark.
With a negative prefix ΦN, move forward ΦN quotation mark.

The list of quotes to jump to is defined by `xah-ascii-quotes'."
  (interactive "p")
  (if (and φn (> 0 φn)) (xah-backward-quote (- 0 φn))
    (search-backward-regexp (eval-when-compile (regexp-opt xah-ascii-quotes)) nil t φn)))

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
