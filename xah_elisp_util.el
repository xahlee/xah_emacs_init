;-*- coding: utf-8 -*-
; A collection of elisp functions

; 2008-11-20
;   Xah Lee
; ∑ http://xahlee.org/



(require 'calc-bin)

(defun xah-dec-to-bin (decStr)
"convert the decimal number string decStr into a binary (string)"
  (let ((calc-number-radix 2))
    (math-format-radix (string-to-number decStr))))

(defun xah-trim-string (φstring)
  "Remove white spaces in beginning and ending of φstring.
White space here is any of: space, tab, emacs newline (line feed, ASCII 10).

Note: in emacs GNU Emacs 24.4+ and later, there's `string-trim' function. You need to (require 'subr-x).
"
  (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" φstring)))