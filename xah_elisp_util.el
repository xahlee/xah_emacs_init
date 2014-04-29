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

(defun xah-replace-BOM-mark-etc ()
  "Query replace Unicode some invisible Unicode chars.
The chars to be searched are:
 RIGHT-TO-LEFT MARK 8207 x200f
 ZERO WIDTH NO-BREAK SPACE 65279 xfeff

start on cursor position to end.
    "
  (interactive)
  (let ()
    (query-replace-regexp "\u200f\\|\ufeff" "")
    ))

(defun xah-show-hexadecimal-value ()
  "Prints the decimal value of a hexadecimal string under cursor.
Samples of valid input:

  ffaf
  ffff
  0xffff
  #xffff
  FFFF
  0xFFFF
  #xFFFF

Test cases
  64*0xc8+#x12c 190*0x1f4+#x258
  100 200 300   400 500 600"
  (interactive )

  (let (inputStr tempStr p1 p2
                 (case-fold-search t) )
    (save-excursion
      ;; (skip-chars-backward "0123456789abcdef")
      ;; (search-backward-regexp "[[:xdigit:]]+" nil t)
      (search-backward-regexp "[0123456789abcdef]+" nil t)
      (setq p1 (point) )
      (search-forward-regexp "[0123456789abcdef]+" nil t)
      (setq p2 (point) )

      (setq inputStr (buffer-substring-no-properties p1 p2) )

      (let ((case-fold-search nil) )
        (setq tempStr (replace-regexp-in-string "\\`0x" "" inputStr )) ; C, Perl, …
        (setq tempStr (replace-regexp-in-string "\\`#x" "" tempStr )) ; elisp …
        (setq tempStr (replace-regexp-in-string "\\`#" "" tempStr ))  ; CSS …
        )

      ;; (message "Hex 「%s」 is 「%d」" tempStr (string-to-number tempStr 16 ) )
      (message "input 「%s」 Hex 「%s」 is 「%d」" inputStr tempStr (string-to-number tempStr 16 ) ) ) ))
