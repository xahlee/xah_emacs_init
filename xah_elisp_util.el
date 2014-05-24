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
