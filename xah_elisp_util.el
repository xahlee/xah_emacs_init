;-*- coding: utf-8 -*-
; A collection of elisp functions

; 2008-11-20
;   Xah Lee
; ∑ http://xahlee.org/


(require 'xfrp_find_replace_pairs)
(require 'xeu_elisp_util)

(defun remove-vowel-old (&optional ξstring ξfrom ξto)
  "Remove the following letters: {a e i o u}.

When called interactively, work on current text block or text selection. (a “text block” is text between empty lines)

When called in lisp code, if ξstring is non-nil, returns a changed string.  If ξstring nil, change the text in the region between positions ξfrom ξto."
  (interactive
   (if (region-active-p)
       (list nil (region-beginning) (region-end))
     (let ((bds (bounds-of-thing-at-point 'paragraph)) )
       (list nil (car bds) (cdr bds)) ) ) )

  (let (workOnStringP inputStr outputStr)
    (setq workOnStringP (if ξstring t nil))
    (setq inputStr (if workOnStringP ξstring (buffer-substring-no-properties ξfrom ξto)))
    (setq outputStr
          (let ((case-fold-search t))
            (replace-regexp-in-string "a\\|e\\|i\\|o\\|u\\|" "" inputStr) )  )

    (if workOnStringP
        outputStr
      (save-excursion
        (delete-region ξfrom ξto)
        (goto-char ξfrom)
        (insert outputStr) )) ) )

(defun remove-vowel (ξstring &optional ξfrom-to-pair)
  "Remove the following letters: {a e i o u}.

When called interactively, work on current text block or text selection. (a “text block” is text between empty lines)

When called in lisp code, if ξfrom-to-pair is non-nil, change the text
in the region between positions [from to]. ξfrom-to-pair should be a
list or vector pair.  Else, returns a changed string."
  (interactive
   (if (region-active-p)
       (list nil (vector (region-beginning) (region-end)))
     (let ((bds (bounds-of-thing-at-point 'paragraph)) )
       (list nil (vector (car bds) (cdr bds))) ) ) )

  (let (workOnStringP inputStr outputStr ξfrom ξto )
    (when ξfrom-to-pair
        (setq ξfrom (elt ξfrom-to-pair 0) )
        (setq ξto (elt ξfrom-to-pair 1) )
      )

    (setq workOnStringP (if ξfrom-to-pair nil t))
    (setq inputStr (if workOnStringP ξstring (buffer-substring-no-properties ξfrom ξto)))
    (setq outputStr
          (let ((case-fold-search t))
            (replace-regexp-in-string "a\\|e\\|i\\|o\\|u\\|" "" inputStr) )  )

    (if workOnStringP
        outputStr
      (save-excursion
        (delete-region ξfrom ξto)
        (goto-char ξfrom)
        (insert outputStr) )) ) )

(defun what-hexadecimal-value ()
  "Prints the decimal value of a hexadecimal string under cursor.
Samples of valid input:

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

  (let (inputStr tempStr p1 p2 )
    (save-excursion
      (search-backward-regexp "[\\`0-9A-Fa-fx#]" nil t)
      (forward-char)
      (setq p1 (point) )
      (search-forward-regexp "[\\`0-9A-Fa-fx#]" nil t)
      (backward-char)
      (setq p2 (point) ) )

    (setq inputStr (buffer-substring-no-properties p1 p2) )

    (let ((case-fold-search nil) )
      (setq tempStr (replace-regexp-in-string "\\`0x" "" inputStr )) ; C, Perl, …
      (setq tempStr (replace-regexp-in-string "\\`#x" "" tempStr )) ; elisp …
      (setq tempStr (replace-regexp-in-string "\\`#" "" tempStr ))  ; CSS …
      )

    (message "Hex %s is %d" tempStr (string-to-number tempStr 16 ) )
    ))


(defun replace-latin-alphabet-to-gothic (p1 p2 reverse-direction-p)
  "Replace English alphabets to Unicode gothic characters.
For example, A ⇒ 𝔄, a ⇒ 𝔞.

When called interactively, work on current text block or text selection. (a “text block” is text between empty lines)

If any `universal-argument' is given, reverse direction.

When called in elisp, the p1 and p2 are region begin/end positions to work on."
  (interactive
   (let ((bds (get-selection-or-unit 'block)) )
     (list (elt bds 1) (elt bds 2) current-prefix-arg )) )

  (let (
        (latin-to-gothic [ ["A" "𝔄"] ["B" "𝔅"] ["C" "ℭ"] ["D" "𝔇"] ["E" "𝔈"] ["F" "𝔉"] ["G" "𝔊"] ["H" "ℌ"] ["I" "ℑ"] ["J" "𝔍"] ["K" "𝔎"] ["L" "𝔏"] ["M" "𝔐"] ["N" "𝔑"] ["O" "𝔒"] ["P" "𝔓"] ["Q" "𝔔"] ["R" "ℜ"] ["S" "𝔖"] ["T" "𝔗"] ["U" "𝔘"] ["V" "𝔙"] ["W" "𝔚"] ["X" "𝔛"] ["Y" "𝔜"] ["Z" "ℨ"] ["a" "𝔞"] ["b" "𝔟"] ["c" "𝔠"] ["d" "𝔡"] ["e" "𝔢"] ["f" "𝔣"] ["g" "𝔤"] ["h" "𝔥"] ["i" "𝔦"] ["j" "𝔧"] ["k" "𝔨"] ["l" "𝔩"] ["m" "𝔪"] ["n" "𝔫"] ["o" "𝔬"] ["p" "𝔭"] ["q" "𝔮"] ["r" "𝔯"] ["s" "𝔰"] ["t" "𝔱"] ["u" "𝔲"] ["v" "𝔳"] ["w" "𝔴"] ["x" "𝔵"] ["y" "𝔶"] ["z" "𝔷"] ])

        (gothic-to-latin [ ["𝔄" "A"] ["𝔅" "B"] ["ℭ" "C"] ["𝔇" "D"] ["𝔈" "E"] ["𝔉" "F"] ["𝔊" "G"] ["ℌ" "H"] ["ℑ" "I"] ["𝔍" "J"] ["𝔎" "K"] ["𝔏" "L"] ["𝔐" "M"] ["𝔑" "N"] ["𝔒" "O"] ["𝔓" "P"] ["𝔔" "Q"] ["ℜ" "R"] ["𝔖" "S"] ["𝔗" "T"] ["𝔘" "U"] ["𝔙" "V"] ["𝔚" "W"] ["𝔛" "X"] ["𝔜" "Y"] ["ℨ" "Z"] ["𝔞" "a"] ["𝔟" "b"] ["𝔠" "c"] ["𝔡" "d"] ["𝔢" "e"] ["𝔣" "f"] ["𝔤" "g"] ["𝔥" "h"] ["𝔦" "i"] ["𝔧" "j"] ["𝔨" "k"] ["𝔩" "l"] ["𝔪" "m"] ["𝔫" "n"] ["𝔬" "o"] ["𝔭" "p"] ["𝔮" "q"] ["𝔯" "r"] ["𝔰" "s"] ["𝔱" "t"] ["𝔲" "u"] ["𝔳" "v"] ["𝔴" "w"] ["𝔵" "x"] ["𝔶" "y"] ["𝔷" "z"] ])

        useMap
        )

    (if reverse-direction-p
        (progn (setq useMap gothic-to-latin))
      (progn (setq useMap latin-to-gothic))
      )
    (save-excursion
      (let ((case-fold-search nil))
        (replace-pairs-region p1 p2 useMap ) ) ) ) )

(require 'calc-bin)

(defun dec-to-bin (decStr)
"convert the decimal number string decStr into a binary (string)"
  (let ((calc-number-radix 2))
    (math-format-radix (string-to-number decStr))))
