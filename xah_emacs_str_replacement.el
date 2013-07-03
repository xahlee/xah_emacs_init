;; -*- coding: utf-8 -*-
;; some elisp string replacement functions

;; 2007-06
;;   Xah Lee
;; ∑ http://xahlee.org/

(require 'xfrp_find_replace_pairs)
(require 'xeu_elisp_util)

(defun cycle-camel-style-case ()
  "Cyclically replace {camelStyle, camel_style} current word or text selection.
actually, currently just change from camel to underscore. no cycle"
  (interactive)
  ;; this function sets a property 「'state」. Possible values are 0 to length of char_array.
  (let (input_text
        replace_text char_array p1 p2 current_state next_state changeFrom
        changeTo startedWithRegion-p )

    (if (region-active-p)
        (progn
          (setq startedWithRegion-p t )
          (setq p1 (region-beginning))
          (setq p2 (region-end))
          )
      (let ((bds (bounds-of-thing-at-point 'word)))
        (setq startedWithRegion-p nil )
        (setq p1 (car bds))
        (setq p2 (cdr bds)) ) )

    (setq char_array [" " "_"])

    (setq current_state
          (if (get 'cycle-camel-style-case 'state)
              (get 'cycle-camel-style-case 'state)
            0))
    (setq next_state (% (+ current_state 1) (length char_array)))

    (setq changeFrom (elt char_array current_state ))
    (setq changeTo (elt char_array next_state ))

    (setq input_text (buffer-substring-no-properties p1 p2))

    (let ((case-fold-search nil))
      (cond
       ;; camel to underscore
       (
        (equal current_state 0)
        (setq replace_text (replace-regexp-in-string "\\([A-Z]\\)" "_\\1" input_text) )
(setq replace_text (downcase replace_text) )
        )
       ((equal current_state 1)
        (setq replace_text (replace-regexp-in-string "_\\([a-z]\\)" "\\,(upcase \\1)" input_text) )
;; (setq replace_text (downcase replace_text) )
        ) ) )

    (save-restriction
      (narrow-to-region p1 p2)
      (delete-region (point-min) (point-max))
      (insert replace_text)
      )

    (put 'cycle-camel-style-case 'state next_state)
    ) )

(defun cycle-hyphen-underscore-space ()
  "Cyclically replace {underscore, space, hypen} chars on current word or text selection.
When called repeatedly, this command cycles the {“ ”, “_”, “-”} characters."
  (interactive)
  ;; this function sets a property 「'state」. Possible values are 0 to length of charArray.
  (let (inputText bds charArray p1 p2 currentState nextState changeFrom
                 changeTo startedWithRegion-p )
    (if (region-active-p)
        (setq startedWithRegion-p t )
      (setq startedWithRegion-p nil ) )

    (setq bds (get-selection-or-unit 'glyphs))
    (setq inputText (elt bds 0) p1 (elt bds 1) p2 (elt bds 2)  )

    (setq charArray [" " "_" "-"])

    (setq currentState
          (if (get 'cycle-hyphen-underscore-space 'state)
              (get 'cycle-hyphen-underscore-space 'state)
            0))
    (setq nextState (% (+ currentState 1) (length charArray)))

    (setq changeFrom (elt charArray currentState ))
    (setq changeTo (elt charArray nextState ))

    (setq inputText (replace-regexp-in-string changeFrom changeTo (buffer-substring-no-properties p1 p2)) )
    (delete-region p1 p2)
    (insert inputText)

    (when (or (string= changeTo " ") startedWithRegion-p)
      (goto-char p2)
      (set-mark p1)
      (setq deactivate-mark nil) )

    (put 'cycle-hyphen-underscore-space 'state nextState)

    ) )

(defun convert-english-chinese-punctuation (p1 p2 &optional ξ-to-direction)
  "Replace punctuation from/to English/Chinese Unicode symbols.

When called interactively, do current text block (paragraph) or text selection. The conversion direction is automatically determined.

If `universal-argument' is called:

 no C-u → Automatic.
 C-u → to English
 C-u 1 → to English
 C-u 2 → to Chinese

When called in lisp code, p1 p2 are region begin/end positions. ξ-to-direction must be any of the following values: 「\"chinese\"」, 「\"english\"」, 「\"auto\"」.

See also: `remove-punctuation-trailing-redundant-space'."
  (interactive
   (let ( (bds (get-selection-or-unit 'block)))
     (list (elt bds 1) (elt bds 2)
           (cond
            ((equal current-prefix-arg nil) "auto")
            ((equal current-prefix-arg '(4)) "english")
            ((equal current-prefix-arg 1) "english")
            ((equal current-prefix-arg 2) "chinese")
            (t "chinese")
            )
           ) ) )
  (let (
        (inputStr (buffer-substring-no-properties p1 p2))
        (ξ-english-chinese-punctuation-map
         [
          [". " "。"]
          [".\n" "。\n"]
          ["," "，"]
          [": " "："]
          ["; " "；"]
          ["?" "？"] ; no space after
          ["!" "！"]

          ;; for inside HTML
          [".</" "。</"]
          ["?</" "？</"]
          [":</" "：</"]
          ]
         ))

    (replace-pairs-region p1 p2
                              (cond
                               ((string= ξ-to-direction "chinese") ξ-english-chinese-punctuation-map)
                               ((string= ξ-to-direction "english") (mapcar (lambda (ξpair) (vector (elt ξpair 1) (elt ξpair 0))) ξ-english-chinese-punctuation-map))
                               ((string= ξ-to-direction "auto")
                                (if (string-match ",\\|. " inputStr)
                                  ξ-english-chinese-punctuation-map
                                  (mapcar (lambda (ξpair) (vector (elt ξpair 1) (elt ξpair 0))) ξ-english-chinese-punctuation-map)
                                  ))

                               (t (user-error "Your 3rd argument 「%s」 isn't valid" ξ-to-direction)) ) ) ) )

(defun convert-asian/ascii-space (p1 p2)
  "Change all space characters between Asian Ideographic one to ASCII one.
Works on current block or text selection.

When called in emacs lisp code, the p1 p2 are cursor positions for region.

See also `convert-english-chinese-punctuation'
 `remove-punctuation-trailing-redundant-space'
"
  (interactive
   (let ( (bds (get-selection-or-unit 'block)))
     (list (elt bds 1) (elt bds 2) ) ) )
  (let ((ξ-space-char-map
         [
          ["　" " "]
          ]
         ))
    (replace-regexp-pairs-region p1 p2
 (if (string-match "　" (buffer-substring-no-properties p1 p2))
     ξ-space-char-map
   (mapcar (lambda (ξpair) (vector (elt ξpair 1) (elt ξpair 0))) ξ-space-char-map) )
 "FIXEDCASE" "LITERAL")
    )
  )

(defun remove-punctuation-trailing-redundant-space (p1 p2)
  "Remove redundant whitespace after punctuation.
Works on current block or text selection.

When called in emacs lisp code, the p1 p2 are cursor positions for region.

See also `convert-english-chinese-punctuation'."
  (interactive
   (let ( (bds (get-selection-or-unit 'block)))
     (list (elt bds 1) (elt bds 2) ) ) )
  (replace-regexp-pairs-region p1 p2
                               [
                                ;; clean up. Remove extra space.
                                [" +," ","]
                                [",  +" ", "]
                                ["?  +" "? "]
                                ["!  +" "! "]
                                ["\\.  +" ". "]

;; fullwidth punctuations
                                ["， +" "，"]
                                ["。 +" "。"]
                                ["： +" "："]
                                ["？ +" "？"]
                                ["； +" "；"]
                                ["！ +" "！"]
                                ["、 +" "、"]
                                ]
                               "FIXEDCASE" "LITERAL") )

(defun convert-fullwidth-chars (p1 p2 &optional ξ-to-direction)
  "Convert ASCII chars to/from Unicode fullwidth version.

When called interactively, do text selection or text block (paragraph).

The conversion direction is determined like this: if the command has been repeated, then toggle. Else, always do to-Unicode direction.

If `universal-argument' is called:

 no C-u → Automatic.
 C-u → to ASCII
 C-u 1 → to ASCII
 C-u 2 → to Unicode

When called in lisp code, p1 p2 are region begin/end positions. ξ-to-direction must be any of the following values: 「\"unicode\"」, 「\"ascii\"」, 「\"auto\"」.

See also: `remove-punctuation-trailing-redundant-space'."
  (interactive
   (let ( (bds (get-selection-or-unit 'block)))
     (list (elt bds 1) (elt bds 2)
           (cond
            ((equal current-prefix-arg nil) "auto")
            ((equal current-prefix-arg '(4)) "ascii")
            ((equal current-prefix-arg 1) "ascii")
            ((equal current-prefix-arg 2) "unicode")
            (t "unicode")
            )
           ) ) )
  (let* (
        (ξ-ascii-unicode-map
         [
 ["0" "０"] ["1" "１"] ["2" "２"] ["3" "３"] ["4" "４"] ["5" "５"] ["6" "６"] ["7" "７"] ["8" "８"] ["9" "９"]
 ["A" "Ａ"] ["B" "Ｂ"] ["C" "Ｃ"] ["D" "Ｄ"] ["E" "Ｅ"] ["F" "Ｆ"] ["G" "Ｇ"] ["H" "Ｈ"] ["I" "Ｉ"] ["J" "Ｊ"] ["K" "Ｋ"] ["L" "Ｌ"] ["M" "Ｍ"] ["N" "Ｎ"] ["O" "Ｏ"] ["P" "Ｐ"] ["Q" "Ｑ"] ["R" "Ｒ"] ["S" "Ｓ"] ["T" "Ｔ"] ["U" "Ｕ"] ["V" "Ｖ"] ["W" "Ｗ"] ["X" "Ｘ"] ["Y" "Ｙ"] ["Z" "Ｚ"]
 ["a" "ａ"] ["b" "ｂ"] ["c" "ｃ"] ["d" "ｄ"] ["e" "ｅ"] ["f" "ｆ"] ["g" "ｇ"] ["h" "ｈ"] ["i" "ｉ"] ["j" "ｊ"] ["k" "ｋ"] ["l" "ｌ"] ["m" "ｍ"] ["n" "ｎ"] ["o" "ｏ"] ["p" "ｐ"] ["q" "ｑ"] ["r" "ｒ"] ["s" "ｓ"] ["t" "ｔ"] ["u" "ｕ"] ["v" "ｖ"] ["w" "ｗ"] ["x" "ｘ"] ["y" "ｙ"] ["z" "ｚ"]
 ["," "，"] ["." "．"] [":" "："] [";" "；"] ["!" "！"] ["?" "？"] ["\"" "＂"] ["'" "＇"] ["`" "｀"] ["^" "＾"] ["~" "～"] ["¯" "￣"] ["_" "＿"]
 ["&" "＆"] ["@" "＠"] ["#" "＃"] ["%" "％"] ["+" "＋"] ["-" "－"] ["*" "＊"] ["=" "＝"] ["<" "＜"] [">" "＞"] ["(" "（"] [")" "）"] ["[" "［"] ["]" "］"] ["{" "｛"] ["}" "｝"] ["(" "｟"] [")" "｠"] ["|" "｜"] ["¦" "￤"] ["/" "／"] ["\\" "＼"] ["¬" "￢"] ["$" "＄"] ["£" "￡"] ["¢" "￠"] ["₩" "￦"] ["¥" "￥"]
          ]
         )
        (ξ-reverse-map (mapcar (lambda (ξpair) (vector (elt ξpair 1) (elt ξpair 0))) ξ-ascii-unicode-map))

        (cmdStates ["to-unicode" "to-ascii"])
        (stateBefore (if (get 'convert-fullwidth-chars 'state) (get 'convert-fullwidth-chars 'state) 0))
        (stateAfter (% (+ stateBefore (length cmdStates) 1) (length cmdStates)))
)

;"０\\|１\\|２\\|３\\|４\\|５\\|６\\|７\\|８\\|９\\|Ａ\\|Ｂ\\|Ｃ\\|Ｄ\\|Ｅ\\|Ｆ\\|Ｇ\\|Ｈ\\|Ｉ\\|Ｊ\\|Ｋ\\|Ｌ\\|Ｍ\\|Ｎ\\|Ｏ\\|Ｐ\\|Ｑ\\|Ｒ\\|Ｓ\\|Ｔ\\|Ｕ\\|Ｖ\\|Ｗ\\|Ｘ\\|Ｙ\\|Ｚ\\|ａ\\|ｂ\\|ｃ\\|ｄ\\|ｅ\\|ｆ\\|ｇ\\|ｈ\\|ｉ\\|ｊ\\|ｋ\\|ｌ\\|ｍ\\|ｎ\\|ｏ\\|ｐ\\|ｑ\\|ｒ\\|ｓ\\|ｔ\\|ｕ\\|ｖ\\|ｗ\\|ｘ\\|ｙ\\|ｚ"

;(message "before %s" stateBefore)
;(message "after %s" stateAfter)
;(message "ξ-to-direction %s" ξ-to-direction)
;(message "real-this-command  %s" this-command)
;(message "real-last-command %s" last-command)

(let ((case-fold-search nil))
 (replace-pairs-region
 p1 p2
 (cond
  ((string= ξ-to-direction "unicode") ξ-ascii-unicode-map)
  ((string= ξ-to-direction "ascii") ξ-reverse-map)
  ((string= ξ-to-direction "auto")
   (if (equal this-command last-command)
       (if (eq stateBefore 0)
           ξ-ascii-unicode-map
         ξ-reverse-map
         )
     ξ-ascii-unicode-map
     ))
      (t (user-error "Your 3rd argument 「%s」 isn't valid" ξ-to-direction)) ) )
)
(put 'convert-fullwidth-chars 'state stateAfter)
 ) )

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

(defvar xah-bracketsList nil "a list of bracket pairs. ⁖ () {} [] “” ‘’ ‹› «» 「」 『』 ….")
(setq xah-bracketsList '( "()" "{}" "[]" "<>" "“”" "‘’" "‹›" "«»" "「」" "『』" "【】" "〖〗" "〈〉" "《》" "〔〕" "⦅⦆" "〚〛" "⦃⦄"
"〈〉" "⦑⦒" "⧼⧽" 
"⟦⟧" "⟨⟩" "⟪⟫" "⟮⟯" "⟬⟭" "❛❜" "❝❞" "❨❩" "❪❫" "❴❵" "❬❭" "❮❯" "❰❱"
))

(defun remove-quotes-or-brackets (bracketType)
  "Remove quotes/brackets
Works on current block or text selection.
"
  (interactive
   (list (ido-completing-read "from:" xah-bracketsList) ) )
  (let* (
         (bds (get-selection-or-unit 'block))
         (p1 (elt bds 1))
         (p2 (elt bds 2))
         )
    (replace-regexp-pairs-region p1 p2
                                 (vector 
                                  (vector (substring bracketType 0 1) "")
                                  (vector (substring bracketType 1 2) "")
                                  )
                                 "FIXEDCASE" "LITERAL")
    ) )

(defun change-bracket-pairs (fromType toType)
  "Change bracket pairs from one type to another on text selection or text block.
For example, change all parenthesis () to square brackets [].

When called in lisp program, fromType and toType is a string of a bracket pair. ⁖ \"()\", likewise for toType."
  (interactive
   (let ( )
     (list
      (ido-completing-read "Replace this:" xah-bracketsList )
      (ido-completing-read "To:" xah-bracketsList ) ) ) )
  (let* (
         (bds (get-selection-or-unit 'block))
         (p1 (elt bds 1))
         (p2 (elt bds 2))
         (changePairs (vector
                 (vector (char-to-string (elt fromType 0)) (char-to-string (elt toType 0)))
                 (vector (char-to-string (elt fromType 1)) (char-to-string (elt toType 1)))
                 ))
         )
    (replace-pairs-region p1 p2 changePairs) ) )


(defun ξ-replace-curly-apostrophe ()
  "Replace some single curly quotes to straight version,
in text selection or text block.
Example: 「it’s」 ⇒ 「it's」."
  (interactive "r")
(let (bds p1 p2)
    (setq bds (get-selection-or-unit 'block))
    (setq p1 (elt bds 1) p2 (elt bds 2)  )
    (replace-pairs-region p1 p2 '(
["‘tis" "'tis"]
["’s" "'s"]
["’d" "'d"]
["n’t" "n't"]
["’ve" "'ve"]
["’ll" "'ll"]
["’m" "'m"]
["’re" "'re"]
["s’ " "s' "]))
    )

)

(defun ξ-replace-straight-quotes (p1 p2)
  "Replace straight double quotes to curly ones, and others.
Works on current text selection, else the current text block between empty lines.

Examples of changes:
 「\"…\"」 ⇒ 「“…”」
 「...」 ⇒ 「…」
 「I’m」 => 「I'm」
 「--」 ⇒ 「—」
 「~=」 ⇒ 「≈」

 In lisp program, the arguments P1 and P2 are region boundaries.
"
;; some examples for debug
;; do "‘em all -- done..."
;; I’am not
;; said "can’t have it, can’t, just can’t"
;; ‘I’ve can’t’

  (interactive
   (let ( (bds (get-selection-or-unit 'block)))
     (list (elt bds 1) (elt bds 2) ) ) )
  (let ( )
    ;; Note: order is important since this is huristic.

    (save-restriction
      (narrow-to-region p1 p2)

      ;; dash and ellipsis etc
      (replace-pairs-region (point-min) (point-max)
                            [
                             ["--" " — "]
                             ["—" " — "]
                             ["..." "…"]
                             [" :)" " ☺"]
                             [" :(" " ☹"]
                             [";)" "😉"]
                             ["e.g." "⁖"]
                             ["~=" "≈"]
                             ])

      (replace-pairs-region (point-min) (point-max)
                            [
                             ["  —  " " — "]                        ; rid of extra space in em-dash
                             [" , " ", "]
                             ])

      ;; fix GNU style ASCII quotes
      (replace-pairs-region (point-min) (point-max)
                            [
                             ["``" "“"]
                             ["''" "”"]
                             ])

      ;; fix straight double quotes
      (replace-pairs-region (point-min) (point-max)
                            [
                             [">\"" ">“"]
                             ["(\"" "(“"]
                             [" \"" " “"]
                             ["\" " "” "]
                             ["\"," "”,"]
                             ["\"." "”."]
                             ["\"?" "”?"]
                             ["\";" "”;"]
                             ["\":" "”:"]
                             ["\")" "”)"]
                             ["\"]" "”]"]
                             [".\"" ".”"]
                             [",\"" ",”"]
                             ["!\"" "!”"]
                             ["?\"" "?”"]
                             ["\"<" "”<"]
                             ;; ";
                             ["\"\n" "”\n"]
                             ])

      ;; fix straight double quotes by regex
      (replace-regexp-pairs-region (point-min) (point-max)
                                   [
                                    ["\\`\"" "“"]
                                    ])

      ;; fix single quotes to curly
      (replace-pairs-region (point-min) (point-max)
                            [
                             [">\'" ">‘"]
                             [" \'" " ‘"]
                             ["\' " "’ "]
                             ["\'," "’,"]
                             [".\'" ".’"]
                             ["!\'" "!’"]
                             ["?\'" "?’"]
                             ["(\'" "(‘"]
                             ["\')" "’)"]
                             ["\']" "’]"]
                             ])

      ;; fix apostrophe
      (replace-regexp-pairs-region (point-min) (point-max)
                                   [
                                    ["\\bcan’t\\b" "can't"]
                                    ["\\bdon’t\\b" "don't"]
                                    ["\\bdoesn’t\\b" "doesn't"]
                                    ["\\bain’t\\b" "ain't"]
                                    ["\\bdidn’t\\b" "didn't"]
                                    ["\\baren’t\\b" "aren't"]
                                    ["\\bwasn’t\\b" "wasn't"]
                                    ["\\bweren’t\\b" "weren't"]
                                    ["\\bcouldn’t\\b" "couldn't"]
                                    ["\\bshouldn’t\\b" "shouldn't"]

                                    ["\\b’ve\\b" "'ve"]
                                    ["\\b’re\\b" "'re"]
                                    ["\\b‘em\\b" "'em"]
                                    ["\\b’ll\\b" "'ll"]
                                    ["\\b’m\\b" "'m"]
                                    ["\\b’d\\b" "'d"]
                                    ["\\b’s\\b" "'s"]
                                    ["s’ " "s' "]
                                    ["s’\n" "s'\n"]

                                    ["\"$" "”"]
                                    ])

      ;; fix back. quotes in HTML code
      (replace-regexp-pairs-region (point-min) (point-max)
                                   [
                                    ["” \\([-a-z]+\\)="       "\" \\1="]   ; any 「” some-thing=」
                                    ["=\”" "=\""]
                                    ["/” " "/\" "]
                                    ["\"\\([0-9]+\\)” "     "\"\\1\" "]
                                    ]
                                   )

      (remove-punctuation-trailing-redundant-space (point-min) (point-max) )

      ) ))

(defun escape-quotes ()
  "Replace 「\"」 by 「\\\"」 in current line or text selection."
  (interactive)
  (let* ((bds (get-selection-or-unit 'line))
         (p1 (elt bds 1)) 
         (p2 (elt bds 2)))
    (replace-pairs-region p1 p2 '(["\"" "\\\""])) ) )

(defun unescape-quotes ()
  "Replace  「\\\"」 by 「\"」 in current line or text selection."
  (interactive)
  (let* ((bds (get-selection-or-unit 'line))
        (p1 (elt bds 1)) 
        (p2 (elt bds 2)))
    (replace-pairs-region p1 p2 '(["\\\"" "\""])) ) )

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



(defun replace-mathematica-symbols-region (p1 p2)
  "Replace Mathematica's special char encoding to Unicode of the same semantics.
For example:
 \\=\\[Infinity] ⇒ ∞
 \\=\\[Equal] ⇒ =="
  (interactive "r")
  (replace-pairs-region p1 p2 '(
 ["\\[Infinity]" "∞"]
 ["\\[Equal]" "=="])))

(defun replace-greek-region (p1 p2)
  "Replace math symbols. e.g. alpha to α."
  (interactive "r")
(replace-pairs-region p1 p2 '(
["alpha" "α"]
["beta" "β"]
["gamma" "γ"]
["theta" "θ"]
["lambda" "λ"]
["delta" "δ"]
["epsilon" "ε"]
["omega" "ω"]
["Pi" "π"])))

(defun ξ-replace-tex-region (p1 p2)
  "Replace some math function names or symbols by their LaTeX markup."
  (interactive "r")
(replace-pairs-region p1 p2 '(
["*" "\\ "]
["cos(" "\\cos("]
["sin(" "\\sin("]
["tan(" "\\tan("]
[" pi" "\\!\\pi"]
["R^2" "\\mathbb{R}^2"]
["R^3" "\\mathbb{R}^3"])))

(defun mathematica-to-lsl-region (p1 p2)
  "Change Mathematica syntax to LSL syntax on region.

LSL is Linden Scripting Language.
This command does simple string replacement only."
  (interactive "r")
(replace-pairs-region p1 p2 '(
["Cos[" "llCos("]
["Sin[" "llSin("]
["Tan[" "llTan("]
["Pi" "PI"]
["π" "PI"]
["{" "<"]
["}" ">"])))

(defun clean-mgs-buffer ()
  "Reduce size of a mgs file by removing whitespace and truncating numbers.
This function does several find and replace on the current buffer.
Removing spaces, removing new lines, truncate numbers to 3 decimals, etc.
The goal of these replacement is to reduce the file size of a Mathematica Graphics file (.mgs) that are read over the net by JavaView."
  (interactive)
    (goto-char 1)
    (while (search-forward "\n" nil t) (replace-match "" nil t))
    (goto-char 1)
    (while (search-forward-regexp "  +" nil t) (replace-match " " nil t))
    (goto-char 1)
    (while (search-forward ", " nil t) (replace-match "," nil t))
    (goto-char 1)
    (while (search-forward-regexp "\\([0-9]\\)\\.\\([0-9][0-9][0-9]\\)[0-9]+" nil t) (replace-match "\\1.\\2" t nil)))

(defun compact-region (p1 p2)
  "Replace any sequence of whitespace chars to a single space on region.
Whitespace here is considered any of \\n, tab, space ."
  (interactive "r")
  (replace-regexp-pairs-region p1 p2
                               '( ["[\n\t]+" " "]
                                  ["  +" " "])
                               t))

(defun format-c-lang-region (p1 p2)
  "Expand region of c style syntax languages so that it is nicely formated.
Experimental code.
WARNING: If region has comment or string, the code'd be fucked up."
  (interactive "r")

  (save-excursion
    (save-restriction
      (narrow-to-region p1 p2)
      (replace-regexp-pairs-region p1 p2
                                   '(
                                     ["{" "{\n"]
                                     [";" ";\n"]
                                     ["}" "}\n"]
                                     [";[\t\n]*}" "; }"]
                                     )
                                   t)
      (indent-region p1 p2)
      )
    )
  )

(defun xah-clean-whitespace ()
  "Delete trailing whitespace, and replace sequence of newlines into just 2.
Work on text selection or whole buffer."
  (interactive)
  (let* (
         (bds (get-selection-or-unit 'buffer))
         (p1 (elt bds 1))
         (p2 (elt bds 2))
         )
    (save-excursion
      (save-restriction
        (narrow-to-region p1 p2)
        (progn
          (goto-char (point-min))
          (while (search-forward-regexp "[ \t]+\n" nil "noerror")
            (replace-match "\n") ))
        (progn
          (goto-char (point-min))
          (while (search-forward-regexp "\n\n\n+" nil "noerror")
            (replace-match "\n\n") )) )) ))

(defun xah-clean-whitespace-backup ()
  "make a backup then call `xah-clean-whitespace-backup'"
  (interactive)
  (progn
    (make-backup)
    (xah-clean-whitespace)
    ))

