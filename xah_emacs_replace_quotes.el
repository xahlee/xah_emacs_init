;; -*- coding: utf-8; lexical-binding: t; -*-
;; 2010-09-03

;; 2015-08-22 removed
;; xah-fly-keys-replace-commands.el xah-fly-keys-misc-commands2.el

;; 2015-08-22
;; removed dependencies to xeu_elisp_util.el xah-get-thing.el xah-find.el xah-replace-pairs.el

(require 'xah-replace-pairs)

;; HHH___________________________________________________________________

(defun xah-replace-latex-to-unicode (@begin @end)
  "Replace TeX markup to Unicode in current line or selection.
Example: \\alpha becomes α.
Version 2015-04-28"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (xah-replace-pairs-region
   @begin
   @end
   '(
     ["\\rightarrow" "→"]
     ["\\Sigma" "Σ"]
     ["\\times" "×"]
     ["\\alpha" "α"]
     ["\\beta" "β"]
     ["\\gamma" "γ"]
     ["\\delta" "δ"]
     ["\\Lambda" "Λ"]
     ["\\epsilon" "ε"]
     ["\\omega" "ω"]
     ["\\cup" "∪"]
     ["\\in" "∈"]
     ) "REPORT" "HILIGHT" ))

(defun xah-replace-text-to-latex-region (@begin @end)
  "Replace math function names or symbols by their LaTeX markup.
Work on current line or selection.
Version 2015-04-28"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (xah-replace-pairs-region
   @begin
   @end
   '(
     ["*" "\\ "]
     ["cos(" "\\cos("]
     ["sin(" "\\sin("]
     ["tan(" "\\tan("]
     [" pi" "\\!\\pi"]
     ["R^2" "\\mathbb{R}^2"]
     ["R^3" "\\mathbb{R}^3"]
     ) "REPORT" "HILIGHT" ))

(defun xah-replace-mathematica-symbols (@begin @end)
  "Replace Mathematica's special char markup to Unicode in current line or selection.
For example:
 \\=\\[Infinity] ⇒ ∞
 \\=\\[Equal] ⇒ ==
Version 2015-04-28"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (xah-replace-pairs-region
   @begin
   @end
   '(
     ["\\[Infinity]" "∞"]
     ["\\[Equal]" "=="]) "REPORT" "HILIGHT" ))

(defun xah-replace-greek-letter-name-to-symbol (@begin @end)
  "Replace alpha to α, beta to β etc in current line or selection.

URL `http://ergoemacs.org/emacs/elisp_replace_greeks_to_symbols.html'
Version 2016-10-05"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let ((case-fold-search nil))
    (xah-replace-pairs-region
     @begin
     @end
     '(
       ["Alpha" "Α"]
       ["Beta" "Β"]
       ["Gamma" "Γ"]
       ["Delta" "Δ"]
       ["Epsilon" "Ε"]
       ["Zeta" "Ζ"]
       ["Eta" "Η"]
       ["Theta" "Θ"]
       ["Iota" "Ι"]
       ["Kappa" "Κ"]
       ["Lambda" "Λ"]
       ["Mu" "Μ"]
       ["Nu" "Ν"]
       ["Xi" "Ξ"]
       ["Omicron" "Ο"]
       ["Pi" "Π"]
       ["Rho" "Ρ"]
       ["Sigma" "Σ"]
       ["Tau" "Τ"]
       ["Upsilon" "Υ"]
       ["Phi" "Φ"]
       ["Chi" "Χ"]
       ["Psi" "Ψ"]
       ["Omega" "Ω"]

       ["alpha" "α"]
       ["beta" "β"]
       ["gamma" "γ"]
       ["delta" "δ"]
       ["epsilon" "ε"]
       ["zeta" "ζ"]
       ["eta" "η"]
       ["theta" "θ"]
       ["iota" "ι"]
       ["kappa" "κ"]
       ["lambda" "λ"]
       ["mu" "μ"]
       ["nu" "ν"]
       ["xi" "ξ"]
       ["omicron" "ο"]
       ["pi" "π"]
       ["rho" "ρ"]
       ["sigmaf" "ς"]
       ["sigma" "σ"]
       ["tau" "τ"]
       ["upsilon" "υ"]
       ["phi" "φ"]
       ["chi" "χ"]
       ["psi" "ψ"]
       ["omega" "ω"]
       ["thetasym" "ϑ"]
       ["upsih" "ϒ"]
       ["piv" "ϖ"]
       ) "REPORT" "HILIGHT" )))

(defun xah-replace-mathematica-to-lsl (@begin @end)
  "Change Mathematica syntax to LSL syntax on region.

LSL is Linden Scripting Language.
Version 2015-04-28"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (xah-replace-pairs-region
   @begin
   @end
   '(
     ["Cos[" "llCos("]
     ["Sin[" "llSin("]
     ["Tan[" "llTan("]
     ["Pi" "PI"]
     ["π" "PI"]
     ["{" "<"]
     ["}" ">"]) "REPORT" "HILIGHT" ))

(defun xah-clean-Mathematica-graphics-buffer ()
  "Remove whitespace, truncate numbers, of current buffer of Mathematica graphics file.
This command does several find/replace on the current buffer.
Removing spaces, removing new lines, truncate numbers to 3 decimals, etc.
The goal of these replacement is to reduce the file size of a Mathematica Graphics file (.mgs) that are read over the net by JavaView.
Version 2015-04-28"
  (interactive)

  (goto-char 1)
  (while (search-forward "\n" nil t) (replace-match "" nil t))

  (goto-char 1)
  (while (re-search-forward "  +" nil t) (replace-match " " nil t))

  (goto-char 1)
  (while (search-forward ", " nil t) (replace-match "," nil t))

  (goto-char 1)
  (while (re-search-forward "\\([0-9]\\)\\.\\([0-9][0-9][0-9]\\)[0-9]+" nil t) (replace-match "\\1.\\2" t nil)))

(defun xah-convert-english-chinese-punctuation (@begin @end &optional @to-direction)
  "Convert punctuation from/to English/Chinese characters.

When called interactively, do current line or selection. The conversion direction is automatically determined.

If `universal-argument' is called, ask user for change direction.

When called in lisp code, @begin @end are region begin/end positions. @to-direction must be any of the following values: 「\"chinese\"」, 「\"english\"」, 「\"auto\"」.

See also: `xah-remove-punctuation-trailing-redundant-space'.

URL `http://ergoemacs.org/emacs/elisp_convert_chinese_punctuation.html'
Version 2015-10-05"
  (interactive
   (let ($p1 $p2)
     (if (use-region-p)
         (setq $p1 (region-beginning) $p2 (region-end))
       (setq $p1 (line-beginning-position) $p2 (line-end-position)))
     (list
      $p1
      $p2
      (if current-prefix-arg
          (ido-completing-read
           "Change to: "
           '( "english"  "chinese")
           "PREDICATE"
           "REQUIRE-MATCH")
        "auto"
        ))))
  (let (
        ($input-str (buffer-substring-no-properties @begin @end))
        ($replacePairs
         [
          [". " "。"]
          [".\n" "。\n"]
          [", " "，"]
          [",\n" "，\n"]
          [": " "："]
          ["; " "；"]
          ["? " "？"] ; no space after
          ["! " "！"]

          ;; for inside HTML
          [".</" "。</"]
          ["?</" "？</"]
          [":</" "：</"]
          [" " "　"]
          ]
         ))

    (when (string-equal @to-direction "auto")
      (setq
       @to-direction
       (if
           (or
            (string-match "　" $input-str)
            (string-match "。" $input-str)
            (string-match "，" $input-str)
            (string-match "？" $input-str)
            (string-match "！" $input-str))
           "english"
         "chinese")))
    (save-excursion
      (save-restriction
        (narrow-to-region @begin @end)
        (mapc
         (lambda ($x)
           (progn
             (goto-char (point-min))
             (while (search-forward (aref $x 0) nil t)
               (replace-match (aref $x 1)))))
         (cond
          ((string-equal @to-direction "chinese") $replacePairs)
          ((string-equal @to-direction "english") (mapcar (lambda (x) (vector (elt x 1) (elt x 0))) $replacePairs))
          (t (user-error "Your 3rd argument 「%s」 isn't valid" @to-direction))))))))

(defun xah-convert-chinese-numeral (@begin @end &optional @to-chinese)
  "Replace convert Chinese numeral to Arabic numeral, or reverse.
On current line or selection.
If `universal-argument' is called first, do reverse direction.
Version 2015-04-29"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (let* (($numMap [["○" "0"] ["一" "1"] ["二" "2"] ["三" "3"] ["四" "4"] ["五" "5"] ["六" "6"] ["七" "7"] ["八" "8"] ["九" "9"] ]))
    (xah-replace-pairs-region
     @begin @end
     (if @to-chinese (mapcar (lambda (x) (vector (elt x 1) (elt x 0))) $numMap) $numMap )
     t t
     )))

(defun xah-remove-vowel ()
  "Remove the following letters: {a e i o u} in current line or text selection.
Version 2017-01-11"
  (interactive)
  (let ($p1 $p2 )
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (setq $p1 (line-beginning-position) $p2 (line-end-position)))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (let ( (case-fold-search nil))
          (goto-char (point-min))
          (while (re-search-forward "a\\|e\\|i\\|o\\|u" (point-max) t)
            (replace-match "" "FIXEDCASE" "LITERAL")))))))

(defun xah-replace-profanity ()
  "Replace swearing words by replacing some chars.
Works in current line or text selection.
Version 2020-04-10"
  (interactive)
  (let ( $p1 $p2)
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (setq $p1 (line-beginning-position) $p2 (line-end-position)))
    (xah-replace-pairs-region
     $p1 $p2
     '(
       ["fuck" "f�ck"]
       ["shit" "sh�t"]
       ["ass" "�ss"]
       ["motherfuck" "m�th�rf�ck"]
       ["pussy" "p�ssy"]
       ) "REPORT" "HILIGHT" )))

(defun xah-replace-curly-apostrophe ()
  "Replace some single curly apostrophe to straight version.
Works on current line or text selection.
Example: 「it’s」 ⇒ 「it's」."
  (interactive)
  (let ( $p1 $p2)
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (setq $p1 (line-beginning-position) $p2 (line-end-position)))
    (xah-replace-pairs-region
     $p1 $p2
     '(
       ["‘tis" "'tis"]
       ["’s" "'s"]
       ["’d" "'d"]
       ["n’t" "n't"]
       ["’ve" "'ve"]
       ["’ll" "'ll"]
       ["’m" "'m"]
       ["’re" "'re"]
       ["s’ " "s' "]) "REPORT" "HILIGHT" )))

(defun xah-convert-fullwidth-chars (@begin @end &optional @to-direction)
  "Convert ASCII chars to/from Unicode fullwidth version.
Works on current line or text selection.

The conversion direction is determined like this: if the command has been repeated, then toggle. Else, always do to-Unicode direction.

If `universal-argument' is called first:

 no C-u → Automatic.
 C-u → to ASCII
 C-u 1 → to ASCII
 C-u 2 → to Unicode

When called in lisp code, @begin @end are region begin/end positions. @to-direction must be any of the following values: 「\"unicode\"」, 「\"ascii\"」, 「\"auto\"」.

See also: `xah-remove-punctuation-trailing-redundant-space'.

URL `http://ergoemacs.org/emacs/elisp_convert_chinese_punctuation.html'
Version 2018-08-02"
  (interactive
   (let ($p1 $p2)
     (if (use-region-p)
         (setq $p1 (region-beginning) $p2 (region-end))
       (setq $p1 (line-beginning-position) $p2 (line-end-position)))
     (list $p1 $p2
           (cond
            ((equal current-prefix-arg nil) "auto")
            ((equal current-prefix-arg '(4)) "ascii")
            ((equal current-prefix-arg 1) "ascii")
            ((equal current-prefix-arg 2) "unicode")
            (t "unicode")))))
  (let* (
         ($ascii-unicode-map
          [
           ["0" "０"] ["1" "１"] ["2" "２"] ["3" "３"] ["4" "４"] ["5" "５"] ["6" "６"] ["7" "７"] ["8" "８"] ["9" "９"]
           ["A" "Ａ"] ["B" "Ｂ"] ["C" "Ｃ"] ["D" "Ｄ"] ["E" "Ｅ"] ["F" "Ｆ"] ["G" "Ｇ"] ["H" "Ｈ"] ["I" "Ｉ"] ["J" "Ｊ"] ["K" "Ｋ"] ["L" "Ｌ"] ["M" "Ｍ"] ["N" "Ｎ"] ["O" "Ｏ"] ["P" "Ｐ"] ["Q" "Ｑ"] ["R" "Ｒ"] ["S" "Ｓ"] ["T" "Ｔ"] ["U" "Ｕ"] ["V" "Ｖ"] ["W" "Ｗ"] ["X" "Ｘ"] ["Y" "Ｙ"] ["Z" "Ｚ"]
           ["a" "ａ"] ["b" "ｂ"] ["c" "ｃ"] ["d" "ｄ"] ["e" "ｅ"] ["f" "ｆ"] ["g" "ｇ"] ["h" "ｈ"] ["i" "ｉ"] ["j" "ｊ"] ["k" "ｋ"] ["l" "ｌ"] ["m" "ｍ"] ["n" "ｎ"] ["o" "ｏ"] ["p" "ｐ"] ["q" "ｑ"] ["r" "ｒ"] ["s" "ｓ"] ["t" "ｔ"] ["u" "ｕ"] ["v" "ｖ"] ["w" "ｗ"] ["x" "ｘ"] ["y" "ｙ"] ["z" "ｚ"]
           ["," "，"] ["." "．"] [":" "："] [";" "；"] ["!" "！"] ["?" "？"] ["\"" "＂"] ["'" "＇"] ["`" "｀"] ["^" "＾"] ["~" "～"] ["¯" "￣"] ["_" "＿"]
           [" " "　"]
           ["&" "＆"] ["@" "＠"] ["#" "＃"] ["%" "％"] ["+" "＋"] ["-" "－"] ["*" "＊"] ["=" "＝"] ["<" "＜"] [">" "＞"] ["(" "（"] [")" "）"] ["[" "［"] ["]" "］"] ["{" "｛"] ["}" "｝"] ["(" "｟"] [")" "｠"] ["|" "｜"] ["¦" "￤"] ["/" "／"] ["\\" "＼"] ["¬" "￢"] ["$" "＄"] ["£" "￡"] ["¢" "￠"] ["₩" "￦"] ["¥" "￥"]
           ]
          )
         ($reverse-map
          (mapcar
           (lambda (x) (vector (elt x 1) (elt x 0)))
           $ascii-unicode-map))

         ($stateBefore
          (if (get 'xah-convert-fullwidth-chars 'state)
              (get 'xah-convert-fullwidth-chars 'state)
            (progn
              (put 'xah-convert-fullwidth-chars 'state 0)
              0
              )))
         ($stateAfter (if (eq $stateBefore 0) 1 0 )))

  ;"０\\|１\\|２\\|３\\|４\\|５\\|６\\|７\\|８\\|９\\|Ａ\\|Ｂ\\|Ｃ\\|Ｄ\\|Ｅ\\|Ｆ\\|Ｇ\\|Ｈ\\|Ｉ\\|Ｊ\\|Ｋ\\|Ｌ\\|Ｍ\\|Ｎ\\|Ｏ\\|Ｐ\\|Ｑ\\|Ｒ\\|Ｓ\\|Ｔ\\|Ｕ\\|Ｖ\\|Ｗ\\|Ｘ\\|Ｙ\\|Ｚ\\|ａ\\|ｂ\\|ｃ\\|ｄ\\|ｅ\\|ｆ\\|ｇ\\|ｈ\\|ｉ\\|ｊ\\|ｋ\\|ｌ\\|ｍ\\|ｎ\\|ｏ\\|ｐ\\|ｑ\\|ｒ\\|ｓ\\|ｔ\\|ｕ\\|ｖ\\|ｗ\\|ｘ\\|ｙ\\|ｚ"

    ;; (message "before %s" $stateBefore)
    ;; (message "after %s" $stateAfter)
    ;; (message "@to-direction %s" @to-direction)
    ;; (message "real-this-command  %s" real-this-command)
    ;; (message "real-last-command %s" real-last-command)
    ;; (message "this-command  %s" this-command)
    ;; (message "last-command %s" last-command)

    (let ((case-fold-search nil))
      (xah-replace-pairs-region
       @begin @end
       (cond
        ((string-equal @to-direction "unicode") $ascii-unicode-map)
        ((string-equal @to-direction "ascii") $reverse-map)
        ((string-equal @to-direction "auto")
         (if (eq $stateBefore 0)
             $reverse-map
           $ascii-unicode-map )

         ;; 2018-08-02 this doesn't work when using smex
         ;; (if (eq last-command this-command)
         ;;     (progn
         ;;       (message "%s" "repeated")
         ;;       (if (eq $stateBefore 0)
         ;;           $reverse-map
         ;;         $ascii-unicode-map ))
         ;;   (progn
         ;;     (message "%s" "not repeated")
         ;;     $ascii-unicode-map))

         ;;

         )
        (t (user-error "Your 3rd argument 「%s」 isn't valid" @to-direction)))
       t t ))
    (put 'xah-convert-fullwidth-chars 'state $stateAfter)))

(defun xah-remove-punctuation-trailing-redundant-space (@begin @end)
  "Remove redundant whitespace after punctuation.
Works on current line or text selection.

When called in emacs lisp code, the @begin @end are cursor positions for region.

See also `xah-convert-english-chinese-punctuation'.

URL `http://ergoemacs.org/emacs/elisp_convert_chinese_punctuation.html'
version 2015-08-22"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (require 'xah-replace-pairs)
  (xah-replace-regexp-pairs-region
   @begin @end
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
   "FIXEDCASE" "LITERAL"))

(defun xah-convert-asian/ascii-space (@begin @end)
  "Change all space characters between Asian Ideographic one to ASCII one.
Works on current line or text selection.

When called in emacs lisp code, the @begin @end are cursor positions for region.

See also `xah-convert-english-chinese-punctuation'
 `xah-remove-punctuation-trailing-redundant-space'
"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let (($space-char-map
         [
          ["　" " "]
          ]
         ))
    (xah-replace-regexp-pairs-region
     @begin @end
     (if (string-match "　" (buffer-substring-no-properties @begin @end))
         $space-char-map
       (mapcar (lambda (x) (vector (elt x 1) (elt x 0))) $space-char-map))
     "FIXEDCASE" "LITERAL")))

(defun xah-convert-latin-alphabet-gothic (@begin @end @reverse-direction-p)
  "Replace English alphabets to Unicode gothic characters.
For example, A → 𝔄, a → 𝔞.

When called interactively, work on current line or text selection.

If `universal-argument' is called first, reverse direction.

When called in elisp, the @begin and @end are region begin/end positions to work on.

URL `http://ergoemacs.org/misc/thou_shalt_use_emacs_lisp.html'
Version 2019-03-07"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end) current-prefix-arg )
     (list (line-beginning-position) (line-end-position) current-prefix-arg )))
  (let (
        ($latin-to-gothic [ ["A" "𝔄"] ["B" "𝔅"] ["C" "ℭ"] ["D" "𝔇"] ["E" "𝔈"] ["F" "𝔉"] ["G" "𝔊"] ["H" "ℌ"] ["I" "ℑ"] ["J" "𝔍"] ["K" "𝔎"] ["L" "𝔏"] ["M" "𝔐"] ["N" "𝔑"] ["O" "𝔒"] ["P" "𝔓"] ["Q" "𝔔"] ["R" "ℜ"] ["S" "𝔖"] ["T" "𝔗"] ["U" "𝔘"] ["V" "𝔙"] ["W" "𝔚"] ["X" "𝔛"] ["Y" "𝔜"] ["Z" "ℨ"] ["a" "𝔞"] ["b" "𝔟"] ["c" "𝔠"] ["d" "𝔡"] ["e" "𝔢"] ["f" "𝔣"] ["g" "𝔤"] ["h" "𝔥"] ["i" "𝔦"] ["j" "𝔧"] ["k" "𝔨"] ["l" "𝔩"] ["m" "𝔪"] ["n" "𝔫"] ["o" "𝔬"] ["p" "𝔭"] ["q" "𝔮"] ["r" "𝔯"] ["s" "𝔰"] ["t" "𝔱"] ["u" "𝔲"] ["v" "𝔳"] ["w" "𝔴"] ["x" "𝔵"] ["y" "𝔶"] ["z" "𝔷"] ])
        $useMap
        )
    (if @reverse-direction-p
        (progn (setq $useMap
                     (mapcar
                      (lambda ($x)
                        (vector (aref $x 1) (aref $x 0)))
                      $latin-to-gothic)))
      (progn (setq $useMap $latin-to-gothic)))
    (save-excursion
      (save-restriction
        (narrow-to-region @begin @end)
        (let ( (case-fold-search nil))
          (mapc
           (lambda ($x)
             (goto-char (point-min))
             (while (search-forward (elt $x 0) nil t)
               (replace-match (elt $x 1) "FIXEDCASE" "LITERAL")))
           $useMap))))))

(defun xah-convert-latin-to-rune (@begin @end @to-latin-p)
  "Replace English alphabet to Unicode runic characters.
For example, f → ᚠ.
When called interactively, work on current line or text selection.

If `universal-argument' is called first, reverse direction.
Note: original letter case are not preserved. B may become b.

URL `http://ergoemacs.org/misc/elisp_latin_to_rune.html'
Version 2019-06-07"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end) current-prefix-arg )
     (list (line-beginning-position) (line-end-position) current-prefix-arg )))
  (let* (
         ($toLower
          ;; this, because we no want to change case of other lang's chars
          [["A" "a"]
           ["B" "b"]
           ["C" "c"]
           ["D" "d"]
           ["E" "e"]
           ["F" "f"]
           ["G" "g"]
           ["H" "h"]
           ["I" "i"]
           ["J" "j"]
           ["K" "k"]
           ["L" "l"]
           ["M" "m"]
           ["N" "n"]
           ["O" "o"]
           ["P" "p"]
           ["Q" "q"]
           ["R" "r"]
           ["S" "s"]
           ["T" "t"]
           ["U" "u"]
           ["V" "v"]
           ["W" "w"]
           ["X" "x"]
           ["Y" "y"]
           ["Z" "z"]
           ]
          )
         ($toLatin
          [ ["ᛆ" "a"]
            ["ᛒ" "b"]
            ["ᛍ" "c"]
            ["ᛑ" "d"]
            ["ᚧ" "ð"]
            ["ᛂ" "e"]
            ["ᚠ" "f"]
            ["ᚵ" "g"]
            ["ᚼ" "h"]
            ["ᛁ" "i"]
            ["ᚴ" "k"]
            ["ᛚ" "l"]
            ["ᛘ" "m"]
            ["ᚿ" "n"]
            ["ᚮ" "o"]
            ["ᛔ" "p"]
            ["ᛕ" "p"]
            ["ᛩ" "q"]
            ["ᚱ" "r"]
            ["ᛌ" "s"]
            ["ᛋ" "s"]
            ["ᛐ" "t"]
            ["ᚢ" "u"]
            ["ᚡ" "v"]
            ["ᚢ" "v"]
            ["ᚥ" "w"]
            ["ᛪ" "x"]
            ["ᛦ" "y"]
            ["ᚤ" "y"]
            ["ᛨ" "y"]
            ["ᛎ" "z"]
            ["ᚦ" "þ"]
            ["ᛅ" "æ"]
            ["ᛆ" "ä"]
            ["ᚯ" "ø"]
            ["ᚯ" "ö"]
            ]
          )
         ($toRunic
          (mapcar
           (lambda ($x)
             (vector (aref $x 1) (aref $x 0)))
           $toLatin))
         ($useMap (if @to-latin-p
                      $toLatin
                    $toRunic)))
    (save-excursion
      (save-restriction
        (narrow-to-region @begin @end)
        (when (not @to-latin-p)
          ;; change to lower case, but only for English letters, not for example greek etc.
          (mapc
           (lambda ($x)
             (goto-char (point-min))
             (while (search-forward (elt $x 0) nil t)
               (replace-match (elt $x 1) "FIXEDCASE" "LITERAL")))
           $toLower))
        (let ( (case-fold-search nil))
          (mapc
           (lambda ($x)
             (goto-char (point-min))
             (while (search-forward (elt $x 0) nil t)
               (replace-match (elt $x 1) "FIXEDCASE" "LITERAL")))
           $useMap))))))

(defun xah-convert-latin-to-braille (@begin @end @reverse-direction-p)
  "Replace English alphabet to Unicode braille characters.

When called interactively, work on current line or text selection.
If `universal-argument' is called first, reverse direction.
Note: original letter case are not preserved. B may become b.

URL `http://ergoemacs.org/misc/elisp_latin_to_braille.html'
Version 2019-09-17"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end) current-prefix-arg )
     (list (line-beginning-position) (line-end-position) current-prefix-arg )))
  (let (
        ($latin-to-braille
         [
          ["1" "⠼⠁"] ["2" "⠼⠃"] ["3" "⠼⠉"] ["4" "⠼⠙"] ["5" "⠼⠑"] ["6" "⠼⠋"] ["7" "⠼⠛"] ["8" "⠼⠓"] ["9" "⠼⠊"] ["0" "⠼⠚"]
          ["," "⠂"] [";" "⠆"] [":" "⠒"] ["." "⠲"] ["?" "⠦"] ["!" "⠖"] ["‘" "⠄"] ["“" "⠄⠶"] ["“" "⠘⠦"] ["”" "⠘⠴"] ["‘" "⠄⠦"] ["’" "⠄⠴"] ["(" "⠐⠣"] [")" "⠐⠜"] ["/" "⠸⠌"] ["\\","⠸⠡"] ["-" "⠤"] ["–" "⠠⠤"] ["—" "⠐⠠⠤"]
          ["a" "⠁"] ["b" "⠃"] ["c" "⠉"] ["d" "⠙"] ["e" "⠑"] ["f" "⠋"] ["g" "⠛"] ["h" "⠓"] ["i" "⠊"] ["j" "⠚"] ["k" "⠅"] ["l" "⠇"] ["m" "⠍"] ["n" "⠝"] ["o" "⠕"] ["p" "⠏"] ["q" "⠟"] ["r" "⠗"] ["s" "⠎"] ["t" "⠞"] ["u" "⠥"] ["v" "⠧"] ["w" "⠺"] ["x" "⠭"] ["y" "⠽"] ["z" "⠵"] ]
         )
        $useMap
        )
    (setq $useMap
          (if @reverse-direction-p
              (mapcar
               (lambda ($x)
                 (vector (aref $x 1) (aref $x 0)))
               $latin-to-braille)
            $latin-to-braille))
    (save-excursion
      (save-restriction
        (narrow-to-region @begin @end)
        (let ( (case-fold-search t))
          (mapc
           (lambda ($x)
             (goto-char (point-min))
             (while (search-forward (elt $x 0) nil t)
               (replace-match (elt $x 1) "FIXEDCASE" "LITERAL")))
           $useMap))))))


;; (defun xah-twitterfy-old-2019-01-14 (@begin @end &optional @direction)
;;   "Shorten words for Twitter 280 char limit on current line or selection.
;; The conversion direction is automatically determined.

;; If `universal-argument' is called, ask for conversion direction.

;; Note: calling this function twice in opposite direction does not necessarily return the origial, because the map is not one-to-one, also the string in the map overlaps.

;; When called in lisp code, @begin @end are region begin/end positions. @direction must be one of the following values:
;;  \"auto\"
;;  \"shorten\"
;;  \"lengthen\"

;; URL `http://ergoemacs.org/emacs/elisp_twitterfy.html'
;; Version 2019-01-14"
;;   (interactive
;;    (list
;;     (if (use-region-p) (region-beginning) (line-beginning-position))
;;     (if (use-region-p) (region-end) (line-end-position))
;;     (if current-prefix-arg
;;         (ido-completing-read
;;          "Direction: "
;;          '( "shorten"  "lengthen")
;;          "PREDICATE"
;;          "REQUIRE-MATCH")
;;       "auto"
;;       )))
;;   (let (($shorten-map
;;          [
;;           [" are " " r "]
;;           [" are, " " r,"]
;;           [" you " " u "]
;;           [" you," " u,"]
;;           [" you." " u."]
;;           [" you." " u。"]
;;           ["e.g. " "eg "]
;;           [" to " " 2 "]
;;           [" your" " ur "]
;;           [" and " "＆"]
;;           ["because" "cuz"]
;;           ["therefore " "∴"]
;;           [" at " " @ "]
;;           [" love " " ♥ "]
;;           [" one " " 1 "]
;;           [" two " " 2 "]
;;           [" three " " 3 "]
;;           [" four " " 4 "]
;;           [" zero " " 0 "]
;;           ["hexadecimal " "hex "]
;;           ["Emacs: " "#emacs "]
;;           ["JavaScript: " "#JavaScript "]
;;           ["Python: " "#python "]
;;           ["Ruby: " "#ruby "]
;;           ["Perl: " "#perl "]
;;           ["Emacs Lisp: " "#emacs #lisp "]
;;           ["Elisp: " "#emacs #lisp "]
;;           [", " "，"]
;;           ["..." "…"]
;;           [". " "。"]
;;           ["? " "？"]
;;           [": " "："]
;;           ["! " "！"]]
;;          ))
;;     (save-restriction
;;       (narrow-to-region @begin @end)
;;       (when (string-equal @direction "auto")
;;         (goto-char (point-min))
;;         (setq @direction
;;               (if (re-search-forward "。\\|，\\|？\\|！" nil t)
;;                   "lengthen" "shorten"
;;                   )))
;;       (let ( (case-fold-search nil)
;;              ($map (if (string-equal @direction "shorten")
;;                        $shorten-map
;;                      (mapcar (lambda (x) (vector (elt x 1) (elt x 0))) $shorten-map))))
;;         (mapc
;;          (lambda ($x)
;;            (goto-char (point-min))
;;            (while (search-forward (elt $x 0) nil t)
;;              (replace-match (elt $x 1) "FIXEDCASE" "LITERAL")))
;;          $map)
;;         (goto-char (point-min))
;;         (while (re-search-forward "  +" nil t)
;;           (replace-match " " "FIXEDCASE" "LITERAL")))
;;       (goto-char 280))))

(defun xah-twitterfy ()
  "Shorten words for Twitter 280 char limit on current line or selection.

If `universal-argument' is called first, ask for conversion direction (shorten/lenthen).

Note: calling this function twice in opposite direction does not necessarily return the origial, because the map is not one-to-one.

URL `http://ergoemacs.org/emacs/elisp_twitterfy.html'
Version 2019-03-02"
  (interactive)
  (let (
        $p1 $p2
        $direction
        ($shorten-map
         [
          ["\\bare\\b" "r"]
          ["\\byou\\b" "u"]
          ["e.g. " "eg "]
          ["\bto\b" "2"]
          [" your" " ur "]
          ["\\band\\b" "＆"]
          ["\\bbecause\\b" "∵"]
          ["\\bcuz\\b" "∵"]
          ["therefore " "∴"]
          [" at " " @ "]
          [" love " " ♥ "]
          [" one " " 1 "]
          [" two " " 2 "]
          [" three " " 3 "]
          [" four " " 4 "]
          [" zero " " 0 "]
          ["hexadecimal " "hex "]
          ["Emacs: " "#emacs "]
          ["JavaScript: " "#JavaScript "]
          ["Python: " "#python "]
          ["Ruby: " "#ruby "]
          ["Perl: " "#perl "]
          ["Emacs Lisp: " "#emacs #lisp "]
          ["Elisp: " "#emacs #lisp "]
          [", " "，"]
          ["\\.\\.\\." "…"]
          ["\\. " "。"]
          ["\\? " "？"]
          [": " "："]
          ["! " "！"]]
         )
        ($lengeth-map
         [
          ["\\bu\\b" "you"]
          ["\\br\\b" "are"]
          ["eg " "e.g. "]
          [" 2 " " to "]
          ["\\bur\\b" "your"]
          ["\\b＆\\b" "and"]
          ["\\bcuz\\b" "because"]
          ["\\b∴\\b" "therefore "]
          [" @ " " at "]
          [" ♥ " " love "]
          [" 1 " " one "]
          [" 2 " " two "]
          [" 3 " " three "]
          [" 4 " " four "]
          [" 0 " " zero "]
          ["hex " "hexadecimal "]
          ["，" ", "]
          ["…" "..."]
          ["。" ". "]
          ["？" "? "]
          ["：" ": "]
          ["！" "! "]
          ]
         ))
    (if (region-active-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (save-excursion
        (if (re-search-backward "\n[ \t]*\n" nil "move")
            (progn
              (setq $p1 (point))
              (re-search-forward "\n[ \t]*\n"))
          (setq $p1 (point)))
        (progn
          (re-search-forward "\n[ \t]*\n" nil "move")
          (setq $p2 (point)))))
    (setq $direction
          (if current-prefix-arg
              (ido-completing-read
               "Direction: "
               '( "shorten"  "lengthen")
               "PREDICATE"
               "REQUIRE-MATCH")
            "auto"
            ))
    (save-restriction
      (narrow-to-region $p1 $p2)
      (when (string-equal $direction "auto")
        (goto-char (point-min))
        (setq $direction
              (if (re-search-forward "。\\|，\\|？\\|！" nil t)
                  "lengthen" "shorten"
                  )))
      (let ( (case-fold-search nil))
        (mapc
         (lambda ($x)
           (goto-char (point-min))
           (while (re-search-forward (elt $x 0) nil t)
             (replace-match (elt $x 1) "FIXEDCASE" "LITERAL")))
         (if (string-equal $direction "shorten")
             $shorten-map
           $lengeth-map))
        (goto-char (point-min))
        (while (re-search-forward "  +" nil t)
          (replace-match " " "FIXEDCASE" "LITERAL")))
      (goto-char (+ (point-min) 280)))))

     ;; (let*
     ;;     ;; 2016-11-06
     ;;     ;; trying to auto find the replacement bracket by looking at char before or after
     ;;     ;; problem is, then you need to find the matching bracket for replacement. need more tedious code. abandone for now
     ;;     (
     ;;      ($bracketsList '("() paren" "{} braces" "[] square" "<> greater" "“” curly quote" "‘’ single" "‹› french" "«» double french" "「」 corner" "『』 double corner" "【】 LENTICULAR" "〖〗 white LENTICULAR" "《》 double angle" "〈〉 angle " "〔〕 TORTOISE" "⦅⦆ white paren" "〚〛 white square" "⦃⦄ white braces" "〈〉" "⦑⦒" "⧼⧽" "⟦⟧ math square" "⟨⟩ math angle" "⟪⟫" "⟮⟯" "⟬⟭" "❛❜" "❝❞" "❨❩" "❪❫" "❴❵" "❬t❭" "❮❯" "❰❱" " none" ))

     ;;      ($leftBrackets (mapcar (lambda (x) (substring x 0 1)) $bracketsList)))
     ;;   (let (($charBefore (char-before))
     ;;         ($charAfter (char-after)))
     ;;     (or
     ;;      (catch 'found
     ;;        (dolist (x $leftBrackets nil)
     ;;          (when (eq (string-to-char x) $charBefore)
     ;;            (progn (throw 'found x)))))
     ;;      (catch 'found
     ;;        (dolist (x $leftBrackets nil)
     ;;          (when (eq (string-to-char x) $charAfter)
     ;;            (progn (throw 'found x))))))))

(defun xah-angle-brackets-to-html (&optional @begin @end)
  "Replace all 〈…〉 to <cite>…</cite> and 《…》 to <cite class=\"book\">…</span> in current text block or selection.

When called non-interactively, @begin @end are region positions.

URL `http://ergoemacs.org/emacs/elisp_replace_title_tags.html'
version 2017-06-10"
  (interactive)
  (let (($changedItems '())
        (case-fold-search nil)
        $p1 $p2
        )
    (if (and @begin @end)
        (setq $p1 (region-beginning) $p2 (region-end))
      (if (use-region-p)
          (setq $p1 (region-beginning) $p2 (region-end))
        (save-excursion
          (if (re-search-backward "\n[ \t]*\n" nil "move")
              (progn (re-search-forward "\n[ \t]*\n")
                     (setq $p1 (point)))
            (setq $p1 (point)))
          (if (re-search-forward "\n[ \t]*\n" nil "move")
              (progn (re-search-backward "\n[ \t]*\n")
                     (setq $p2 (point)))
            (setq $p2 (point))))))
    (save-restriction
      (narrow-to-region $p1 $p2)
      (goto-char (point-min))
      (while (re-search-forward "《\\([^》]+?\\)》" nil t)
        (push (match-string-no-properties 1) $changedItems)
        (replace-match "<cite class=\"book\">\\1</cite>" "FIXEDCASE"))
      (goto-char (point-min))
      (while (re-search-forward "〈\\([^〉]+?\\)〉" nil t)
        (push (match-string-no-properties 1) $changedItems)
        (replace-match "<cite>\\1</cite>" t)))
    (if (> (length $changedItems) 0)
        (mapcar
         (lambda ($x)
           (princ $x)
           (terpri))
         (reverse $changedItems))
      (message "No change needed."))))

(defun xah-remove-square-brackets (&optional @begin @end)
  "Delete any text of the form “[‹n›]”, eg [1], [2], … in current text block or selection.

For example
 「… announced as Blu-ray Disc [11][12], and …」
becomes
 「… announced as Blu-ray Disc, and …」.

When called non-interactively, @begin @end are region positions.

URL `http://ergoemacs.org/emacs/elisp_replace_title_tags.html'
Version 2017-06-10"
  (interactive)
  (let ($p1 $p2 $changedItems)
    (if (and  @begin @end)
        (setq $p1 (region-beginning) $p2 (region-end))
      (if (use-region-p)
          (setq $p1 (region-beginning) $p2 (region-end))
        (save-excursion
          (if (re-search-backward "\n[ \t]*\n" nil "move")
              (progn (re-search-forward "\n[ \t]*\n")
                     (setq $p1 (point)))
            (setq $p1 (point)))
          (if (re-search-forward "\n[ \t]*\n" nil "move")
              (progn (re-search-backward "\n[ \t]*\n")
                     (setq $p2 (point)))
            (setq $p2 (point))))))
    (save-restriction
      (narrow-to-region $p1 $p2)
      (progn
        (goto-char 1)
        (while (re-search-forward "\\(\\[[0-9]+?\\]\\)" nil t)
          (setq $changedItems (cons (match-string 1) $changedItems ))
          (replace-match "" t)))
      (progn
        (goto-char 1)
        (while (search-forward "[citation needed]" nil t)
          (setq $changedItems (cons "[citation needed]" $changedItems ))
          (backward-char 17)
          (delete-char 17))))
    (if (> (length $changedItems) 0)
        (mapcar
         (lambda ($x)
           (princ $x)
           (terpri))
         (reverse $changedItems))
      (message "No change needed."))))

;; HHH___________________________________________________________________

(defun xah-curly-quotes-to-bracket (@left-bracket @right-bracket)
  "Replace “…” to one of 「…」.
Which bracket is determined by the string LEFTBRACKET and RIGHTBRACKET."
  (interactive)
  (let ()
    (if (string-equal major-mode "dired-mode")
        (progn
          (dired-do-query-replace-regexp
           "“\\([^”]+?\\)”"
           (concat @left-bracket "\\1" @right-bracket)
           ))
      (progn (query-replace-regexp
              "“\\([^”]+?\\)”"
           (concat @left-bracket "\\1" @right-bracket) )) ) ))

(defun xah-curly-quotes-to-code-bracket ()
  "Replace “…” to 「…」"
  (interactive)
  (xah-curly-quotes-to-bracket "「" "」")
)

(defun xah-curly-quotes-to-html-code-tag ()
  "Replace 「“…”」 to 「<code>…</code>」"
  (interactive)
  (xah-curly-quotes-to-bracket "<code>" "</code>")
)

(defun xah-curly-quotes-to-html-strong-tag ()
  "Replace 「“…”」 to 「<strong>…</strong>」"
  (interactive)
  (xah-curly-quotes-to-bracket "<strong>" "</strong>")
)

(defun xah-curly-quotes-to-elisp-function-bracket ()
  "Replace “…” to ｢…｣"
  (interactive)
  (xah-curly-quotes-to-bracket "｢" "｣")
)

(defun xah-curly-quotes-to-french-quote ()
  "Replace “…” to «…»"
  (interactive)
  (xah-curly-quotes-to-bracket "«" "»")
)

(defun xah-curly-quotes-to-kbd-tag ()
  "Replace “…” to <kbd>…</kbd>"
  (interactive)
  (xah-curly-quotes-to-bracket "<kbd>" "</kbd>")
)

(defun xah-curly-quotes-to-keyboard-bracket ()
  "Replace “…” to 【…】"
  (interactive)
  (xah-curly-quotes-to-bracket "【" "】")
)

(defun xah-curly-quotes-to-menu-bracket ()
  "Replace “…” to 〖…〗"
  (interactive)
  (xah-curly-quotes-to-bracket "〖" "〗")
)

(defun xah-curly-quotes-to-book-bracket ()
  "Replace “…” to 《…》"
  (interactive)
  (xah-curly-quotes-to-bracket "《" "》")
)

(defun xah-curly-quotes-to-title-bracket ()
  "Replace “…” to 〈…〉"
  (interactive)
  (xah-curly-quotes-to-bracket "〈" "〉")
)

(defun xah-curly-quotes-to-file-path ()
  "Replace “…” to 〔…〕"
  (interactive)
  (xah-curly-quotes-to-bracket "〔" "〕")
)

(defun xah-single-quote-to-curly (@begin @end)
  "Replace straight double quotes to curly ones etc.
URL `http://ergoemacs.org/emacs/elisp_straight_curly_quotes.html'
Version 2019-07-25"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (let ($p1 $p2)
       (save-excursion
         (if (re-search-backward "\n[ \t]*\n" nil "move")
             (progn (re-search-forward "\n[ \t]*\n")
                    (setq $p1 (point)))
           (setq $p1 (point)))
         (if (re-search-forward "\n[ \t]*\n" nil "move")
             (progn (re-search-backward "\n[ \t]*\n")
                    (setq $p2 (point)))
           (setq $p2 (point))))
       (list $p1 $p2))))
  (let ( (case-fold-search nil))
    (save-excursion
      (save-restriction
        (narrow-to-region @begin @end )
        (xah-replace-pairs-region
         (point-min) (point-max)
         [
          [">\'" ">‘"]
          [" \'" " ‘"]
          ["(\'" "(‘"]

          ["\' " "’ "]
          ["\'," "’,"]
          [".\'" ".’"]
          ["!\'" "!’"]
          ["?\'" "?’"]
          ["\')" "’)"]
          ["\']" "’]"]
          ] "REPORT" "HILIGHT")

        (xah-replace-regexp-pairs-region
         (point-min) (point-max)
         [
          ["\\bcan’t\\b" "can't"]
          ["\\bdon’t\\b" "don't"]
          ["\\bdoesn’t\\b" "doesn't"]
          ["\\bwon’t\\b" "won't"]
          ["\\bisn’t\\b" "isn't"]
          ["\\baren’t\\b" "aren't"]
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
          ] "FIXEDCASE" "LITERAL-P" "HILIGHT")
        ;;
        ))))

(defun xah-ascii-to-math-symbol (@begin @end)
  "Replace straight double quotes to curly ones etc.
URL `http://ergoemacs.org/emacs/elisp_straight_curly_quotes.html'
Version 2019-07-25"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (let ($p1 $p2)
       (save-excursion
         (if (re-search-backward "\n[ \t]*\n" nil "move")
             (progn (re-search-forward "\n[ \t]*\n")
                    (setq $p1 (point)))
           (setq $p1 (point)))
         (if (re-search-forward "\n[ \t]*\n" nil "move")
             (progn (re-search-backward "\n[ \t]*\n")
                    (setq $p2 (point)))
           (setq $p2 (point))))
       (list $p1 $p2))))
  (let ( (case-fold-search nil))
    (save-excursion
      (save-restriction
        (narrow-to-region @begin @end )
        (xah-replace-pairs-region
         (point-min) (point-max)
         [
          [" ---> " " ⟶ "]
          [" --> " " ⟶ "]
          [" <= " " ≤ "]
          [" >= " " ≥ "]
          ["--" " — "]
          ["~=" "≈"]
          ] "REPORT" "HILIGHT")
        ;;
        ))))

(defun xah-prettify-punctuations (@begin @end)
  "Replace straight double quotes to curly ones etc.
URL `http://ergoemacs.org/emacs/elisp_straight_curly_quotes.html'
Version 2019-07-25"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (let ($p1 $p2)
       (save-excursion
         (if (re-search-backward "\n[ \t]*\n" nil "move")
             (progn (re-search-forward "\n[ \t]*\n")
                    (setq $p1 (point)))
           (setq $p1 (point)))
         (if (re-search-forward "\n[ \t]*\n" nil "move")
             (progn (re-search-backward "\n[ \t]*\n")
                    (setq $p2 (point)))
           (setq $p2 (point))))
       (list $p1 $p2))))
  (let ( (case-fold-search nil))
    (save-excursion
      (save-restriction
        (narrow-to-region @begin @end )
        (xah-replace-pairs-region
         (point-min) (point-max)
         [
          ["  —  " " — "] ; rid of extra space in em-dash
          ["..." "…"]
          [" & " " ＆ "]
          [" :)" " 😊"]
          [" :(" " ☹"]
          [" ;)" " 😉"]
          [" , " ", "]
          ["—" " — "]
          ] "REPORT" "HILIGHT")
        ;;
        ))))

(defun xah-replace-straight-quotes (@begin @end)
  "Replace straight double quotes to curly ones, and others.
Works on current text block or selection.

Examples of changes:
 「\"…\"」 → 「“…”」
 「...」 → 「…」
 「I’m」 → 「I'm」
 「--」 → 「—」
 「~=」 → 「≈」

When called in lisp code, @begin and @end are region begin/end positions.

WARNING: this command does not guarantee 100% correct conversion of quotes, because it impossible. You should double check highlighted places after.

URL `http://ergoemacs.org/emacs/elisp_straight_curly_quotes.html'
Version 2019-07-22 2020-12-22"
  ;; some examples for debug
  ;; do "‘em all -- done..."
  ;; I’am not
  ;; said "can’t have it, can’t, just can’t"
  ;; ‘I’ve can’t’
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (let ($p1 $p2)
       (save-excursion
         (if (re-search-backward "\n[ \t]*\n" nil "move")
             (progn (re-search-forward "\n[ \t]*\n")
                    (setq $p1 (point)))
           (setq $p1 (point)))
         (if (re-search-forward "\n[ \t]*\n" nil "move")
             (progn (re-search-backward "\n[ \t]*\n")
                    (setq $p2 (point)))
           (setq $p2 (point))))
       (list $p1 $p2))))
  (let ( (case-fold-search nil))
    (save-excursion
      (save-restriction
        (narrow-to-region @begin @end )
        (xah-prettify-punctuations (point-min) (point-max))
        (xah-ascii-to-math-symbol (point-min) (point-max))
        ;; Note: order is important since this is huristic.
        (xah-replace-pairs-region
         (point-min)
         (point-max)
         [
          ;; fix GNU style ASCII quotes
          ["``" "“"]
          ["''" "”"]
          ] "REPORT" "HILIGHT")
        (xah-replace-pairs-region
         (point-min) (point-max)
         [
          ;; double straight quote → double curly quotes
          ["\n\"" "\n“"]
          [">\"" ">“"]
          ["(\"" "(“"]
          [" \"" " “"]
          ["\" " "” "]
          ["\"," "”,"]
          ["\",\n" "”,\n"]
          ["\". " "”. "]
          ["\".\n" "”.\n"]
          ["\"." "”."]
          ["\"?" "”?"]
          ["\";" "”;"]
          ["\":" "”:"]
          ["\")" "”)"]
          ["\"]" "”]"]
          ;; ["\"[" "\”["]
          [".\"" ".”"]
          [",\"" ",”"]
          ["!\"" "!”"]
          ["?\"" "?”"]
          ["\"<" "”<"]
          ["\"\n" "”\n"]
          ] "REPORT" "HILIGHT")
        ;; fix straight double quotes by regex
        (xah-replace-regexp-pairs-region
         (point-min) (point-max)
         [
          ;; ["\\`\"" "“"]
          ;; ["\"\\([-A-Za-z0-9]+\\)\"" "“"]
          ["\"\\([-A-Za-z0-9]+\\)\"" "“\\1”"]
          ] "FIXEDCASE" "LITERAL-P" "HILIGHT")
        (xah-single-quote-to-curly (point-min) (point-max))
        ;; fix back escaped quotes in code
        (xah-replace-pairs-region
         (point-min) (point-max)
         [
          ["\\”" "\\\""]
          ["\\”" "\\\""]
          ] "REPORT" "HILIGHT")
        ;; fix back. quotes in HTML code
        (xah-replace-regexp-pairs-region
         (point-min) (point-max)
         [
          ["” \\([-a-z]+\\)="       "\" \\1="] ; any 「” some-thing=」
          ["=”" "=\""]
          ["/” " "/\" "]
          ["\\([0-9]+\\)” "     "\\1\" "]
          ] "FIXEDCASE" nil "HILIGHT"
         )))))

