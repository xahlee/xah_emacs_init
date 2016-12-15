;; -*- coding: utf-8 -*-
;; 2010-09-03

;; 2015-08-22 removed
;; xah-fly-keys-replace-commands.el xah-fly-keys-misc-commands2.el

;; 2015-08-22
;; removed dependencies to xeu_elisp_util.el xah-get-thing.el xah-find.el xah-replace-pairs.el

(require 'xah-replace-pairs)

(defun xah-replace-newline-whitespaces-to-space (&optional *begin *end *absolute-p)
  "Replace newline+tab char sequence to 1 just space, in current text block or selection.
This is similar to `fill-region' but without being smart.
Version 2015-06-09"
  (interactive)
  (let (-begin -end)
    (if (null *begin)
        (if (use-region-p)
            (progn (setq -begin (region-beginning)) (setq -end (region-end)))
          (save-excursion
            (if (re-search-backward "\n[ \t]*\n" nil "NOERROR")
                (progn (re-search-forward "\n[ \t]*\n")
                       (setq -begin (point)))
              (setq -begin (point)))
            (if (re-search-forward "\n[ \t]*\n" nil "NOERROR")
                (progn (re-search-backward "\n[ \t]*\n")
                       (setq -end (point)))
              (setq -end (point)))))
      (progn (setq -begin *begin) (setq -end *end)))
    (save-excursion
      (save-restriction
        (narrow-to-region -begin -end)
        (goto-char (point-min))
        (while (search-forward-regexp "\n[ \t]*\n" nil t) (replace-match "\n\n"))
        (goto-char (point-min))
        (while (search-forward-regexp "[ \t]*\n[ \t]*" nil t) (replace-match "\n"))
        (goto-char (point-min))
        (while (search-forward-regexp "\n\n+" nil t) (replace-match "hqnvdr9b35"))
        (goto-char (point-min))
        (while (search-forward-regexp "\n" nil t) (replace-match " "))
        (goto-char (point-min))
        (while (search-forward "hqnvdr9b35" nil t) (replace-match "\n\n"))))))

(defun xah-camelCase-underscore (*begin *end)
  "Toggle between camelCase camel_case of current word.

When called in elisp code, *begin *end are region begin/end positions.
2016-01-05 FIXME. currently broken
Version 2015-04-13"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (let ((-bounds (bounds-of-thing-at-point 'symbol)))
       (list (car -bounds) (cdr -bounds)))))
  ;; this function sets a property 「'state」. Possible values are 0 to length of -charArray.
  (let* ((-inputText (buffer-substring-no-properties *begin *end))
         (-charArray ["_" "-" " "])
         (-length (length -charArray))
         (-regionWasActive-p (region-active-p))
         (-nowState
          (if (equal last-command this-command )
              (get 'xah-cycle-hyphen-underscore-space 'state)
            0 ))
         (-changeTo (elt -charArray -nowState)))
    (save-excursion
      (save-restriction
        (narrow-to-region *begin *end)

        ;; def convert(name):
        ;;     s1 = re.sub('(.)([A-Z][a-z]+)', r'\1_\2', name)
        ;;     return re.sub('([a-z0-9])([A-Z])', r'\1_\2', s1).lower()
        (progn
          (goto-char (point-min))
          (while
              (search-forward-regexp "\\(.\\)\\([A-Z][a-z]+\\)" nil 'NOERROR)
            (replace-match "\1_\2" 'FIXEDCASE)))
        (progn
          (goto-char (point-min))
          (while
              (search-forward-regexp "\\([a-z0-9][A-Z]+\\)" nil 'NOERROR)
            (replace-match "\1_\2" 'FIXEDCASE)))))
    (put 'xah-cycle-hyphen-underscore-space 'state (% (+ -nowState 1) -length))))

(defun xah-cycle-camel-style-case ()
  "Cyclically replace {camelStyle, camel_style} current word or text selection.
actually, currently just change from camel to underscore. no cycle
WARNING: this command is currently unstable."
  (interactive)
  ;; this function sets a property 「'state」. Possible values are 0 to length of char_array.
  (let (input_text
        replace_text char_array -p1 -p2 current_state next_state changeFrom
        changeTo startedWithRegion-p )

    (if (region-active-p)
        (progn
          (setq startedWithRegion-p t )
          (setq -p1 (region-beginning))
          (setq -p2 (region-end)))
      (let ((-boundary (bounds-of-thing-at-point 'word)))
        (setq startedWithRegion-p nil )
        (setq -p1 (car -boundary))
        (setq -p2 (cdr -boundary))))

    (setq char_array [" " "_"])

    (setq current_state
          (if (get 'xah-cycle-camel-style-case 'state)
              (get 'xah-cycle-camel-style-case 'state)
            0))
    (setq next_state (% (+ current_state 1) (length char_array)))

    (setq changeFrom (elt char_array current_state ))
    (setq changeTo (elt char_array next_state ))

    (setq input_text (buffer-substring-no-properties -p1 -p2))

    (let ((case-fold-search nil))
      (cond
       ;; camel to underscore
       (
        (equal current_state 0)
        (setq replace_text (replace-regexp-in-string "\\([A-Z]\\)" "_\\1" input_text))
        (setq replace_text (downcase replace_text)))
       ((equal current_state 1)
        (setq replace_text (replace-regexp-in-string "_\\([a-z]\\)" "\\,(upcase \\1)" input_text))
        ;; (setq replace_text (downcase replace_text) )
        )))

    (save-restriction
      (narrow-to-region -p1 -p2)
      (delete-region (point-min) (point-max))
      (insert replace_text))

    (put 'xah-cycle-camel-style-case 'state next_state)))



(defun xah-compact-region (*begin *end)
  "Replace any sequence of whitespace chars to a single space on region.
Whitespace here is considered any of {newline char, tab, space}."
  (interactive "r")
  (xah-replace-regexp-pairs-region
   *begin *end
   '( ["[\n\t]+" " "]
      ["  +" " "])
   t))

(defun xah-format-c-lang-region (*begin *end)
  "Expand region of C style syntax languages so that it is nicely formated.
Experimental code.
WARNING: If region has comment or string, the code'd be fucked up."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region *begin *end)
      (xah-replace-regexp-pairs-region
       *begin *end
       '(
         ["{" "{\n"]
         [";" ";\n"]
         ["}" "}\n"]
         [";[\t\n]*}" "; }"]
         )
       t)
      (indent-region *begin *end))))



(defun xah-replace-latex-to-unicode (*begin *end)
  "Replace TeX markup to Unicode in current line or selection.
Example: \\alpha becomes α.
Version 2015-04-28"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (xah-replace-pairs-region
   *begin
   *end
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
     ) 'REPORT 'HILIGHT ))

(defun xah-replace-text-to-latex-region (*begin *end)
  "Replace math function names or symbols by their LaTeX markup.
Work on current line or selection.
Version 2015-04-28"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (xah-replace-pairs-region
   *begin
   *end
   '(
     ["*" "\\ "]
     ["cos(" "\\cos("]
     ["sin(" "\\sin("]
     ["tan(" "\\tan("]
     [" pi" "\\!\\pi"]
     ["R^2" "\\mathbb{R}^2"]
     ["R^3" "\\mathbb{R}^3"]
     ) 'REPORT 'HILIGHT ))

(defun xah-replace-mathematica-symbols (*begin *end)
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
   *begin
   *end
   '(
     ["\\[Infinity]" "∞"]
     ["\\[Equal]" "=="]) 'REPORT 'HILIGHT ))

(defun xah-replace-greek-letter-name-to-symbol (*begin *end)
  "Replace alpha to α, beta to β etc in current line or selection.

URL `http://ergoemacs.org/emacs/elisp_replace_greeks_to_symbols.html'
Version 2016-10-05"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let ((case-fold-search nil))
    (xah-replace-pairs-region
     *begin
     *end
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
       ) 'REPORT 'HILIGHT )))

(defun xah-replace-mathematica-to-lsl (*begin *end)
  "Change Mathematica syntax to LSL syntax on region.

LSL is Linden Scripting Language.
Version 2015-04-28"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (xah-replace-pairs-region
   *begin
   *end
   '(
     ["Cos[" "llCos("]
     ["Sin[" "llSin("]
     ["Tan[" "llTan("]
     ["Pi" "PI"]
     ["π" "PI"]
     ["{" "<"]
     ["}" ">"]) 'REPORT 'HILIGHT ))

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
  (while (search-forward-regexp "  +" nil t) (replace-match " " nil t))

  (goto-char 1)
  (while (search-forward ", " nil t) (replace-match "," nil t))

  (goto-char 1)
  (while (search-forward-regexp "\\([0-9]\\)\\.\\([0-9][0-9][0-9]\\)[0-9]+" nil t) (replace-match "\\1.\\2" t nil)))

(defun xah-convert-english-chinese-punctuation (*begin *end &optional *to-direction)
  "Convert punctuation from/to English/Chinese characters.

When called interactively, do current line or selection. The conversion direction is automatically determined.

If `universal-argument' is called, ask user for change direction.

When called in lisp code, *begin *end are region begin/end positions. *to-direction must be any of the following values: 「\"chinese\"」, 「\"english\"」, 「\"auto\"」.

See also: `xah-remove-punctuation-trailing-redundant-space'.

URL `http://ergoemacs.org/emacs/elisp_convert_chinese_punctuation.html'
Version 2015-10-05"
  (interactive
   (let (-p1 -p2)
     (if (use-region-p)
         (progn
           (setq -p1 (region-beginning))
           (setq -p2 (region-end)))
       (progn
         (setq -p1 (line-beginning-position))
         (setq -p2 (line-end-position))))
     (list
      -p1
      -p2
      (if current-prefix-arg
          (ido-completing-read
           "Change to: "
           '( "english"  "chinese")
           "PREDICATE"
           "REQUIRE-MATCH")
        "auto"
        ))))
  (let (
        (-input-str (buffer-substring-no-properties *begin *end))
        (-replacePairs
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

    (when (string= *to-direction "auto")
      (setq
       *to-direction
       (if
           (or
            (string-match "　" -input-str)
            (string-match "。" -input-str)
            (string-match "，" -input-str)
            (string-match "？" -input-str)
            (string-match "！" -input-str))
           "english"
         "chinese")))
    (save-excursion
      (save-restriction
        (narrow-to-region *begin *end)
        (mapc
         (lambda (-x)
           (progn
             (goto-char (point-min))
             (while (search-forward (aref -x 0) nil "noerror")
               (replace-match (aref -x 1)))))
         (cond
          ((string= *to-direction "chinese") -replacePairs)
          ((string= *to-direction "english") (mapcar (lambda (x) (vector (elt x 1) (elt x 0))) -replacePairs))
          (t (user-error "Your 3rd argument 「%s」 isn't valid" *to-direction))))))))

(defun xah-convert-chinese-numeral (*begin *end &optional *to-chinese)
  "Replace convert Chinese numeral to Arabic numeral, or reverse.
On current line or selection.
If `universal-argument' is called first, do reverse direction.
Version 2015-04-29"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (let* ((-numMap [["○" "0"] ["一" "1"] ["二" "2"] ["三" "3"] ["四" "4"] ["五" "5"] ["六" "6"] ["七" "7"] ["八" "8"] ["九" "9"] ]))
    (xah-replace-pairs-region
     *begin *end
     (if *to-chinese (mapcar (lambda (x) (vector (elt x 1) (elt x 0))) -numMap) -numMap )
     t t
     )))

(defun xah-remove-vowel ()
  "Remove the following letters: {a e i o u} in current line or text selection.
Version 2015-08-22"
  (interactive)
  (let (-p1 -p2 )
    (if (use-region-p)
        (progn
          (setq -p1 (region-beginning))
          (setq -p2 (region-end)))
      (progn
        (setq -p1 (line-beginning-position))
        (setq -p2 (line-end-position))))
    (save-excursion
      (save-restriction
        (narrow-to-region -p1 -p2)
        (let ( (case-fold-search nil))
          (goto-char (point-min))
          (while (search-forward-regexp "a\\|e\\|i\\|o\\|u" (point-max) 'NOERROR)
            (replace-match "" 'FIXEDCASE 'LITERAL)))))))

(defun xah-replace-profanity ()
  "Replace fuck shit scumbag … in current line or text selection.
Version 2016-10-05"
  (interactive)
  (let ( -p1 -p2)
    (if (use-region-p)
        (progn
          (setq -p1 (region-beginning))
          (setq -p2 (region-end)))
      (progn
        (setq -p1 (line-beginning-position))
        (setq -p2 (line-end-position))))
    (xah-replace-pairs-region
     -p1 -p2
     '(
       ["fuck" "f￼ck"]
       ["shit" "sh￼t"]
       ["motherfuck" "m￼th￼rf￼ck"]
       ) 'REPORT 'HILIGHT )))

(defun xah-replace-slanted-apostrophe ()
  "Replace some single curly apostrophe to straight version.
Works on current line or text selection.
Example: 「it’s」 ⇒ 「it's」."
  (interactive)
  (let ( -p1 -p2)
    (if (use-region-p)
        (progn
          (setq -p1 (region-beginning))
          (setq -p2 (region-end)))
      (progn
        (setq -p1 (line-beginning-position))
        (setq -p2 (line-end-position))))
    (xah-replace-pairs-region
     -p1 -p2
     '(
       ["‘tis" "'tis"]
       ["’s" "'s"]
       ["’d" "'d"]
       ["n’t" "n't"]
       ["’ve" "'ve"]
       ["’ll" "'ll"]
       ["’m" "'m"]
       ["’re" "'re"]
       ["s’ " "s' "]) 'REPORT 'HILIGHT )))

(defun xah-convert-fullwidth-chars (*begin *end &optional *to-direction)
  "Convert ASCII chars to/from Unicode fullwidth version.
Works on current line or text selection.

The conversion direction is determined like this: if the command has been repeated, then toggle. Else, always do to-Unicode direction.

If `universal-argument' is called:

 no C-u → Automatic.
 C-u → to ASCII
 C-u 1 → to ASCII
 C-u 2 → to Unicode

When called in lisp code, *begin *end are region begin/end positions. *to-direction must be any of the following values: 「\"unicode\"」, 「\"ascii\"」, 「\"auto\"」.

See also: `xah-remove-punctuation-trailing-redundant-space'.
Version 2016-10-05"
  (interactive
   (let (-p1 -p2)
     (if (use-region-p)
         (progn
           (setq -p1 (region-beginning))
           (setq -p2 (region-end)))
       (progn
         (setq -p1 (line-beginning-position))
         (setq -p2 (line-end-position))))
     (list -p1 -p2
           (cond
            ((equal current-prefix-arg nil) "auto")
            ((equal current-prefix-arg '(4)) "ascii")
            ((equal current-prefix-arg 1) "ascii")
            ((equal current-prefix-arg 2) "unicode")
            (t "unicode")))))
  (let* (
         (--ascii-unicode-map
          [
           ["0" "０"] ["1" "１"] ["2" "２"] ["3" "３"] ["4" "４"] ["5" "５"] ["6" "６"] ["7" "７"] ["8" "８"] ["9" "９"]
           ["A" "Ａ"] ["B" "Ｂ"] ["C" "Ｃ"] ["D" "Ｄ"] ["E" "Ｅ"] ["F" "Ｆ"] ["G" "Ｇ"] ["H" "Ｈ"] ["I" "Ｉ"] ["J" "Ｊ"] ["K" "Ｋ"] ["L" "Ｌ"] ["M" "Ｍ"] ["N" "Ｎ"] ["O" "Ｏ"] ["P" "Ｐ"] ["Q" "Ｑ"] ["R" "Ｒ"] ["S" "Ｓ"] ["T" "Ｔ"] ["U" "Ｕ"] ["V" "Ｖ"] ["W" "Ｗ"] ["X" "Ｘ"] ["Y" "Ｙ"] ["Z" "Ｚ"]
           ["a" "ａ"] ["b" "ｂ"] ["c" "ｃ"] ["d" "ｄ"] ["e" "ｅ"] ["f" "ｆ"] ["g" "ｇ"] ["h" "ｈ"] ["i" "ｉ"] ["j" "ｊ"] ["k" "ｋ"] ["l" "ｌ"] ["m" "ｍ"] ["n" "ｎ"] ["o" "ｏ"] ["p" "ｐ"] ["q" "ｑ"] ["r" "ｒ"] ["s" "ｓ"] ["t" "ｔ"] ["u" "ｕ"] ["v" "ｖ"] ["w" "ｗ"] ["x" "ｘ"] ["y" "ｙ"] ["z" "ｚ"]
           ["," "，"] ["." "．"] [":" "："] [";" "；"] ["!" "！"] ["?" "？"] ["\"" "＂"] ["'" "＇"] ["`" "｀"] ["^" "＾"] ["~" "～"] ["¯" "￣"] ["_" "＿"]
           ["&" "＆"] ["@" "＠"] ["#" "＃"] ["%" "％"] ["+" "＋"] ["-" "－"] ["*" "＊"] ["=" "＝"] ["<" "＜"] [">" "＞"] ["(" "（"] [")" "）"] ["[" "［"] ["]" "］"] ["{" "｛"] ["}" "｝"] ["(" "｟"] [")" "｠"] ["|" "｜"] ["¦" "￤"] ["/" "／"] ["\\" "＼"] ["¬" "￢"] ["$" "＄"] ["£" "￡"] ["¢" "￠"] ["₩" "￦"] ["¥" "￥"]
           ]
          )
         (--reverse-map
          (mapcar
           (lambda (x) (vector (elt x 1) (elt x 0)))
           --ascii-unicode-map))

         (cmdStates ["to-unicode" "to-ascii"])
         (stateBefore (if (get 'xah-convert-fullwidth-chars 'state) (get 'xah-convert-fullwidth-chars 'state) 0))
         (stateAfter (% (+ stateBefore (length cmdStates) 1) (length cmdStates))))

  ;"０\\|１\\|２\\|３\\|４\\|５\\|６\\|７\\|８\\|９\\|Ａ\\|Ｂ\\|Ｃ\\|Ｄ\\|Ｅ\\|Ｆ\\|Ｇ\\|Ｈ\\|Ｉ\\|Ｊ\\|Ｋ\\|Ｌ\\|Ｍ\\|Ｎ\\|Ｏ\\|Ｐ\\|Ｑ\\|Ｒ\\|Ｓ\\|Ｔ\\|Ｕ\\|Ｖ\\|Ｗ\\|Ｘ\\|Ｙ\\|Ｚ\\|ａ\\|ｂ\\|ｃ\\|ｄ\\|ｅ\\|ｆ\\|ｇ\\|ｈ\\|ｉ\\|ｊ\\|ｋ\\|ｌ\\|ｍ\\|ｎ\\|ｏ\\|ｐ\\|ｑ\\|ｒ\\|ｓ\\|ｔ\\|ｕ\\|ｖ\\|ｗ\\|ｘ\\|ｙ\\|ｚ"

  ;(message "before %s" stateBefore)
  ;(message "after %s" stateAfter)
  ;(message "*to-direction %s" *to-direction)
  ;(message "real-this-command  %s" this-command)
  ;(message "real-last-command %s" last-command)

    (let ((case-fold-search nil))
      (xah-replace-pairs-region
       *begin *end
       (cond
        ((string= *to-direction "unicode") --ascii-unicode-map)
        ((string= *to-direction "ascii") --reverse-map)
        ((string= *to-direction "auto")
         (if (equal this-command last-command)
             (if (eq stateBefore 0)
                 --ascii-unicode-map
               --reverse-map
               )
           --ascii-unicode-map
           ))
        (t (user-error "Your 3rd argument 「%s」 isn't valid" *to-direction)))
       t t ))
    (put 'xah-convert-fullwidth-chars 'state stateAfter)))

(defun xah-remove-punctuation-trailing-redundant-space (*begin *end)
  "Remove redundant whitespace after punctuation.
Works on current line or text selection.

When called in emacs lisp code, the *begin *end are cursor positions for region.

See also `xah-convert-english-chinese-punctuation'.

URL `http://ergoemacs.org/emacs/elisp_convert_chinese_punctuation.html'
version 2015-08-22"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (require 'xah-replace-pairs)
  (xah-replace-regexp-pairs-region
   *begin *end
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

(defun xah-convert-asian/ascii-space (*begin *end)
  "Change all space characters between Asian Ideographic one to ASCII one.
Works on current line or text selection.

When called in emacs lisp code, the *begin *end are cursor positions for region.

See also `xah-convert-english-chinese-punctuation'
 `xah-remove-punctuation-trailing-redundant-space'
"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let ((--space-char-map
         [
          ["　" " "]
          ]
         ))
    (xah-replace-regexp-pairs-region
     *begin *end
     (if (string-match "　" (buffer-substring-no-properties *begin *end))
         --space-char-map
       (mapcar (lambda (x) (vector (elt x 1) (elt x 0))) --space-char-map))
     "FIXEDCASE" "LITERAL")))

(defun xah-convert-latin-alphabet-gothic (*begin *end *reverse-direction-p)
  "Replace English alphabets to Unicode gothic characters.
For example, A ⇒ 𝔄, a ⇒ 𝔞.

When called interactively, work on current line or text selection.

If any `universal-argument' is called first, reverse direction.

When called in elisp, the *begin and *end are region begin/end positions to work on.

URL `http://ergoemacs.org/misc/thou_shalt_use_emacs_lisp.html'
Version 2015-04-12"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end) current-prefix-arg )
     (list (line-beginning-position) (line-end-position) current-prefix-arg )))
  (let (
        (-latin-to-gothic [ ["A" "𝔄"] ["B" "𝔅"] ["C" "ℭ"] ["D" "𝔇"] ["E" "𝔈"] ["F" "𝔉"] ["G" "𝔊"] ["H" "ℌ"] ["I" "ℑ"] ["J" "𝔍"] ["K" "𝔎"] ["L" "𝔏"] ["M" "𝔐"] ["N" "𝔑"] ["O" "𝔒"] ["P" "𝔓"] ["Q" "𝔔"] ["R" "ℜ"] ["S" "𝔖"] ["T" "𝔗"] ["U" "𝔘"] ["V" "𝔙"] ["W" "𝔚"] ["X" "𝔛"] ["Y" "𝔜"] ["Z" "ℨ"] ["a" "𝔞"] ["b" "𝔟"] ["c" "𝔠"] ["d" "𝔡"] ["e" "𝔢"] ["f" "𝔣"] ["g" "𝔤"] ["h" "𝔥"] ["i" "𝔦"] ["j" "𝔧"] ["k" "𝔨"] ["l" "𝔩"] ["m" "𝔪"] ["n" "𝔫"] ["o" "𝔬"] ["p" "𝔭"] ["q" "𝔮"] ["r" "𝔯"] ["s" "𝔰"] ["t" "𝔱"] ["u" "𝔲"] ["v" "𝔳"] ["w" "𝔴"] ["x" "𝔵"] ["y" "𝔶"] ["z" "𝔷"] ])
        -useMap
        )
    (if *reverse-direction-p
        (progn (setq -useMap
                     (mapcar
                      (lambda (-x)
                        (vector (aref -x 1) (aref -x 0)))
                      -latin-to-gothic)))
      (progn (setq -useMap -latin-to-gothic)))
    (save-excursion
      (save-restriction
        (narrow-to-region *begin *end)
        (let ( (case-fold-search nil))
          (mapc
           (lambda (-x)
             (goto-char (point-min))
             (while (search-forward (elt -x 0) nil t)
               (replace-match (elt -x 1) 'FIXEDCASE 'LITERAL)))
           -useMap))))))

(defun xah-twitterfy (*begin *end &optional *to-direction)
  "Shorten words for Twitter 140 char limit on current line or selection.
The conversion direction is automatically determined.

If `universal-argument' is called, ask for conversion direction.

Note: calling this function twice in opposite direction does not necessarily return the origial, because the map is not one-to-one, also the string in the map overlaps.

When called in lisp code, *begin *end are region begin/end positions. *to-direction must be one of the following values:
 'auto
 'shorten
 'lengthen

URL `http://ergoemacs.org/emacs/elisp_twitterfy.html'
Version 2016-12-13"
  (interactive
   (list
    (if (use-region-p) (region-beginning) (line-beginning-position))
    (if (use-region-p) (region-end) (line-end-position))
    (intern
     (if current-prefix-arg
         (ido-completing-read
          "Direction: "
          '( "shorten"  "lengthen")
          "PREDICATE"
          "REQUIRE-MATCH")
       "auto"
       ))))
  (let ((-shorten-map
         [
          [" are " " r "]
          [" are, " " r,"]
          [" you " " u "]
          [" you," " u,"]
          [" you." " u."]
          [" you." " u。"]
          [" to " " 2 "]
          [" your" " ur "]
          [" and " "＆"]
          ["because" "cuz"]
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
          [", " "，"]
          ["..." "…"]
          [". " "。"]
          ["? " "？"]
          [": " "："]
          ["! " "！"]]
         ))
    (save-excursion
      (save-restriction
        (narrow-to-region *begin *end)
        (when (string= *to-direction 'auto)
          (goto-char (point-min))
          (if
              (re-search-forward "。\\|，\\|？\\|！" nil 'NOERROR)
              (setq *to-direction 'lengthen)
            (setq *to-direction 'shorten)))
        (let ( (case-fold-search nil))
          (mapc
           (lambda (-x)
             (goto-char (point-min))
             (while (search-forward (elt -x 0) nil t)
               (replace-match (elt -x 1) 'FIXEDCASE 'LITERAL)))
           (if (string= *to-direction 'shorten)
               -shorten-map
             (mapcar (lambda (-pair) (vector (elt -pair 1) (elt -pair 0))) -shorten-map)))
          (goto-char (point-min))
          (while (search-forward "  " nil t)
            (replace-match " " 'FIXEDCASE 'LITERAL))

          (goto-char (point-min))
          (while (search-forward "  " nil t)
            (replace-match " " 'FIXEDCASE 'LITERAL)))))))

(defun xah-remove-quotes-or-brackets (*begin *end *bracketType)
  "Remove quotes/brackets in current line or text selection.

When called in lisp program, *begin *end are region begin/end position, *bracketType is a string of a bracket pair. eg \"()\",  \"[]\", etc.
URL `http://ergoemacs.org/emacs/elisp_change_brackets.html'
Version 2016-11-04"
  (interactive
   (list
    (if (use-region-p) (region-beginning) (line-beginning-position))
    (if (use-region-p) (region-end) (line-end-position))
    (ido-completing-read
     "Replace this:"
     '("() paren"
       "{} braces" "[] square"
       "<> greater"
       "“” curly quote"
       "‘’ single"
       "‹› french"
       "«» double french"
       "「」 corner"
       "『』 double corner"
       "【】 LENTICULAR"
       "〖〗 white LENTICULAR"
       "《》 double angle"
       "〈〉 angle "
       "〔〕 TORTOISE"
       "⦅⦆ white paren"
       "〚〛 white square"
       "⦃⦄ white braces"
       "〈〉"
       "⦑⦒"
       "⧼⧽"
       "⟦⟧ math square"
       "⟨⟩ math angle"
       "⟪⟫"
       "⟮⟯"
       "⟬⟭"
       "❛❜"
       "❝❞"
       "❨❩"
       "❪❫"
       "❴❵"
       "❬❭"
       "❮❯"
       "❰❱"))))
  (save-excursion
    (save-restriction
      (narrow-to-region *begin *end)
      (let ( (case-fold-search nil))
        (mapc
         (lambda (x)
           (goto-char (point-min))
           (while (search-forward (char-to-string x)  nil t)
             (replace-match "" 'FIXEDCASE 'LITERAL)))
         (substring *bracketType 0 2))))))

(defun xah-change-bracket-pairs ( *fromType *toType *begin *end)
  "Change bracket pairs from one type to another on current line or selection.
For example, change all parenthesis () to square brackets [].

When called in lisp program, *begin *end are region begin/end position, *fromType or *toType is a string of a bracket pair. eg \"()\",  \"[]\", etc.
URL `http://ergoemacs.org/emacs/elisp_change_brackets.html'
Version 2016-11-04"
  (interactive
   (let ((-bracketsList
          '("() paren"
            "{} braces" "[] square"
            "<> greater"
            "“” curly quote"
            "‘’ single"
            "‹› french"
            "«» double french"
            "「」 corner"
            "『』 double corner"
            "【】 LENTICULAR"
            "〖〗 white LENTICULAR"
            "《》 double angle"
            "〈〉 angle "
            "〔〕 TORTOISE"
            "⦅⦆ white paren"
            "〚〛 white square"
            "⦃⦄ white braces"
            "〈〉"
            "⦑⦒"
            "⧼⧽"
            "⟦⟧ math square"
            "⟨⟩ math angle"
            "⟪⟫"
            "⟮⟯"
            "⟬⟭"
            "❛❜"
            "❝❞"
            "❨❩"
            "❪❫"
            "❴❵"
            "❬❭"
            "❮❯"
            "❰❱"
            "   none"
            )))

     ;; (let*
     ;;     ;; 2016-11-06
     ;;     ;; trying to auto find the replacement bracket by looking at char before or after
     ;;     ;; problem is, then you need to find the matching bracket for replacement. need more tedious code. abandone for now
     ;;     (
     ;;      (-bracketsList '("() paren" "{} braces" "[] square" "<> greater" "“” curly quote" "‘’ single" "‹› french" "«» double french" "「」 corner" "『』 double corner" "【】 LENTICULAR" "〖〗 white LENTICULAR" "《》 double angle" "〈〉 angle " "〔〕 TORTOISE" "⦅⦆ white paren" "〚〛 white square" "⦃⦄ white braces" "〈〉" "⦑⦒" "⧼⧽" "⟦⟧ math square" "⟨⟩ math angle" "⟪⟫" "⟮⟯" "⟬⟭" "❛❜" "❝❞" "❨❩" "❪❫" "❴❵" "❬❭" "❮❯" "❰❱" " none" ))

     ;;      (-leftBrackets (mapcar (lambda (x) (substring x 0 1)) -bracketsList)))
     ;;   (let ((-charBefore (char-before))
     ;;         (-charAfter (char-after)))
     ;;     (or
     ;;      (catch 'found
     ;;        (dolist (x -leftBrackets nil)
     ;;          (when (eq (string-to-char x) -charBefore)
     ;;            (progn (throw 'found x)))))
     ;;      (catch 'found
     ;;        (dolist (x -leftBrackets nil)
     ;;          (when (eq (string-to-char x) -charAfter)
     ;;            (progn (throw 'found x))))))))

     (list
      (ido-completing-read "Replace this:" -bracketsList )
      (ido-completing-read "To:" -bracketsList )
      (if (use-region-p) (region-beginning) nil)
      (if (use-region-p) (region-end) nil))))
  (save-excursion
    (save-restriction
      (when (null *begin)
        (setq *begin (line-beginning-position))
        (setq *end (line-end-position)))
      (narrow-to-region *begin *end)
      (let ( (case-fold-search nil)
             (-fromLeft (substring *fromType 0 1))
             (-toLeft (if (string-equal (substring *toType 0 1) " ")
                          (progn "")
                        (substring *toType 0 1)))
             (-fromRight (substring *fromType 1 2))
             (-toRight (if (string-equal (substring *toType 1 2) " ")
                           (progn "")
                         (substring *toType 1 2))))
        (progn
          (goto-char (point-min))
          (while (search-forward -fromLeft nil t)
            (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
            (replace-match -toLeft 'FIXEDCASE 'LITERAL)))
        (progn
          (goto-char (point-min))
          (while (search-forward -fromRight nil t)
            (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
            (replace-match -toRight 'FIXEDCASE 'LITERAL)))))))

(defun xah-corner-bracket→html-i (*begin *end)
       "Replace all 「…」 to <code>…</code> in current text block.
When called with `universal-argument', work on visible portion of whole buffer (i.e. respect `narrow-to-region'). When call in lisp program, the *begin *end are region positions."
       (interactive
        (cond
         ((equal current-prefix-arg nil) ; universal-argument not called
          (let (pt1 pt2)
                   (save-excursion
                     (if (re-search-backward "\n[ \t]*\n" nil "move")
                         (progn (re-search-forward "\n[ \t]*\n")
                                (setq pt1 (point)))
                       (setq pt1 (point)))
                     (if (re-search-forward "\n[ \t]*\n" nil "move")
                         (progn (re-search-backward "\n[ \t]*\n")
                                (setq pt2 (point)))
                       (setq pt2 (point)))
                     (list pt1 pt2))))
         (t ; all other cases
          (list (point-min) (point-max)))))
       (save-excursion
         (save-restriction
           (narrow-to-region *begin *end)
           (goto-char (point-min))
           (while (search-forward-regexp "「\\([^」]+?\\)」" nil t)
             (if (y-or-n-p "Replace this one?")
                 (replace-match "<code>\\1</code>" t) ) ) )) )

(defun xah-angle-brackets-to-html (*begin *end)
  "Replace all 〈…〉 to <cite>…</cite> and 《…》 to <cite class=\"book\">…</span> in current text block or selection.

When called non-interactively, *begin *end are region positions.

URL `http://ergoemacs.org/emacs/elisp_replace_title_tags.html'
version 2015-04-13"
  (interactive
   (let (-p1 -p2)
     (save-excursion
       (if (re-search-backward "\n[ \t]*\n" nil "move")
           (progn (re-search-forward "\n[ \t]*\n")
                  (setq -p1 (point)))
         (setq -p1 (point)))
       (if (re-search-forward "\n[ \t]*\n" nil "move")
           (progn (re-search-backward "\n[ \t]*\n")
                  (setq -p2 (point)))
         (setq -p2 (point))))
     (list -p1 -p2)))

  (let ((-changedItems '())
        (case-fold-search nil))
    (save-restriction
      (narrow-to-region *begin *end)

      (goto-char (point-min))
      (while (search-forward-regexp "《\\([^》]+?\\)》" nil t)
        (push (match-string-no-properties 1) -changedItems)
        (replace-match "<cite class=\"book\">\\1</cite>" "FIXEDCASE"))

      (goto-char (point-min))
      (while (search-forward-regexp "〈\\([^〉]+?\\)〉" nil t)
        (push (match-string-no-properties 1) -changedItems)
        (replace-match "<cite>\\1</cite>" t)))

    (if (> (length -changedItems) 0)
        (mapcar
         (lambda (-x)
           (princ -x)
           (terpri))
         (reverse -changedItems))
      (message "No change needed."))))

(defun xah-remove-square-brackets (*begin *end)
  "Delete any text of the form “[‹n›]”, eg [1], [2], … in current text block or selection.

For example
 「… announced as Blu-ray Disc [11][12], and …」
becomes
 「… announced as Blu-ray Disc, and …」.

When called non-interactively, *begin *end are region positions.

URL `http://ergoemacs.org/emacs/elisp_replace_title_tags.html'
Version 2015-06-04"
  (interactive
   (let (-p1 -p2)
     (save-excursion
       (if (re-search-backward "\n[ \t]*\n" nil "move")
           (progn (re-search-forward "\n[ \t]*\n")
                  (setq -p1 (point)))
         (setq -p1 (point)))
       (if (re-search-forward "\n[ \t]*\n" nil "move")
           (progn (re-search-backward "\n[ \t]*\n")
                  (setq -p2 (point)))
         (setq -p2 (point))))
     (list -p1 -p2)))
  (let (-changedItems)
    (save-restriction
      (narrow-to-region *begin *end)
      (goto-char 1)
      (while (search-forward-regexp "\\(\\[[0-9]+?\\]\\)" nil t)
        (setq -changedItems (cons (match-string 1) -changedItems ))
        (replace-match "" t))

      (goto-char 1)
      (while (search-forward "[citation needed]" nil t)
        (setq -changedItems (cons "[citation needed]" -changedItems ))
        (backward-char 17)
        (delete-char 17)))

    (if (> (length -changedItems) 0)
        (mapcar
         (lambda (-x)
           (princ -x)
           (terpri))
         (reverse -changedItems))
      (message "No change needed."))))


(defun xah-curly-quotes→bracket (*left-bracket *right-bracket)
  "Replace “…” to one of 「…」.
Which bracket is determined by the string LEFTBRACKET and RIGHTBRACKET."
  (interactive)
  (let ()
    (if (string= major-mode "dired-mode")
        (progn
          (dired-do-query-replace-regexp
           "“\\([^”]+?\\)”"
           (concat *left-bracket "\\1" *right-bracket)
           ))
      (progn (query-replace-regexp
              "“\\([^”]+?\\)”"
           (concat *left-bracket "\\1" *right-bracket) )) ) ))

(defun xah-curly-quotes→code-bracket ()
  "Replace “…” to 「…」"
  (interactive)
  (xah-curly-quotes→bracket "「" "」")
)

(defun xah-curly-quotes→html-code-tag ()
  "Replace 「“…”」 to 「<code>…</code>」"
  (interactive)
  (xah-curly-quotes→bracket "<code>" "</code>")
)

(defun xah-curly-quotes→html-strong-tag ()
  "Replace 「“…”」 to 「<strong>…</strong>」"
  (interactive)
  (xah-curly-quotes→bracket "<strong>" "</strong>")
)

(defun xah-curly-quotes→elisp-function-bracket ()
  "Replace “…” to ｢…｣"
  (interactive)
  (xah-curly-quotes→bracket "｢" "｣")
)

(defun xah-curly-quotes→french-quote ()
  "Replace “…” to «…»"
  (interactive)
  (xah-curly-quotes→bracket "«" "»")
)

(defun xah-curly-quotes→kbd-tag ()
  "Replace “…” to <kbd>…</kbd>"
  (interactive)
  (xah-curly-quotes→bracket "<kbd>" "</kbd>")
)

(defun xah-curly-quotes→keyboard-bracket ()
  "Replace “…” to 【…】"
  (interactive)
  (xah-curly-quotes→bracket "【" "】")
)

(defun xah-curly-quotes→menu-bracket ()
  "Replace “…” to 〖…〗"
  (interactive)
  (xah-curly-quotes→bracket "〖" "〗")
)

(defun xah-curly-quotes→book-bracket ()
  "Replace “…” to 《…》"
  (interactive)
  (xah-curly-quotes→bracket "《" "》")
)

(defun xah-curly-quotes→title-bracket ()
  "Replace “…” to 〈…〉"
  (interactive)
  (xah-curly-quotes→bracket "〈" "〉")
)

(defun xah-curly-quotes→file-path ()
  "Replace “…” to 〔…〕"
  (interactive)
  (xah-curly-quotes→bracket "〔" "〕")
)

;; (defun curly-quotes-replacement ()
;;   "to be used …
;; TODO

;; Replace “…” to one of 〔…〕, 「…」, 【…】"
;;   (interactive)
;;   (let (replacePattern)

;;     (goto-char 1)
;;     (search-forward-regexp "“\\([^”]+?\\)”" nil t)

;;     (cond
;;      ((or
;;        (string-match "^Ctrl" (match-string-no-properties 1 ) )
;;        (string-match "^Alt" (match-string-no-properties 1 ) )
;;        (string-match "^Win" (match-string-no-properties 1 ) )
;;        (string-match "^Menu" (match-string-no-properties 1 ) )
;;        (string-match "^Meta" (match-string-no-properties 1 ) )
;;        (string-match "^Cmd" (match-string-no-properties 1 ) )
;;        (string-match "^Opt" (match-string-no-properties 1 ) )
;;        (string-match "^Super" (match-string-no-properties 1 ) )
;;        (string-match "^Hyper" (match-string-no-properties 1 ) )
;;        )
;;       (setq replacePattern "【\1】" )
;;       )
;;      (CONDITION BODY)
;;      )

;;     )
;;   )

(defun xah-replace-straight-quotes (*begin *end)
  "Replace straight double quotes to curly ones, and others.
Works on current text block or selection.

Examples of changes:
 「\"…\"」 ⇒ 「“…”」
 「...」 ⇒ 「…」
 「I’m」 => 「I'm」
 「--」 ⇒ 「—」
 「~=」 ⇒ 「≈」

When called in lisp code, *begin and *end are region begin/end positions.

WARNING: this command does not guarantee 100% correct conversion, because it's heuristics based. Also, if you use it in code, such as HTML, watch out for bad change of straight quotes such as in 「class=\"…\"」.

URL `http://ergoemacs.org/emacs/elisp_straight_curly_quotes.html'
Version 2016-08-22"
  ;; some examples for debug
  ;; do "‘em all -- done..."
  ;; I’am not
  ;; said "can’t have it, can’t, just can’t"
  ;; ‘I’ve can’t’
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (let (-p1 -p2)
       (save-excursion
         (if (re-search-backward "\n[ \t]*\n" nil "move")
             (progn (re-search-forward "\n[ \t]*\n")
                    (setq -p1 (point)))
           (setq -p1 (point)))
         (if (re-search-forward "\n[ \t]*\n" nil "move")
             (progn (re-search-backward "\n[ \t]*\n")
                    (setq -p2 (point)))
           (setq -p2 (point))))
       (list -p1 -p2))))

  (let ( (case-fold-search nil))
    (save-excursion
      (save-restriction
        (narrow-to-region *begin *end )
        ;; Note: order is important since this is huristic.
        (xah-replace-pairs-region
         (point-min) (point-max)
         [
          ;; dash and ellipsis etc
          ["--" " — "]
          ["—" " — "]
          ["..." "…"]
          [" & " " ＆ "]
          [" :)" " ☺"]
          [" :(" " ☹"]
          [" ;)" " 😉"]
          ["~=" "≈"]
          [" , " ", "]
          ;; fix GNU style ASCII quotes
          ["``" "“"]
          ["''" "”"]
          ;; "straight quote" ⇒ “double quotes”
          ["\n\"" "\n“"]
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
          ["\"\n" "”\n"]
          ] 'REPORT 'HILIGHT)

        (xah-replace-pairs-region
         (point-min) (point-max)
         [
          ["  —  " " — "] ; rid of extra space in em-dash
          ] 'REPORT 'HILIGHT)

        ;; fix straight double quotes by regex
        (xah-replace-regexp-pairs-region
         (point-min) (point-max)
         [
          ["\\`\"" "“"]
          ])

        ;; fix single quotes to curly
        (xah-replace-pairs-region
         (point-min) (point-max)
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
          ] 'REPORT 'HILIGHT)

        (xah-replace-regexp-pairs-region
         (point-min) (point-max)
         [
          ["\\bcan’t\\b" "can't"]
          ["\\bdon’t\\b" "don't"]
          ["\\bdoesn’t\\b" "doesn't"]
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
          ])

        ;; fix back escaped quotes in code
        (xah-replace-pairs-region
         (point-min) (point-max)
         [
          ["\\”" "\\\""]
          ] 'REPORT 'HILIGHT)

        ;; fix back. quotes in HTML code
        (xah-replace-regexp-pairs-region
         (point-min) (point-max)
         [
          ["” \\([-a-z]+\\)="       "\" \\1="] ; any 「” some-thing=」
          ["=”" "=\""]
          ["/” " "/\" "]
          ["\\([0-9]+\\)” "     "\\1\" "]
          ]
         )))))

