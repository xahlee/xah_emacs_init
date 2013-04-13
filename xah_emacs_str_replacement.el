;; -*- coding: utf-8 -*-
;; some elisp string replacement functions

;; 2007-06, 2011-09-29
;;   Xah Lee
;; ∑ http://xahlee.org/

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

(defun replace-mathematica-symbols-region (start end)
  "Replace Mathematica's special char encoding to Unicode of the same semantics.
For example:
 \\=\\[Infinity] ⇒ ∞
 \\=\\[Equal] ⇒ =="
  (interactive "r")
  (replace-pairs-region start end '(
 ["\\[Infinity]" "∞"]
 ["\\[Equal]" "=="])))

(defun replace-greek-region (start end)
  "Replace math symbols. e.g. alpha to α."
  (interactive "r")
(replace-pairs-region start end '(
["alpha" "α"]
["beta" "β"]
["gamma" "γ"]
["theta" "θ"]
["lambda" "λ"]
["delta" "δ"]
["epsilon" "ε"]
["omega" "ω"]
["Pi" "π"])))

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

                               (t (error "Your 3rd argument 「%s」 isn't valid." ξ-to-direction)) ) ) ) )

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
                                [",  +" ", "]
                                ["?  +" "? "]
                                ["!  +" "! "]
                                ["\\.   +" "\\. "]

                                ["， +" "，"]
                                ["。 +" "。"]
                                ["： +" "："]
                                ["？ +" "？"]
                                ["； +" "；"]
                                ["！ +" "！"]
                                ["、 +" "、"]
                                ]
                               "FIXEDCASE" "LITERAL") )

(defun convert-ideographic/ascii-space (p1 p2)
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

(defun replace-straight-quotes (p1 p2)
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
) ) ))

(defun escape-quotes ()
  "Replace 「\"」 by 「\\\"」 in current line or text selection."
  (interactive)
  (let (bds p1 p2)
    (setq bds (get-selection-or-unit 'line))
    (setq p1 (elt bds 1) p2 (elt bds 2)  )
    (replace-pairs-region p1 p2 '(["\"" "\\\""]))
    )
  )

(defun unescape-quotes ()
  "Replace  「\\\"」 by 「\"」 in current line or text selection."
  (interactive)
  (let (bds p1 p2)
    (setq bds (get-selection-or-unit 'line))
    (setq p1 (elt bds 1) p2 (elt bds 2)  )
    (replace-pairs-region p1 p2 '(["\\\"" "\""]))
    )
  )

(defun replace-curly-apostrophe ()
  "Replace some single curly quotes to straight version,
in current text block or text selection.
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


(defun replace-tex-region (start end)
  "Replace some math function names or symbols by their LaTeX markup."
  (interactive "r")
(replace-pairs-region start end '(
["*" "\\ "]
["cos(" "\\cos("]
["sin(" "\\sin("]
["tan(" "\\tan("]
[" pi" "\\!\\pi"]
["R^2" "\\mathbb{R}^2"]
["R^3" "\\mathbb{R}^3"])))

(defun mathematica-to-lsl-region (start end)
  "Change Mathematica syntax to LSL syntax on region.

LSL is Linden Scripting Language.
This command does simple string replacement only."
  (interactive "r")
(replace-pairs-region start end '(
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

(defun compact-region (start end)
  "Replace any sequence of whitespace chars to a single space on region.
Whitespace here is considered any of \\n, tab, space ."
  (interactive "r")
  (replace-regexp-pairs-region start end
                               '( ["[\n\t]+" " "]
                                  ["  +" " "])
                               t))

(defun format-c-lang-region (start end)
  "Expand region of c style syntax languages so that it is nicely formated.
Experimental code.
WARNING: If region has comment or string, the code'd be fucked up."
  (interactive "r")

  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (replace-regexp-pairs-region start end
                                   '(
                                     ["{" "{\n"]
                                     [";" ";\n"]
                                     ["}" "}\n"]
                                     [";[\t\n]*}" "; }"]
                                     )
                                   t)
      (indent-region start end)
      )
    )
  )

(defun xah-clean-whitespace ()
  "Delete trailing whitespace, and replace sequence of newlines into just 2.

Work on whole buffer, or text selection."
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
          (while (search-forward-regexp " +\n" nil "noerror")
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

