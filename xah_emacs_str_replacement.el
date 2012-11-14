;; -*- coding: utf-8 -*-
;; some elisp string replacement functions

;; 2007-06, 2011-09-29
;;   Xah Lee
;; ∑ http://xahlee.org/

(defun cycle-hyphen-underscore-space ()
  "Cyclically replace {underscore, space, hypen} chars current line or text selection.
When called repeatedly, this command cycles the {“ ”, “_”, “-”} characters."
  (interactive)
  ;; this function sets a property 「'state」. Possible values are 0 to length of charArray.
  (let (mainText charArray p1 p2 currentState nextState changeFrom
             changeTo startedWithRegion-p )

    (if (region-active-p)
        (progn
          (setq startedWithRegion-p t )
          (setq p1 (region-beginning))
          (setq p2 (region-end))
          )
      (progn (setq startedWithRegion-p nil ) 
             (setq p1 (line-beginning-position))
             (setq p2 (line-end-position)) ) )

    (setq charArray [" " "_" "-"])

    (setq currentState
          (if (get 'cycle-hyphen-underscore-space 'state) 
              (get 'cycle-hyphen-underscore-space 'state)
            0))
    (setq nextState (% (+ currentState 1) (length charArray)))

    (setq changeFrom (elt charArray currentState ))
    (setq changeTo (elt charArray nextState ))

    (setq mainText (replace-regexp-in-string changeFrom changeTo (buffer-substring-no-properties p1 p2)) )
    (delete-region p1 p2)
    (insert mainText)
    
    (put 'cycle-hyphen-underscore-space 'state nextState)

    (when startedWithRegion-p 
      (goto-char p2)
      (set-mark p1)
      (setq deactivate-mark nil) ) ) )

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

(defun replace-html-characters ()
  "Replace “<” to “&lt;” and some other chars in HTML.
This works on the current text selection or block of text.
The string replaced are:
 & ⇒ &amp;
 < ⇒ &lt;
 > ⇒ &gt;"
  (interactive)
  (let (bds p1 p2 myText)
    (setq bds (get-selection-or-unit 'block))
    (setq myText (elt bds 0) p1 (elt bds 1) p2 (elt bds 2)  )
    (save-excursion (replace-pairs-region p1 p2 '( ["&" "&amp;"] ["<" "&lt;"] [">" "&gt;"] ) ))
     ) )

(defun replace-html-characters-to-unicode ()
  "Replace “<” to “‹” and some other chars in HTML.
This works on the current text selection or block of text.
The string replaced are:
 & ⇒ ＆
 < ⇒ ‹
 > ⇒ ›"
  (interactive)
  (let (bds p1 p2 myText)
    (setq bds (get-selection-or-unit 'block))
    (setq myText (elt bds 0) p1 (elt bds 1) p2 (elt bds 2)  )

    (replace-pairs-region p1 p2 '( ["&" "＆"] ["<" "‹"] [">" "›"] ) ) ) )

(defun to-chinese-punctuation (p1 p2)
  "Replace English punctuation to Chinese version.
When called interactively, do current text block (paragraph) or text selection.
When called in lisp code, p1 p2 are region end points."
  (interactive
   (let ( (bds (get-selection-or-unit 'block)))
     (list (elt bds 1) (elt bds 2) ) ) )
  (replace-pairs-region p1 p2
                        [
                         [". " "。"]
                         [", " "，"]
                         ["," "，"]
                         [": " "："]
                         [".</" "。</"]
                         ["? " "？"]
                         ["?" "？"]
                         ["?</" "？</"]
                         ]))

(defun replace-straight-quotes (p1 p2)
  "Replace straight double quotes to curly ones, and others.
Works on current text selection, else the current text block between empty lines.

Examples of changes:
 \"…\" ⇒ “…”
 ... ⇒ …
 I’m => I'm
 -- ⇒ —
 ~= ⇒ ≈

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
 ["~=" "≈"]
 ])

(replace-pairs-region (point-min) (point-max)
[
 ["  —  " " — "]
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
 ["\n\"" "\n“"]
 ["\"\n" "”\n"]
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

;; fix fix apostrophe
(replace-regexp-pairs-region (point-min) (point-max)
[
 ["\\bcan’t\\b" "can't"]
 ["\\bdon’t\\b" "don't"]
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

;; fix back quotes in HTML code
(replace-pairs-region (point-min) (point-max)
[
 ;; fix back, e.g. in HTML files
 ["=\”" "=\""]
 ["/” " "/\" "]
 ["” href="       "\" href="]
 ["” class="     "\" class="]
 ["” height="    "\" height="]
 ["” width="     "\" width="]
 ["” src="       "\" src="]
 ["” title="       "\" title="]
 ])

 ) ))

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
