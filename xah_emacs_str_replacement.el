;; -*- coding: utf-8 -*-

(defun xah-replace-mathematica-symbols-region (p1 p2)
  "Replace Mathematica's special char encoding to Unicode of the same semantics.
For example:
 \\=\\[Infinity] ⇒ ∞
 \\=\\[Equal] ⇒ =="
  (interactive "r")
  (replace-pairs-region p1 p2 '(
 ["\\[Infinity]" "∞"]
 ["\\[Equal]" "=="])))

(defun xah-replace-greek-region (p1 p2)
  "Replace math symbols. e.g. alpha to α."
  (interactive "r")
(replace-pairs-region p1 p2 '(
["alpha" "α"]
["beta" "β"]
["gamma" "γ"]
["theta" "θ"]
["lambda" "λ"]
["delta" "δ"]
["epsilon" "φ"]
["omega" "ω"]
["Pi" "π"])))

(defun xah-replace-tex-region (p1 p2)
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
