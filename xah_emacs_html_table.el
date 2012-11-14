;; -*- coding: utf-8 -*-
;; some custome string functions for working with HTML table
;; 2011-11-04
;;   Xah Lee
;; ∑ http://xahlee.org/



(defun make-html-table-string (textBlock ξdelimiter)
  "Transform the string TEXTBLOCK into a HTML marked up table.

 “\\n” is used as delimiter of rows. Extra newlines at the end is discarded.
The argument ΞDELIMITER is a char used as the delimiter for columns.

 See the parent function `make-html-table'."
(let ((txtbk textBlock))
    (setq txtbk (replace-regexp-in-string "\n+$" "\n" (concat txtbk "\n"))) ; make sure ending is just one newline char
    (setq txtbk (replace-regexp-in-string ξdelimiter "</td><td>" txtbk))
    (setq txtbk (replace-regexp-in-string "\n" "</td></tr>\n<tr><td>" txtbk))
    (setq txtbk (substring txtbk 0 -8)) ; delete the beginning “<tr><td>” in last line
    (concat "<table class=\"nrm\">\n<tr><td>" txtbk "</table>")
))

(defun make-html-table (sep)
  "Transform the current text block or selection into a HTML table.

If there's a text selection, use the selection as input.
Otherwise, used current text block delimited by empty lines.

SEP is a string used as a delimitor for columns.

For example:

a*b*c
1*2*3
this*and*that

with “*” as separator, becomes

<table class=\"nrm\">
<tr><td>a</td><td>b</td><td>c</td></tr>
<tr><td>1</td><td>2</td><td>3</td></tr>
<tr><td>this</td><td>and</td><td>that</td></tr>
</table>"
  (interactive "sEnter string pattern for column separation:")
  (let (bds p1 p2 myStr)

    (setq bds (get-selection-or-unit 'block))
    (setq myStr (elt bds 0) )
    (setq p1 (elt bds 1) )
    (setq p2 (elt bds 2) )
    (delete-region p1 p2)
    (insert (make-html-table-string myStr sep) "\n")
  ))

;; (defun html-table-switch-column (ξi ξj)
;;   "Switch the ξi ξj th columns in a HTML table."
;;   (interactive "nEnter i-th column:\nnEnter j-th column:")
;;   (let (bds p1 p2 myStr)

;; (search-backward "<table")
;; (setq p1 (point))
;; (search-forward "</table>")
;; (setq p2 (point))
;; (setq myStr (buffer-substring-no-properties p1 p2))

;; (with-temp-buffer
;; (setq myStr )
 
;; )

;;     (setq bds (get-selection-or-unit 'block))
;;     (setq myStr (elt bds 0) )
;;     (setq p1 (elt bds 1) )
;;     (setq p2 (elt bds 2) )
;;     (delete-region p1 p2)
;;     (insert (make-html-table-string myStr sep) "\n")
;;   ))