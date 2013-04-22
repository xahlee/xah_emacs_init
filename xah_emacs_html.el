;; -*- coding: utf-8 -*-
;; some custome string functions for working with HTML
;; most things moved to xah-html-mode
;; ∑ http://xahlee.org/

(defun forward-html-end-tag ()
  "Move cursor to the next HTML tag's content."
  (interactive)
  (forward-char 1)
  (search-forward "</")
  (backward-char 2)
  )

(defun backward-html-end-tag ()
  "Move cursor to the previous HTML tag's content."
  (interactive)
  (search-backward "</")
  ;; (forward-char-char 2)
  )



(defun mark-unicode (p1)
  "Wrap 「<mark class=\"unicode\"></mark>」 around current character.

When called in elisp program, wrap the tag at point P1."
  (interactive (list (point)))
  (goto-char p1)
  (insert "<mark class=\"unicode\">")
  (forward-char 1)
  (insert "</mark>"))



(defun xahsite-update-article-timestamp ()
  "Update article's timestamp.
Add today's date to the form
 <p class=\"author_0\">Xah Lee, <time>2005-01-17</time>, <time>2011-07-25</time></p>
 of current file."
  (interactive)
  (let (p1 p2)
    (progn
      (goto-char 1)
      (when (search-forward "<p class=\"author_0\">Xah Lee" nil)
        (beginning-of-line)
        (setq p1 (point) )
        (end-of-line)
        (setq p2 (point) )
        (search-backward "</p>")
        (insert (format ", <time>%s</time>" (format-time-string "%Y-%m-%d")))
        (message "%s" (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
) ) ))

(defun xahsite-update-page-tag-old (p1 p2)
  "Update HTML page navigation tags.

The input is a text selection.
Each line should a file name
Update each file's page navigation tag.

Each file name is a file path without dir, and relative to current dir.
Sample text selection for input:
“combowords.html
combowords-2.html
combowords-3.html
combowords-4.html”"
  (interactive "r")
  (let (filez pageNavStr (i 1))
    (setq filez
          (split-string (buffer-substring-no-properties p1 p2) "\n" t)
          )

    (delete-region p1 p2)

    ;; generate the page nav string
    (setq pageNavStr "<div class=\"pgs\">")

    (while (<= i (length filez))
      (setq pageNavStr
            (concat pageNavStr
                    "<a href=\""
                    (nth (- i 1) filez)
                    "\">"
                    (number-to-string i)
                    "</a>, ")
            )
      (setq i (1+ i))
      )

    (setq pageNavStr (substring pageNavStr 0 -2) ) ; remove the last ", "
    (setq pageNavStr (concat pageNavStr "</div>"))

    ;; open each file, insert the page nav string, remove link in the
    ;; nav string that's the current page
    (mapc
     (lambda (thisFile)
       (message "%s" thisFile)
       (find-file thisFile)
       (goto-char (point-min))
       (search-forward "<div class=\"pgs\">")
       (beginning-of-line)
       (kill-line 1)
       (insert pageNavStr "\n")
       (search-backward (file-name-nondirectory buffer-file-name))

       (require 'sgml-mode)
       (sgml-delete-tag 1)
       ;;        (save-buffer)
       ;;        (kill-buffer)
       )
     filez)
    ))

(defun xahsite-update-page-tag ()
  "Update HTML page navigation tags.

The input is a text block or text selection.
Each line should a file name/path (can be relative path)
Update each file's page navigation tag.

Each file name is a file path without dir, and relative to current dir.
Sample text selection for input:

words.html
words-2.html
words-3.html
words-4.html
"
  (interactive)
  (require 'sgml-mode)
  (let (bds p1 p2 inputStr fileList pageNavStr )
    (setq bds (get-selection-or-unit 'block))
    (setq inputStr (elt bds 0) p1 (elt bds 1) p2 (elt bds 2)  )
    (setq fileList (split-string (buffer-substring-no-properties p1 p2) "\n" t) )

    (delete-region p1 p2)

    ;; generate the page nav string
    (setq pageNavStr (format "<nav class=\"page\">\n%s</nav>"
                             (let (ξresult linkPath fTitle (ξi 0) )
                               (while (< ξi (length fileList))
                                 (setq linkPath (elt fileList ξi) )
                                 (setq fTitle (get-html-file-title linkPath) )
                                 (setq ξresult (concat ξresult "<a href=\"" linkPath "\" title=\"" fTitle "\">" (number-to-string (1+ ξi)) "</a>\n") )
                                 (setq ξi (1+ ξi))
                                 )
                               ξresult
                               )))

    ;; open each file, insert the page nav string
    (mapc
     (lambda (thisFile)
       (message "%s" thisFile)
       (find-file thisFile)
       (goto-char 1)

       (if
           (search-forward "<nav class=\"page\">" nil t)
           (let (p3 p4 )
             (search-backward "<")
             (setq p3 (point))
             (sgml-skip-tag-forward 1)
             (setq p4 (point))
             (delete-region p3 p4)
             (insert pageNavStr)
             )
         (progn
           (search-forward "<script><!--
google_ad_client")
           (progn
             (search-backward "<script>")
             (insert pageNavStr "\n\n")
             ) ) )

       )
     fileList)
    ))

