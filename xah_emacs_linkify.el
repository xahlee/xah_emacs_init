;;-*- coding: utf-8 -*-
;; Xah Lee's personal functions for transforming cursor location's text into HTML links.
;; 2007-10, 2011-05-29
;; ∑ http://xahlee.org/



(defun image-linkify ()
  "Replace a image file's path under cursor with a HTML img tag,
If there's a text selection, use that as path.

For example, if cursor is on the string
i/cat.png
then it will became
<img src=\"i/cat.png\" alt=\"cat\" width=\"707\" height=\"517\" />

Image path can be a URL or local file.  Supported file suffix are {.gif, .png, .svg}. If it is URL (starting with “http”), then no “width” and “height” attribute will be added."
  (interactive)
  (let* ( 
         (bds (get-selection-or-unit 'filepath))
         (ξinputPath (elt bds 0) )
         (p1 (aref bds 1) )
         (p2 (aref bds 2) )
         (ξcurrentDir (file-name-directory (or (buffer-file-name) default-directory )))
         (ξffp (expand-file-name (xahsite-web-path-to-filepath ξinputPath) ξcurrentDir ) ) ;full path
          ;; (setq ξffp (windows-style-path-to-unix (local-url-to-file-path ξffp)))
         ξwidthHeight ξwidth ξheight altText
         )

;; (message "ooo %s" ξffp)

    (setq altText (file-name-sans-extension (file-name-nondirectory ξffp)))
    (setq altText (replace-regexp-in-string "_" " " altText t t))
    (setq altText (replace-regexp-in-string "-s$" "" altText))

    (if (xahsite-is-link-to-xahsite-p (file-relative-name ξffp (or (buffer-file-name) default-directory) ))
        (progn
          (if (file-exists-p ξffp)
              (progn
                (setq ξwidthHeight
                      (cond
                       ((string-match "\.svg$" ξffp) (get-image-dimensions ξffp))
                       (t (get-image-dimensions-imk ξffp)) ) )
                (setq ξwidth (number-to-string (elt ξwidthHeight 0)))
                (setq ξheight (number-to-string (elt ξwidthHeight 1)))
                (delete-region p1 p2)
                (insert 
                 (format "<img src=\"%s\" alt=\"%s\" width=\"%s\" height=\"%s\" />"
                         (xahsite-filepath-to-href-value ξffp (or (buffer-file-name) default-directory))
                         altText
                         ξwidth ξheight
                         ))
                )
            (error "File does not exist 「%s」" ξffp )) )
      (progn
        (delete-region p1 p2)
        (insert "<img src=\"" ξffp "\" alt=\"" altText "\">") )
      ) ))

(defun image-file-to-html-figure-tag ()
  "Replace a image file's path under cursor with a HTML img tag,
and wrap it with “figure” and “figcaption” tags.

Example, if cursor is on the word “i/cat.png”, then it will became

<figure>
<img src=\"cat.png\" alt=\"cat\" width=\"707\" height=\"517\" />
<figcaption>▮</figcaption>
</figure>

If there's a text selection, use that as image path.

This function calls `image-linkify' to do its work."
  (interactive)
  (let (myStr)
    (image-linkify)
    (search-backward "<")
    (insert "<figure>\n")
    (search-forward ">")
    (insert "
<figcaption>
</figcaption>
</figure>
")
    (search-backward "</figcaption>")
    (backward-char)
    ))

(defun full-size-img-linkify ()
  "Make image file path at cursor point into a img link.

Example:
i/goddess.jpg
becomes
<a class=\"big-i\" href=\"i/goddess.jpg\" title=\"622×800\" target=\"_blank\">❐</a>

If there's a text selection, use that region as file name."
  (interactive)
  (let
      (bds p3 p4 inputStr imgPath
           ;; imgFileName linkText
           ξdimension ξwidth ξheight resultStr)

    (setq bds (get-selection-or-unit 'filepath))
    (setq inputStr (elt bds 0) p3 (elt bds 1) p4 (elt bds 2) )

    (setq imgPath (local-url-to-file-path inputStr))
    ;; (setq imgPath (windows-style-path-to-unix imgPath))

    ;; (message "ttt is : %s" imgPath)

    ;; (setq imgFileName (file-name-nondirectory imgPath))
    ;; (setq linkText
    ;;           (if (< (length imgFileName) 20)
    ;;               imgFileName
    ;;             (concat  (substring imgFileName 0 5)  "…" (substring imgFileName -6)  ) ))


    (setq ξdimension (get-image-dimensions-imk imgPath))
    (setq ξwidth (number-to-string (elt ξdimension 0)))
    (setq ξheight (number-to-string (elt ξdimension 1)))
    (setq resultStr
          (concat "<a class=\"big-i\" href=\"" (file-relative-name imgPath) "\" target=\"_blank\">" ξwidth "×" ξheight "</a>")
)

    (delete-region p3 p4)
    (insert resultStr)))

(defvar ξurl-encode-chars-pairs nil "A list of pairs of chars that needs to be percent encoded. WARNING: a hack. Incomplete.")
(setq ξurl-encode-chars-pairs
[
                              ["'" "%27"]
                              ["(" "%28"]
                              [")" "%29"]
                              ["–" "%E2%80%93"]
                              ["&" "&amp;"]
                              ["," "%2C"]
                              ["\"" "%22"]
                              ]
 )

(defun url-percent-encode-string (ξstring)
  "Returns URL percent-encoded
Example:
 http://en.wikipedia.org/wiki/Python_(programming_language)
⇒
 http://en.wikipedia.org/wiki/Python_%28programming_language%29
WARNING: the encoding is incomplete.
See also: `url-percent-decode-string'."
(progn
    (replace-pairs-in-string ξstring ξurl-encode-chars-pairs)
    ))

(defun url-percent-decode-string (ξstring)
  "Decode URL percent-encoded string.
e.g. 「%28」 ⇒ 「'」.
WARNING: the decoding is incomplete.
See also: `url-percent-encode-string'."
  (replace-pairs-in-string ξstring (mapcar (lambda (ξx) (vector (elt ξx 1) (elt ξx 0))) ξurl-encode-chars-pairs) ))

(defun wikipedia-url-linkify (ξstring &optional ξfrom-to-pair)
  "Make the URL at cursor point into a html link.

If there is a text selection, use that as input.

Example:
http://en.wikipedia.org/wiki/Emacs
⇒
<a href=\"http://en.wikipedia.org/wiki/Emacs\">Emacs</a>.

When called interactively, work on current URL or text selection.

When called in lisp code, if ξstring is non-nil, returns a changed string.  If ξstring nil, change the text in the region between positions in sequence ξfrom-to-pair."

  (interactive
   (if (region-active-p)
       (list nil (vector (region-beginning) (region-end)))
     (let ((bds (get-selection-or-unit 'url)) )
       (list nil (vector (aref bds 1) (aref bds 2))) ) ) )

  (let (workOnStringP inputStr outputStr
                      (ξfrom (elt ξfrom-to-pair 0))
                      (ξto (elt ξfrom-to-pair 1)))
    (setq workOnStringP (if () t nil))
    (setq inputStr (if workOnStringP ξstring (buffer-substring-no-properties ξfrom ξto)))

    (setq outputStr (format "<a href=\"%s\">%s</a>" (url-percent-encode-string inputStr) (replace-regexp-in-string "_" " " (url-percent-decode-string (file-name-nondirectory inputStr) ) )) )

    (if workOnStringP
        outputStr
      (progn
        (delete-region ξfrom ξto)
        (goto-char ξfrom)
        (insert outputStr) )) ) )

(defun wrap-url (ξstring &optional ξfrom ξto)
  "Make the URL at cursor point into a html link.

When called interactively, work on current glyph sequence or text selection.

When called in lisp code, if ξstring is non-nil, returns a changed string.  If ξstring nil, change the text in the region between positions ξfrom ξto."
  (interactive
   (if (region-active-p)
       (list nil (region-beginning) (region-end))
     (let ((bds (unit-at-cursor 'glyphs)) )
       (list nil (elt bds 1) (elt bds 2)) ) ) )

  (let (workOnStringP inputStr outputStr)
    (setq workOnStringP (if ξstring t nil))
    (setq inputStr (if workOnStringP ξstring (buffer-substring-no-properties ξfrom ξto)))
    (setq outputStr (concat "<a href=\"" (url-percent-encode-string inputStr) "\">" inputStr "</a>" )  )

    (if workOnStringP
        outputStr
      (save-excursion
        (delete-region ξfrom ξto)
        (goto-char ξfrom)
        (insert outputStr) )) )
  )

(defun blogger-linkify ()
  "Make URL at cursor point into a html link.

Example: http://xahlee.blogspot.com/2010/03/some.html
becomes

<div class=\"blgcmt\"><a href=\"http://xahlee.blogspot.com/2010/03/some.html\">✍</a></div>"
  (interactive)
  (let (bds p7 p8 ξurl)
    (setq bds (get-selection-or-unit 'url))
    (setq ξurl (elt bds 0) )
    (setq p7 (elt bds 1) )
    (setq p8 (elt bds 2) )

    (delete-region p7 p8)
    (insert (concat "<div class=\"blgcmt\"><a href=\"" (url-percent-encode-string ξurl) "\">✍</a></div>"))))

(defun source-linkify (prefixArgCode)
  "Make URL at cursor point into a html link.
If there's a text selection, use the text selection as input.

Example: http://example.com/xyz.htm
becomes
<a class=\"sorc\" href=\"http://example.com/xyz.htm\" data-accessed=\"2008-12-25\">example.com…</a>

The anchor text may be of 4 possibilities, depending on value of `universal-argument'.

1 → 「‹full url›」
2 or 4 → 「‹domain›…」
3 → 「img src」
0 or any → smartly decide."

  (interactive "P")
  (let (inputStr
        bds p1-input p2-input
        p1-url p2-url p1-tag p2-tag
        ξurl domainName linkText resultLinkStr)

    (setq bds (get-selection-or-unit 'url))
    (setq inputStr (elt bds 0) )
    (setq p1-input (elt bds 1) )
    (setq p2-input (elt bds 2) )

    ;; check if it's just plain URL or already in linked form 「<a href=…>…</a>」
    ;; If latter, you need to get the boundaries for the entire link too.
    (if (string-match "href=\"" inputStr)
        (save-excursion
          (search-backward "href=" (- (point) 90)) ; search boundary as extra guard for error
          (forward-char 6)
          (setq p1-url (point))
          (search-forward "\"" (+ p1-url 90))
          (setq p2-url (- (point) 1))

          (goto-char p1-url)
          (search-backward "<a" (- p1-url 30) )
          (setq p1-tag (point))
          (goto-char p2-url)
          (search-forward "</a>" (+ p2-url 140))
          (setq p2-tag (point))
          )
      (progn
        (setq p1-url p1-input)
        (setq p2-url p2-input)
        (setq p1-tag p1-input)
        (setq p2-tag p2-input) ) )

    (setq ξurl (replace-regexp-in-string "&amp;" "&" (buffer-substring-no-properties p1-url p2-url) nil "LITERAL") ) ; in case it's already encoded. TODO this is only 99% correct.

    ;; get the domainName
    (setq domainName
          (progn
            (string-match "://\\([^\/]+?\\)/" ξurl)
            (match-string 1 ξurl)
            )
          )

    (setq linkText
          (cond
           ((equal prefixArgCode 1) ξurl)           ; full url
           ((or (equal prefixArgCode 2) (equal prefixArgCode 4) (equal prefixArgCode '(4))) (concat domainName "…"))           ; ‹domain›…
           ((equal prefixArgCode 3) "img src")           ; img src
           (t (if
                  (or
                   (string-match "wikipedia\\.org.+jpg$" ξurl)
                   (string-match "wikipedia\\.org.+JPG$" ξurl)
                   (string-match "wikipedia\\.org.+png$" ξurl)
                   (string-match "wikipedia\\.org.+PNG$" ξurl)
                   (string-match "wikipedia\\.org.+svg$" ξurl)
                   (string-match "wikipedia\\.org.+SVG$" ξurl)
                   )
                  "img src"
                ξurl
                ))        ; smart
           )
          )

    (setq ξurl (replace-regexp-in-string "&" "&amp;" ξurl))
    (setq resultLinkStr
          (format "<a class=\"sorc\" href=\"%s\" data-accessed=\"%s\">%s</a>"
                  ξurl (format-time-string "%Y-%m-%d") linkText
                  )
          )

    ;; delete URL and insert the link
    (delete-region p1-tag p2-tag)
    (insert resultLinkStr)
    ))


(defun defunct-link ()
  "Make the html link under cursor to a defunct form.
Example:
If cursor is inside this tag
<a class=\"sorc\" href=\"http://example.com/\" data-accessed=\"2008-12-26\">…</a>
 (and inside the opening tag.)

It becomes:
<s class=\"deadurl\" title=\"accessed:2008-12-26; defunct:2008-12-26; http://example.com\">…</s>"
  (interactive)
  (let (p1 p2 wholeLinkStr newLinkStr ξurl titleStr)
    (save-excursion
      ;; get the boundary of opening tag
      (forward-char 3)
      (search-backward "<a " ) (setq p1 (point) )
      (search-forward "</a>") (setq p2 (point) )

      ;; get wholeLinkStr
      (setq wholeLinkStr (buffer-substring-no-properties p1 p2))

      ;; generate replacement text
      (with-temp-buffer
        (insert wholeLinkStr)

        (goto-char 1)
        (search-forward-regexp  "href=\"\\([^\"]+?\\)\"")
        (setq ξurl (match-string 1))

        (search-forward-regexp  "data-accessed=\"\\([^\"]+?\\)\"")
        (setq titleStr (match-string 1))

        (setq newLinkStr (format "<s class=\"deadurl\" title=\"accessed:%s; defunct:%s\">%s</s>" titleStr (format-time-string "%Y-%m-%d") ξurl ) )))

    (delete-region p1 p2)
    (insert newLinkStr)))

(defun wikipedia-linkify ()
  "Make the current word or text selection into a Wikipedia link.

For Example: 「Emacs」 ⇒ 「<a href=\"http://en.wikipedia.org/wiki/Emacs\">Emacs</a>」"
  (interactive)
  (let (linkText bds p1 p2 wikiTerm resultStr)

    (setq bds (get-selection-or-unit 'url))
    (setq linkText (elt bds 0) )
    (setq p1 (aref bds 1) )
    (setq p2 (aref bds 2) )

    (setq wikiTerm (replace-regexp-in-string " " "_" linkText) )
    (setq resultStr (concat "<a href=\"http://en.wikipedia.org/wiki/" wikiTerm "\">" linkText "</a>"))

    (delete-region p1 p2)
    (insert resultStr) ))

(defun chinese-linkify ()
  "Make the current Chinese character into several Chinese dictionary links.
If there's a text selection, use that for input."
  (interactive)
  (let ( ξchar p1 p2 big5Code templateStr resultStr)

    (if (region-active-p)
        (progn
          (setq p1 (region-beginning) )
          (setq p2 (region-end) )
          )
      (progn
        (setq p1 (point) )
        (setq p2 (1+ (point)) ) ) )

    (setq ξchar (buffer-substring-no-properties p1 p2))

    ;; (setq big5Code (encode-char (string-to-char ξchar) 'big5) )

    (setq templateStr
          "<div class=\"cδ\"><b class=\"w\">�</b> <span class=\"en\"><a href=\"http://translate.google.com/#zh-CN|en|�\">Translate</a> ◇ <a href=\"http://en.wiktionary.org/wiki/�\">Wiktionary</a> ◇ <a href=\"http://www.chineseetymology.org/CharacterEtymology.aspx?submitButton1=Etymology&amp;characterInput=�\">history</a></span></div>"
          )

    (setq resultStr (replace-regexp-in-string "�" ξchar templateStr))
    (delete-region p1 p2)
    (insert resultStr) ))

(defun word-etymology-linkify ()
  "Make the current word into a etymology reference link.
."
  (interactive)
  (let ( bds p1 p2 inputstr resultStr)

    (setq bds (get-selection-or-unit 'line))
    (setq inputstr (elt bds 0) p1 (elt bds 1) p2 (elt bds 2)  )
    (setq resultStr (concat "<span class=\"cδe\"><a href=\"http://www.etymonline.com/index.php?search=" inputstr "\">" inputstr "</a></span>") )
    (delete-region p1 p2)
    (insert resultStr) ))




(defun youporn-search-linkify ()
  "Make the current line into a YouPorn.com link.
For example, if the cursor is on the line:
anal
Then it'll become
\(YouPorn video: <a href=\"http://www.youporn.com/search?query=anal\">anal</a>\)"
  (interactive)
  (let (bds p1 p2 ξword ξurl)

    (setq bds (get-selection-or-unit 'line))
    (setq ξword (elt bds 0) )
    (setq p1 (aref bds 1) )
    (setq p2 (aref bds 2) )

    (setq ξurl (concat "http://www.youporn.com/search?query=" ξword) )
    (setq ξurl (replace-regexp-in-string " " "+" ξurl ) )
    (delete-region p1 p2)
    (insert "(YouPorn video: <a href=\"" ξurl "\">" ξword "</a>)\n")))

(defun youtube-search-linkify ()
  "Make the current line into a YouTube link.
If there's a text selection, use that.
For example, if the cursor is on the line:
David Bowie
Then it'll become
<a class=\"utb\" href=\"http://youtube.com/results?search_query=David+Bowie&amp;search=Search\">David Bowie</a>

Warning: the line must end in a line return char else the result is wrong.

Note: old version returns this form:
<span class=\"utb\"><a href=\"http://youtube.com/results?search_query=David+Bowie&amp;search=Search\">David Bowie</a></span>
"
  (interactive)
  (let (bds p1 p2 ξword ξurl)

    (setq bds (get-selection-or-unit 'line))
    (setq ξword (elt bds 0) )
    (setq p1 (aref bds 1) )
    (setq p2 (aref bds 2) )

    (setq ξurl (concat "http://youtube.com/results?search_query=" ξword "&amp;search=Search") )
    (setq ξurl (replace-regexp-in-string " " "+" ξurl ) )
    (setq ξurl (replace-regexp-in-string "," "%2C" ξurl ) )
    (delete-region p1 p2)
    (insert "<a class=\"utb\" href=\"" ξurl "\">" ξword "</a>")))

(defun video-search-string (searchString)
  "Return a Google video search string URL of SEARCHSTRING.

Example:
 「(video-search-string \"White Rabbit, Jefferson Airplane\")」 ⇒
 「http://www.google.com/search?tbs=vid%3A1&q=White+Rabbit%2C+Jefferson+Airplane」

This command is called by `video-search-linkify'."
  (let (strEncoded)
    (setq strEncoded searchString )
    (setq strEncoded (replace-regexp-in-string " " "+" strEncoded ) )
    (setq strEncoded (url-percent-encode-string strEncoded ) )
    (concat "http://www.google.com/search?tbs=vid%3A1&q=" strEncoded)
    ))

(defun video-search-linkify ()
  "Make the current line into a Google video search link.
If there's a text selection, use that.
For example, if the cursor is on the line:

White Rabbit, Jefferson Airplane

Then it'll become

<a class=\"gvidsr\" href=\"http://www.google.com/search?tbs=vid%3A1&q=White+Rabbit%2C+Jefferson+Airplane\">White Rabbit, Jefferson Airplane</a>

Warning: the line must end in a line return char else the result is wrong.

This command calls `video-search-string'"
  (interactive)
  (let (bds p1 p2 ξword ξurl)

    (setq bds (get-selection-or-unit 'line))
    (setq ξword (elt bds 0) )
    (setq p1 (aref bds 1) )
    (setq p2 (aref bds 2) )

    (setq ξurl (video-search-string ξword) )
    (delete-region p1 p2)
    (insert "<a class=\"gvidsr\" href=\"" ξurl "\">" ξword "</a>")))

(defun google-search-linkify ()
  "Make the current line into a Google search link.
For example, if the cursor is on the line:

emacs lisp

Then it'll become

<p>Google search: <a href=\"http://www.google.com/search?q=emacs+lisp\">emacs lisp</a>.</p>

Warning: the line must end in a line return char else the result is wrong."
  (interactive)
  (let (bds p1 p2 ξword ξurl)

    (setq bds (get-selection-or-unit 'line))
    (setq ξword (elt bds 0) )
    (setq p1 (aref bds 1) )
    (setq p2 (aref bds 2) )

    (setq ξurl (concat "http://www.google.com/search?q=" ξword))
    (setq ξurl (replace-regexp-in-string " " "+" ξurl ) )
    (delete-region p1 p2)
    (insert "<p>Google search: <a href=\"" ξurl "\">" ξword "</a>.</p>\n")))



;; some custom html markup and functions for working with html

(defun nks-linkify ()
  "Make the current word into into a link to Wolfram Science site.
For Example, if you cursor is on the word “p123”, then
it becomes
“<a href=\"http://www.wolframscience.com/nksonline/page-123\">p123</a>”"
  (interactive)
  (let (bds p1 p2 inputStr pagenum myresult)

    (setq bds (get-selection-or-unit 'glyphs))
    (setq inputStr (elt bds 0) )
    (setq p1 (aref bds 1) )
    (setq p2 (aref bds 2) )

    (setq pagenum (substring inputStr 1) )
    (setq myresult
          (concat
           "<a href=\"http://www.wolframscience.com/nksonline/page-"
           pagenum "\">p" pagenum "</a>"))

    (delete-region p1 p2)
    (insert myresult)
    ))

(defun listify-block ()
  "Make the current block of lines into a HTML list.
Any URL in the line will be turned into links.

Example:
If your cursor is in the following block of text:

Castratos are castrated males made for singing: http://en.wikipedia.org/wiki/Castrato , record of the last castrato: http://www.archive.org/details/AlessandroMoreschi
human vocal range: http://en.wikipedia.org/wiki/Vocal_range

It will become:
<ul>
<li>Castratos are castrated males made for singing: <a href=\"http://en.wikipedia.org/wiki/Castrato\">Castrato</a> , record of the last castrato: <a href=\"http://www.archive.org/details/AlessandroMoreschi\">http://www.archive.org/details/AlessandroMoreschi</a></li>
<li>human vocal range: <a href=\"http://en.wikipedia.org/wiki/Vocal_range\">Vocal range</a></li>
</ul>"
  (interactive)
  (let (bds p1 p2 inputStr resultStr)
    (setq bds (get-selection-or-unit 'block))
    (setq inputStr (elt bds 0) p1 (elt bds 1) p2 (elt bds 2)  )
    (save-excursion
      (setq resultStr
            (with-temp-buffer
              (insert inputStr)
              (delete-trailing-whitespace)
              (goto-char 1)
              (while
                  (search-forward-regexp  "\.html$" nil t)
                (backward-char 1)
                (xah-all-linkify)
                )

              (goto-char 1)
              (while
                  (not (equal (line-end-position) (point-max)))
                (beginning-of-line) (insert "<li>")
                (end-of-line) (insert "</li>")
                (forward-line 1 )
                )

              (beginning-of-line) (insert "<li>")
              (end-of-line) (insert "</li>")

              (goto-char 1)
              (insert "<ul>\n")
              (goto-char (point-max))
              (insert "\n</ul>")

              (buffer-string)
              ) )
      )
    (delete-region p1 p2)
    (insert resultStr)
    ) )

(defun listify-block_old_2011-02-01 ()
  "Make the current block of lines into a HTML list.
Any URL in the line will be turned into links.

Example:
If your cursor is in the following block of text:

Castratos are castrated males made for singing: http://en.wikipedia.org/wiki/Castrato , record of the last castrato: http://www.archive.org/details/AlessandroMoreschi
human vocal range: http://en.wikipedia.org/wiki/Vocal_range

It will become:
<ul>
<li>Castratos are castrated males made for singing: <a href=\"http://en.wikipedia.org/wiki/Castrato\">Castrato</a> , record of the last castrato: <a href=\"http://www.archive.org/details/AlessandroMoreschi\">http://www.archive.org/details/AlessandroMoreschi</a></li>
<li>human vocal range: <a href=\"http://en.wikipedia.org/wiki/Vocal_range\">Vocal range</a></li>
</ul>"
  (interactive)
  (let (p1 p2 mainText lines linkify transform-line)
    (progn
      (search-backward "\n\n")
      (setq p1 (search-forward "\n\n"))
      (search-forward "\n\n")
      (setq p2 (search-backward "\n\n"))
      (setq mainText (buffer-substring-no-properties p1 p2))

      (setq lines (split-string mainText "\n" ))
      (setq linkify (lambda (x) (when (string-match "\\`http" x) (wrap-url x))))
      (setq transform-line
            (lambda (line) (mapconcat 'identity (mapcar linkify (split-string line " ")) " ")) )
      )
    (delete-region p1 p2)
    (insert "<ul>\n")
    (insert
     (mapconcat (lambda (x) (concat "<li>" x "</li>"))
                (mapcar transform-line lines) "\n"))
    (insert "\n</ul>")))




;; more specific to Xah Lee

(defun amazon-search-linkify-url (sString productCat assid)
  "Returns a URL of amazon search based on search string and product category.
sString is the search string. e.g. “deep throat”
productCat is a short code for amazon's product category.
See `amazon-search-linkify' for the possible code string.
Sample call:
 (amazon-search-linkify-url \"debbie does dollas\" \"dvd\" \"xahh-20\")"
  (interactive)
  (let (sStrPercent)
    (setq sStrPercent sString)
    (setq sStrPercent (replace-regexp-in-string " " "%20" sStrPercent) )
    (setq sStrPercent (replace-regexp-in-string "," "%2c" sStrPercent) )

    (concat
     "<a class=\"amzs\" href=\"http://www.amazon.com/gp/search?ie=UTF8&amp;keywords="
     sStrPercent
     "&amp;tag="
     assid
     "&amp;index="
     productCat
     "&amp;linkCode=ur2&amp;camp=1789&amp;creative=9325\">"
     sString
     "</a>"
     ) ) )


(defun amazon-search-linkify ()
  "Make the current line or text-selection into a Amazon product search link.
The current line must have this format:
search word;code
The “search word” is any letter and space.
the “code” is one of the following:
a = “blended” = all categories.
d = “dvd” = movies and tv.
b = “books”
c = “classical” = classical music
p = “pc-hardware”
e = “electronics”
m = “music”
s = “software”
There are other amazon categories, but not supported by this function."
  (interactive)
  (let (p1 p2 mainText tmplist sstr pcato pcc)
    (if (region-active-p)
        (setq p1 (region-beginning) p2 (region-end))
      (progn
        (setq p1 (line-beginning-position) )
        (setq p2 (line-end-position) )
        ))
    ;; get the text
    (setq mainText (buffer-substring-no-properties p1 p2) )
    (setq tmplist (split-string mainText ";") )
    (setq sstr (nth 0 tmplist ) )
    (setq pcato (nth 1 tmplist ) )
    (message "%s , %s" sstr pcato)

    (cond
     ((string= pcato "a") (setq pcc "blended"))
     ((string= pcato "d") (setq pcc "dvd"))
     ((string= pcato "b") (setq pcc "books"))
     ((string= pcato "c") (setq pcc "classical"))
     ((string= pcato "p") (setq pcc "pc-hardware"))
     ((string= pcato "e") (setq pcc "electronics"))
     ((string= pcato "m") (setq pcc "music"))
     ((string= pcato "s") (setq pcc "software"))
     (t (error "Code does not match"))
     )

    (delete-region p1 p2)
    (insert  (amazon-search-linkify-url sstr pcc "xahh-20"))
    ))


(defun amazon-linkify ()
  "Make the current URL or text selection into a Amazon.com link.

Examples of Amazon product URL formats
http://www.amazon.com/Cyborg-R-T-Gaming-Mouse/dp/B003CP0BHM/ref=pd_sim_e_1
http://www.amazon.com/gp/product/B003CP0BHM
http://www.amazon.com/exec/obidos/ASIN/B003CP0BHM/xahh-20
http://www.amazon.com/exec/obidos/tg/detail/-/B003CP0BHM/
http://www.amazon.com/dp/B003CP0BHM?tag=xahhome-20

Example output:
<a class=\"amz\" href=\"http://www.amazon.com/dp/B003CP0BHM/?tag=xahh-20\" title=\"Cyborg R T Gaming Mouse\">amazon</a>

For info about the Amazon ID in URL, see: URL `http://en.wikipedia.org/wiki/Amazon_Standard_Identification_Number'"
  (interactive)
  (let (bds p1 p2 mainText asin productName )

    (setq bds (get-selection-or-unit 'url))
    (setq mainText (elt bds 0) )
    (setq p1 (aref bds 1) )
    (setq p2 (aref bds 2) )

    ;; extract the id from text
    (cond
     ((string-match "/dp/\\([[:alnum:]]\\{10\\}\\)/" mainText) (setq asin (match-string 1 mainText) ))
     ((string-match "/dp/\\([[:alnum:]]\\{10\\}\\)\\?tag=" mainText) (setq asin (match-string 1 mainText) ))
     ((string-match "/gp/product/\\([[:alnum:]]\\{10\\}\\)" mainText) (setq asin (match-string 1 mainText) ))
     ((string-match "/ASIN/\\([[:alnum:]]\\{10\\}\\)" mainText) (setq asin (match-string 1 mainText) ))
     ((string-match "/tg/detail/-/\\([[:alnum:]]\\{10\\}\\)/" mainText) (setq asin (match-string 1 mainText) ))
     ((and
       (equal 10 (length mainText ) )
       (string-match "\\`\\([[:alnum:]]\\{10\\}\\)\\'" mainText)
       )
      (setq asin mainText ))
     (t (error "no amazon ASIN found"))
     )

    ;; extract the product name from URL, if any
    (cond
     ((string-match "amazon\.com/\\([^/]+?\\)/dp/" mainText) (setq productName (match-string 1 mainText) ))
     (t (setq productName "") (message "no product name found" ) (ding))
     )

    ;; replace dash to space in productName
    (setq productName (replace-regexp-in-string "-" " " productName) )

    (delete-region p1 p2)
    (insert
     "<a class=\"amz\" href=\"http://www.amazon.com/dp/"
     asin "/?tag=xahh-20\" title=\"" productName "\">amazon</a>")
    (search-backward "\">")
    ))

;; (defun local-linkify ()
;; "Make the path under cursor into a local link.\n
;; For Example, if you cursor is on the text “../emacs/emacs.html”,
;; then it'll become:
;; “<a href=\"../emacs/emacs.html\">Xah's Emacs Tutorial</a>”.
;; The link text is pulled from the file's <h1> tag.

;; If a region is active, use the region as file path."
;;  (interactive)
;;  (let (myPath bounds tempBuff x1 x2 titleText resultStr)
;;    (setq myPath
;;          (if (region-active-p)
;;              (buffer-substring-no-properties (region-beginning) (region-end))
;;            (thing-at-point 'filename)
;;            ))
;;    (setq bounds (bounds-of-thing-at-point 'filename))

;;    (setq tempBuff (generate-new-buffer-name " temp"))

;;    (when (file-exists-p myPath)
;;        (progn
;;          (save-current-buffer
;;            (message myPath)
;;            (set-buffer (get-buffer-create tempBuff))
;;            (goto-char (point-min))
;;            (insert-file-contents myPath nil nil nil t)
;;            (setq x1 (search-forward "<title>"))
;;            (search-forward "</title>")
;;            (setq x2 (search-backward "<"))
;;            (setq titleText (buffer-substring-no-properties x1 x2))
;;            (kill-buffer tempBuff))

;;          (setq resultStr (concat "<a href=\"" myPath "\">" titleText "</a>"))
;;          (save-excursion
;;            (delete-region (car bounds) (cdr bounds))
;;            (insert resultStr))))
;;    ))


(defun xah-file-linkify ()
  "Make the path under cursor into a HTML link for xah site.

For Example, if you cursor is on the text “../emacs/emacs.html”,
then it'll become:
“<a href=\"../emacs/emacs.html\">Xah's Emacs Tutorial</a>”.
The link text is pulled from the file's <h1> tag.

If there is text selection, use it as file path.

The file path can also be a full path or URL, See: `xahsite-web-path-to-filepath'.
"
  (interactive)
  (let* (
         (bds (get-selection-or-unit 'glyphs))
         (inputStr (elt bds 0) )
         (p1 (aref bds 1) )
         (p2 (aref bds 2) )
         fPath rltvPath titleText resultStr
         (currentBufferFilePathOrDir (or (buffer-file-name) default-directory))
         (currentBufferFileDir (file-name-directory (or (buffer-file-name) default-directory)))
         )

    (setq fPath (xahsite-web-path-to-filepath inputStr default-directory) )

    (if (file-exists-p fPath)
        (progn
          (setq titleText
                (if (string-match-p ".+html\\'" fPath)
                    (get-html-file-title fPath)
                  (file-name-nondirectory fPath)))
          (setq resultStr
                (if (string-equal
                     (xahsite-get-domain-of-local-file-path currentBufferFilePathOrDir)
                     (xahsite-get-domain-of-local-file-path fPath)
                     )
                    (progn
                      (setq rltvPath (file-relative-name fPath currentBufferFileDir))
                      (format "<a href=\"%s\">%s</a>" rltvPath titleText))
                  (progn
                    (format "<a href=\"%s\">%s</a>" (xahsite-filepath-to-url fPath) titleText)) ) )
          (delete-region p1 p2)
          (insert resultStr)
          )
      (progn (message (format "Cannot locate the file: 「%s」" fPath) )) ) ) )

(defun xah-curve-linkify ()
  "Make the current word or text selection into a html link.

This function works on Xah Lee's website only.
 Example:
 “parabola” becomes
“<a href=\"../Parabola_dir/parabola.html\">parabola</a>”.

The directory to search includes:
“SpecialPlaneCurves_dir” and “surface”."
  (interactive)
  (let (bds p1 p2 cursorWord wordPath ξi testPaths ξfound-p rPath linkWord)

    (setq bds (get-selection-or-unit 'glyphs))
    (setq cursorWord (elt bds 0) )
    (setq p1 (aref bds 1) )
    (setq p2 (aref bds 2) )

    ;; word for constructing possible dir
    (setq wordPath (replace-regexp-in-string " " "_" (downcase cursorWord)))

    ;; the paths to test
    (setq testPaths
          (vector
           (concat "~/web/xahlee_org/SpecialPlaneCurves_dir/" (upcase-initials wordPath) "_dir/" wordPath ".html")
           (concat "~/web/xahlee_org/surface/" wordPath "/" wordPath ".html")))

    ;; loop thru the paths until a file is found
    (setq ξfound-p nil)
    (setq ξi 0)
    (while (and (not ξfound-p) (< ξi (length testPaths)))
      (setq rPath (elt testPaths ξi))
      (setq ξfound-p (file-exists-p rPath))
      (setq ξi (1+ ξi)))

    (if ξfound-p
        (progn
          (setq linkWord (replace-regexp-in-string "_" " " cursorWord))
          (delete-region p1 p2)
          (insert (concat "<a href=\"" (file-relative-name rPath) "\">" linkWord "</a>")))
      (progn (beep) (message "No file found")))))

(defun xah-all-linkify ()
  "Make the text under cursor into a HTML link for xah's sites.

text can be any of:
• relative path (file, image, or anything)
• Wikipedia link
• any URL

They will be changed into a html link in various formats, depending on the input.

If there is text selection, use it as input."
  (interactive)
  (let* ((myPath (elt (get-selection-or-unit 'glyphs ) 0) ))
    (cond
     ((and (string-match-p "\\`http://xahlee\.blogspot\.com/" myPath)) (blogger-linkify))
     ((and (string-match-p "\\`http://wordy-english\.blogspot\.com/" myPath)) (blogger-linkify))
     ((and (string-match-p "www\.amazon\.com/" myPath)) (amazon-linkify))
     ((and (string-match-p "www\.youtube\.com/" myPath)) (youtube-linkify))
     ((xahsite-url-is-xah-website-p myPath) (xah-file-linkify))
     ((string-match-p "wikipedia.org/" myPath)
      (let ((case-fold-search nil))
        (if (path-ends-in-image-suffix-p myPath)
            (source-linkify 0)
          (call-interactively 'wikipedia-url-linkify) ) ) )

     ((and (string-match-p "\\`https?://" myPath)) (source-linkify 0)) ; generic URL

     ((path-ends-in-image-suffix-p myPath) (image-file-to-html-figure-tag))

     (t (xah-file-linkify))
     ) ))
