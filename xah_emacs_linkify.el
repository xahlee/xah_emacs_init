;;-*- coding: utf-8 -*-
;; Xah Lee's personal functions for transforming cursor location's text into HTML links.
;; 2007-10, 2011-05-29
;; ∑ http://xahlee.org/



(defun xah-html-image-linkify ()
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
         (ξinputPath (elt bds 0))
         (p1 (aref bds 1))
         (p2 (aref bds 2))
         (ξcurrentDir (file-name-directory (or (buffer-file-name) default-directory )))
         (ξfp (expand-file-name (xahsite-web-path-to-filepath ξinputPath) ξcurrentDir )) ;full path
         ;; (setq ξfp (windows-style-path-to-unix (local-url-to-file-path ξfp)))
         altText
         )

    ;; (message "ooo %s" ξfp)

    (setq altText (file-name-sans-extension (file-name-nondirectory ξfp)))
    (setq altText (replace-regexp-in-string "_" " " altText t t))
    (setq altText (replace-regexp-in-string "-s$" "" altText))

    (if (xahsite-is-link-to-xahsite-p (file-relative-name ξfp (or (buffer-file-name) default-directory)))
        (progn
          (if (file-exists-p ξfp)
              (let (ξwh ξw ξh ξwhStr)
                (setq ξwh
                      (cond
                       ((string-match "\.svg$" ξfp) (get-image-dimensions ξfp))

                       (t (get-image-dimensions ξfp))
                       ;; (t (get-image-dimensions-imk ξfp))
                       ))
                (setq ξw (number-to-string (elt ξwh 0)))
                (setq ξh (number-to-string (elt ξwh 1)))
                (setq ξwhStr
                      (if (string-match "\.svg$" ξfp)
                          ""
                        (format "width=\"%s\" height=\"%s\"" ξw ξh)))
                (delete-region p1 p2)
                (insert
                 (format "<img src=\"%s\" alt=\"%s\" %s />"
                         (xahsite-filepath-to-href-value ξfp (or (buffer-file-name) default-directory))
                         altText ξwhStr )))
            (error "File does not exist 「%s」" ξfp )))
      (progn
        (delete-region p1 p2)
        (insert "<img src=\"" ξfp "\" alt=\"" altText "\">")))))

(defun image-file-to-html-figure-tag ()
  "Replace a image file's path under cursor with a HTML img tag,
and wrap it with “figure” and “figcaption” tags.

Example, if cursor is on the word “i/cat.png”, then it will became

<figure>
<img src=\"cat.png\" alt=\"cat\" width=\"707\" height=\"517\" />
<figcaption>▮</figcaption>
</figure>

If there's a text selection, use that as image path.

This function calls `xah-html-image-linkify' to do its work."
  (interactive)
  (let (myStr)
    (xah-html-image-linkify)
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

(defun xah-html-full-size-img-linkify ()
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

(defun url-percent-encode-string (φstring)
  "Returns URL percent-encoded
Example:
 http://en.wikipedia.org/wiki/Python_(programming_language)
⇒
 http://en.wikipedia.org/wiki/Python_%28programming_language%29
WARNING: the encoding is incomplete.
See also: `url-percent-decode-string'."
(progn
    (replace-pairs-in-string φstring ξurl-encode-chars-pairs)
    ))

(defun url-percent-decode-string (φstring)
  "Decode URL percent-encoded string.
e.g. 「%28」 ⇒ 「'」.
WARNING: the decoding is incomplete.
See also: `url-percent-encode-string'."
  (replace-pairs-in-string φstring (mapcar (lambda (ξx) (vector (elt ξx 1) (elt ξx 0))) ξurl-encode-chars-pairs) ))

(defun blogger-linkify ()
  "Make URL at cursor point into a HTML link.

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

(defun video-search-string (φsearchString)
  "Return a Google video search string URL of SEARCHSTRING.

Example:
 「(video-search-string \"White Rabbit, Jefferson Airplane\")」 ⇒
 「http://www.google.com/search?tbs=vid%3A1&q=White+Rabbit%2C+Jefferson+Airplane」

This command is called by `video-search-linkify'."
  (let (strEncoded)
    (setq strEncoded φsearchString )
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


;; some custom HTML markup and functions for working with HTML

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


;; more specific to Xah Lee

(defun amazon-search-linkify-url (φsString φproductCat φassid)
  "Returns a URL of amazon search based on search string and product category.
φsString is the search string. e.g. “deep throat”
φproductCat is a short code for amazon's product category.
See `amazon-search-linkify' for the possible code string.
Sample call:
 (amazon-search-linkify-url \"debbie does dollas\" \"dvd\" \"xahh-20\")"
  (interactive)
  (let (sStrPercent)
    (setq sStrPercent φsString)
    (setq sStrPercent (replace-regexp-in-string " " "%20" sStrPercent) )
    (setq sStrPercent (replace-regexp-in-string "," "%2c" sStrPercent) )

    (concat
     "<a class=\"amzs\" href=\"http://www.amazon.com/gp/search?ie=UTF8&amp;keywords="
     sStrPercent
     "&amp;tag="
     φassid
     "&amp;index="
     φproductCat
     "&amp;linkCode=ur2&amp;camp=1789&amp;creative=9325\">"
     φsString
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
    (if (use-region-p)
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
     ((string-match "/dp/\\([[:alnum:]]\\{10\\}\\)/?" mainText) (setq asin (match-string 1 mainText) ))
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
;;          (if (use-region-p)
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
         (bds (get-selection-or-unit 'filepath))
         (inputStr (elt bds 0) )
         (p1 (aref bds 1) )
         (p2 (aref bds 2) )
         (inputStParts (split-uri-hashmark inputStr) )
         (pt1 (aref inputStParts 0) )
         (fragPart (aref inputStParts 1) )
         (fPath (xahsite-web-path-to-filepath pt1 default-directory) )
         rltvPath titleText resultStr
         (currentBufferFilePathOrDir (expand-file-name (or (buffer-file-name) default-directory)))
         (currentBufferFileDir (file-name-directory (or (buffer-file-name) default-directory)))
         )

    (if (file-exists-p fPath)
        (progn
          (setq titleText
                (if (string-match-p ".+html\\'" fPath)
                    (concat (xhm-get-html-file-title fPath) fragPart)
                  (file-name-nondirectory fPath)))
          (setq resultStr
                (if (string-equal
                     (xahsite-get-domain-of-local-file-path currentBufferFilePathOrDir)
                     (xahsite-get-domain-of-local-file-path fPath)
                     )
                    (progn
                      (setq rltvPath (file-relative-name fPath currentBufferFileDir))
                      (format "<a href=\"%s\">%s</a>" (concat rltvPath fragPart) titleText))
                  (progn
                    (format "<a href=\"%s\">%s</a>" (concat (xahsite-filepath-to-url fPath) fragPart) titleText)) )
                )
          (delete-region p1 p2)
          (insert resultStr)
          )
      (progn (message (format "Cannot locate the file: 「%s」" fPath) )) ) ) )

(defun nodejs-get-title (φfName φfragPart)
  "Return the file frag part function title.
 (nodejs-get-title \"/home/xah/web/xahlee_info/node_api/net.html\" \"#net_server_listen_port_host_backlog_callback\" )
returns
 \"server.listen(port, [host], [backlog], [callback])\"
"
  (with-temp-buffer
    (insert-file-contents φfName nil nil nil t)
    (goto-char 1)
    (if (string= φfragPart "")
        (progn
          (search-forward "<div id=\"apicontent\">")
          (if (search-forward "<h1>" nil "NOERROR")
              (progn (buffer-substring-no-properties
                      (point)
                      (-  (search-forward "<span>") 6)) )
            (progn
              (goto-char 1)
              (buffer-substring-no-properties
               (search-forward "<title>")
               (- (search-forward "</title>") 8)) ) ) )
      (progn
        (search-forward φfragPart)
        (buffer-substring-no-properties
         (search-forward "\">")
         (-  (search-forward "</a>") 4))  )
      ) ))

(defun nodejs-ref-linkify ()
  "Make the path under cursor into a HTML link for xah site.

For Example, if you cursor is on the text “../emacs/emacs.html”,
then it'll become:
“<a href=\"../emacs/emacs.html\">Xah's Emacs Tutorial</a>”.
The link text is pulled from the file's <h1> tag.

If there is text selection, use it as file path.

The file path can also be a full path or URL, See: `xahsite-web-path-to-filepath'.

sample
file:///home/xah/web/xahlee_info/node_api/process.html#process_process_execpath

file:///home/xah/web/xahlee_info/node_api/process.html#process_process_execpath

<span class=\"ref\"><a href=\"../node_api/process.html#process_process_execpath\">Node doc process.execpath</a></span>

linkText

"
  (interactive)
  (let* (
         (bds (get-selection-or-unit 'filepath))
         (inputStr (elt bds 0) )
         (p1 (aref bds 1) )
         (p2 (aref bds 2) )
         (currentBufferFilePathOrDir (or (buffer-file-name) default-directory))
         (currentBufferFileDir (file-name-directory (or (buffer-file-name) default-directory)))

         (temp87318 (split-uri-hashmark inputStr) )
         (urlMainPart (elt temp87318 0) )
         (urlFragPart (elt temp87318 1) )
         (fPath (xahsite-web-path-to-filepath urlMainPart default-directory) )
         rltvPath titleText resultStr
         )

    (if (file-exists-p fPath)
        (progn
          (setq titleText (concat "⬢ " (nodejs-get-title fPath urlFragPart) ))
          (setq resultStr
                (if (string-equal
                     (xahsite-get-domain-of-local-file-path currentBufferFilePathOrDir)
                     (xahsite-get-domain-of-local-file-path fPath)
                     )
                    (progn
                      (setq rltvPath (file-relative-name fPath currentBufferFileDir))
                      (format "<span class=\"ref\"><a href=\"%s%s\">%s</a></span>" rltvPath urlFragPart titleText))
                  (progn
                    (format "<span class=\"ref\"><a href=\"%s%s\">%s</a></span>" (xahsite-filepath-to-url fPath) urlFragPart titleText)) ) )
          (delete-region p1 p2)
          (insert resultStr)
          )
      (progn (message (format "Cannot locate the file: 「%s」" fPath) )) ) ) )

(defun javascript-linkify ()
  "Make the path under cursor into a HTML link.
 ⁖ <script src=\"xyz.js\"></script>
"
  (interactive)
  (let* (
         (bds (get-selection-or-unit 'filepath))
         (inputStr (elt bds 0) )
         (p1 (aref bds 1) )
         (p2 (aref bds 2) )
         fPath
         )
    (setq fPath (file-relative-name inputStr) )
    (delete-region p1 p2)
    (insert (format "<script defer src=\"%s\"></script>" fPath)
            )
    ) )

(defun css-linkify ()
  "Make the path under cursor into a HTML link.
 ⁖
/home/xah/web/xahlee_org/lit.css
→
<link rel=\"stylesheet\" href=\"../lit.css\" />
"
  (interactive)
  (let* (
         (bds (get-selection-or-unit 'filepath))
         (inputStr (elt bds 0) )
         (p1 (aref bds 1) )
         (p2 (aref bds 2) )
         fPath
         )
    (setq fPath (file-relative-name inputStr) )
    (delete-region p1 p2)
    (insert (format "<link rel=\"stylesheet\" href=\"%s\" />" fPath)
            )
    ) )

(defun xah-curve-linkify ()
  "Make the current word or text selection into a HTML link.

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

They will be changed into a HTML link in various formats, depending on the input.

If there is text selection, use it as input."
  (interactive)
  (let* ((myPath (elt (get-selection-or-unit 'filepath ) 0)))
    (cond
     ((string-match-p "\\`http://xahlee\.blogspot\.com/" myPath) (blogger-linkify))
     ((string-match-p "\\`http://wordy-english\.blogspot\.com/" myPath) (blogger-linkify))
     ((string-match-p "www\.amazon\.com/" myPath) (amazon-linkify))
     ((string-match-p "www\.youtube\.com/watch" myPath) (youtube-linkify))
     ((string-match-p "/emacs_manual/" myPath) (xah-html-emacs-ref-linkify))
     ((string-match-p "/node_api/" myPath) (nodejs-ref-linkify))
     ((string-match-p "\\.js\\'" myPath) (javascript-linkify))
     ((string-match-p "\\.css\\'" myPath) (css-linkify))

     ((string-match-p "javascript_ecma-262_5.1_2011" myPath) (xah-file-linkify) (xah-ref-span-tag))
     ((string-match-p "css_transitions/CSS_Transitions.html" myPath) (xah-file-linkify) (xah-ref-span-tag))

     ((xahsite-url-is-xah-website-p myPath) (xah-file-linkify))
     ((string-match-p "wikipedia.org/" myPath)
      (let ((case-fold-search nil))
        (if (path-ends-in-image-suffix-p myPath)
            (xhm-source-url-linkify 0)
          (call-interactively 'xhm-wikipedia-url-linkify))))

     ((and (string-match-p "\\`https?://" myPath)) (xhm-source-url-linkify 0)) ; generic URL

     ((path-ends-in-image-suffix-p myPath) (image-file-to-html-figure-tag))

     (t (xah-file-linkify)))))
