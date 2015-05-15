;;-*- coding: utf-8 -*-
;; Xah Lee's personal functions for transforming cursor location's text into HTML links.
;; 2007-10, 2011-05-29
;; ∑ http://xahlee.org/


(require 'url-util)

(defun xah-html-image-linkify ()
  "Replace a path to image file with a HTML img tag.
Example:
 emacs_logo.png
become
 <img src=\"emacs_logo.png\" alt=\"emacs logo\" width=\"123\" height=\"456\">

This function requires the 「identify」 command from ImageMagick.
URL `http://ergoemacs.org/emacs/elisp_image_tag.html'
Version 2015-05-15"
  (interactive)
  (let* ((ξbounds (bounds-of-thing-at-point 'filename))
         (ξp1 (car ξbounds))
         (ξp2 (cdr ξbounds))
         (ξimgPath (buffer-substring-no-properties ξp1 ξp2 ))
         (ξhrefValue (file-relative-name ξimgPath
                                         (file-name-directory (or (buffer-file-name) default-directory))))
         (ξaltText (replace-regexp-in-string "_" " " (replace-regexp-in-string "\\.[A-Za-z]\\{3,4\\}$" "" ξhrefValue t t) t t))
         (ξimgWH (xah-get-image-dimensions ξimgPath))
         (ξwidth (number-to-string (elt ξimgWH 0)))
         (ξheight (number-to-string (elt ξimgWH 1))))
    (delete-region ξp1 ξp2)
    (insert
     (concat
      "<img src=\""
      ξhrefValue
      "\"" " " "alt=\"" ξaltText "\"" " " "width=\"" ξwidth "\" " "height=\"" ξheight "\">"))))

(defun xahsite-html-image-linkify ( &optional φbegin φend)
  "Replace a image file's path under cursor with a HTML img tag.
If there's a text selection, use that as path.
For example,
 i/cat.png
becames
 <img src=\"i/cat.png\" alt=\"cat\" width=\"707\" height=\"517\" />

Image path can be a URL or local file.  Supported file suffix are {.gif, .png, .svg}. If path starts with “http”, then no “width” and “height” attribute will be added.
URL `http://ergoemacs.org/emacs/elisp_image_tag.html'
Version 2015-05-12"
  (interactive)
  (let ( ξp0 ξp1 ξp2 ξinputPath ξcurrentDir ξfullPath ξaltText )
    (progn ; sets ξp1 ξp2
      (if φbegin
          (progn (setq ξp1 φbegin) (setq ξp2 φend))
        (if (use-region-p)
            (progn (setq ξp1 (region-beginning)) (setq ξp2 (region-end)))
          (save-excursion
            (setq ξp0 (point))
            ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
            (skip-chars-backward "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\`")
            (setq ξp1 (point))
            (goto-char ξp0)
            (skip-chars-forward "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\'")
            (setq ξp2 (point))))))

    (progn
      (setq ξinputPath (buffer-substring-no-properties ξp1 ξp2))
      (setq ξcurrentDir (file-name-directory (or (buffer-file-name) default-directory )))
      (setq ξfullPath (expand-file-name (xah-local-url-to-file-path ξinputPath) ξcurrentDir ))
      (setq ξaltText (replace-regexp-in-string "-s$" "" (replace-regexp-in-string "_" " " (file-name-sans-extension (file-name-nondirectory ξfullPath)) t t))))

    (if (xahsite-is-link-to-xahsite-p (file-relative-name ξfullPath (or (buffer-file-name) default-directory)))
        (progn
          (if (file-exists-p ξfullPath)
              (let (ξwh ξw ξh ξwhStr)
                (setq ξwh
                      (cond
                       ((string-match "\.svg$" ξfullPath) (xah-get-image-dimensions ξfullPath))
                       (t (xah-get-image-dimensions ξfullPath))))
                (setq ξw (number-to-string (elt ξwh 0)))
                (setq ξh (number-to-string (elt ξwh 1)))
                (setq ξwhStr
                      (if (string-match "\.svg$" ξfullPath)
                          ""
                        (format "width=\"%s\" height=\"%s\"" ξw ξh)))
                (delete-region ξp1 ξp2)
                (insert
                 (format "<img src=\"%s\" alt=\"%s\" %s />"
                         (xahsite-filepath-to-href-value ξfullPath (or (buffer-file-name) default-directory))
                         ξaltText ξwhStr )))
            (error "File does not exist 「%s」" ξfullPath )))
      (progn
        (delete-region ξp1 ξp2)
        (insert "<img src=\"" ξfullPath "\" alt=\"" ξaltText "\">")))))

(defun xah-image-file-to-html-figure-tag ()
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
  (progn
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
    (backward-char)))

(defun xah-html-full-size-img-linkify (&optional φbegin φend)
  "Make image file path at cursor point into a img link.

Example:
i/goddess.jpg
becomes
<a class=\"big-i\" href=\"i/goddess.jpg\" title=\"622×800\" target=\"_blank\">❐</a>

If there's a text selection, use that region as file name."
  (interactive)
  (let
      (ξp0 ξp1 ξp2 ξinputStr ξimgPath ξdimension ξwidth ξheight ξresultStr)
    (progn ; sets ξp1 ξp2
      (if φbegin
          (progn
            (setq ξp1 φbegin)
            (setq ξp2 φend))
        (if (use-region-p)
            (progn
              (setq ξp1 (region-beginning))
              (setq ξp2 (region-end)))
          (save-excursion
            (setq ξp0 (point))
            ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
            (skip-chars-backward "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\`")
            (setq ξp1 (point))
            (goto-char ξp0)
            (skip-chars-forward "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\'")
            (setq ξp2 (point))))))

    (setq ξinputStr (buffer-substring-no-properties ξp1 ξp2))
    (setq ξimgPath (xah-local-url-to-file-path ξinputStr))
    (setq ξdimension (xah-get-image-dimensions-imk ξimgPath))
    (setq ξwidth (number-to-string (elt ξdimension 0)))
    (setq ξheight (number-to-string (elt ξdimension 1)))
    (setq ξresultStr
          (concat "<a class=\"big-i\" href=\"" (file-relative-name ξimgPath) "\" target=\"_blank\">" ξwidth "×" ξheight "</a>"))

    (delete-region ξp1 ξp2)
    (insert ξresultStr)))



(defun xah-blogger-linkify ()
  "Make URL at cursor point into a HTML link.

Example: http://xahlee.blogspot.com/2010/03/some.html
becomes

<div class=\"blgcmt\"><a href=\"http://xahlee.blogspot.com/2010/03/some.html\">✍</a></div>"
  (interactive)
  (let (ξbds p7 p8 ξurl)
    (setq ξbds (get-selection-or-unit 'url))
    (setq ξurl (elt ξbds 0) )
    (setq p7 (elt ξbds 1) )
    (setq p8 (elt ξbds 2) )

    (delete-region p7 p8)
    (insert (concat "<div class=\"blgcmt\"><a href=\"" (url-encode-url ξurl) "\">✍</a></div>"))))



(defun youporn-search-linkify ()
  "Make the current line into a YouPorn.com link.
For example, if the cursor is on the line:
anal
Then it'll become
\(YouPorn video: <a href=\"http://www.youporn.com/search?query=anal\">anal</a>\)"
  (interactive)
  (let (ξp1 ξp2 ξword ξurl)

    (if (use-region-p)
        (progn (setq ξp1 (region-beginning))
               (setq ξp2 (region-end)))
      (progn (setq ξp1 (line-beginning-position))
             (setq ξp2 (line-end-position))))

    (setq ξword (buffer-substring-no-properties ξp1 ξp2) )

    (setq ξurl (concat "http://www.youporn.com/search?query=" ξword) )
    (setq ξurl (replace-regexp-in-string " " "+" ξurl ) )
    (delete-region ξp1 ξp2)
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
  (let (ξp1 ξp2 ξword ξurl)
    (if (use-region-p)
        (progn (setq ξp1 (region-beginning))
               (setq ξp2 (region-end)))
      (progn (setq ξp1 (line-beginning-position))
             (setq ξp2 (line-end-position))))

    (setq ξword (buffer-substring-no-properties ξp1 ξp2))

    (setq ξurl (concat "http://youtube.com/results?search_query=" ξword "&amp;search=Search"))
    (setq ξurl (replace-regexp-in-string " " "+" ξurl ))
    (setq ξurl (replace-regexp-in-string "," "%2C" ξurl ))
    (delete-region ξp1 ξp2)
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
    (setq strEncoded (url-encode-url strEncoded ) )
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
  (let (ξp1 ξp2 ξword ξurl)
    (if (use-region-p)
        (progn (setq ξp1 (region-beginning))
               (setq ξp2 (region-end)))
      (progn (setq ξp1 (line-beginning-position))
             (setq ξp2 (line-end-position))))
    (setq ξword (buffer-substring-no-properties ξp1 ξp2))
    (setq ξurl (video-search-string ξword))
    (delete-region ξp1 ξp2)
    (insert "<a class=\"gvidsr\" href=\"" ξurl "\">" ξword "</a>")))

(defun google-search-linkify ()
  "Make the current line into a Google search link.
For example, if the cursor is on the line:

emacs lisp

Then it'll become

<p>Google search: <a href=\"http://www.google.com/search?q=emacs+lisp\">emacs lisp</a>.</p>

Warning: the line must end in a line return char else the result is wrong."
  (interactive)
  (let (ξp1 ξp2 ξword ξurl)
    (if (use-region-p)
        (progn (setq ξp1 (region-beginning))
               (setq ξp2 (region-end)))
      (progn (setq ξp1 (line-beginning-position))
             (setq ξp2 (line-end-position))))
    (setq ξword (buffer-substring-no-properties ξp1 ξp2))
    (setq ξurl (concat "http://www.google.com/search?q=" ξword))
    (setq ξurl (replace-regexp-in-string " " "+" ξurl ))
    (delete-region ξp1 ξp2)
    (insert "<p>Google search: <a href=\"" ξurl "\">" ξword "</a>.</p>\n")))


;; some custom HTML markup and functions for working with HTML

(defun nks-linkify ()
  "Make the current word into into a link to Wolfram Science site.
For Example, if you cursor is on the word 「p123」, then
it becomes
 「<a href=\"http://www.wolframscience.com/nksonline/page-123\">p123</a>」
Version 2015-05-15"
  (interactive)
  (let* ((ξbounds (bounds-of-thing-at-point 'word))
         (ξp1 (car ξbounds))
         (ξp2 (cdr ξbounds))
         (ξinputStr (buffer-substring-no-properties ξp1 ξp2))
         (ξpageNum (substring ξinputStr 1)))
    (delete-region ξp1 ξp2)
    (insert
     (concat
      "<a href=\"http://www.wolframscience.com/nksonline/page-"
      ξpageNum "\">p" ξpageNum "</a>"))))


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
  (let (ξp1 ξp2 mainText tmplist sstr pcato pcc)
    (if (use-region-p)
        (setq ξp1 (region-beginning) ξp2 (region-end))
      (progn
        (setq ξp1 (line-beginning-position) )
        (setq ξp2 (line-end-position) )
        ))
    ;; get the text
    (setq mainText (buffer-substring-no-properties ξp1 ξp2) )
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

    (delete-region ξp1 ξp2)
    (insert  (amazon-search-linkify-url sstr pcc "xahh-20"))
    ))

(defun xah-amazon-linkify ()
  "Make the current amazon URL or selection into a link.

Examples of amazon product URL formats
http://www.amazon.com/Cyborg-R-T-Gaming-Mouse/dp/B003CP0BHM/ref=pd_sim_e_1
http://www.amazon.com/gp/product/B003CP0BHM
http://www.amazon.com/exec/obidos/ASIN/B003CP0BHM/xahh-20
http://www.amazon.com/exec/obidos/tg/detail/-/B003CP0BHM/
http://www.amazon.com/dp/B003CP0BHM?tag=xahhome-20
http://amzn.to/1F5M1hA

Example output:
<a class=\"amz\" href=\"http://www.amazon.com/dp/B003CP0BHM/?tag=xahh-20\" title=\"Cyborg R T Gaming Mouse\">amazon</a>

For info about the Amazon ID in URL, see: URL `http://en.wikipedia.org/wiki/Amazon_Standard_Identification_Number'
URL `http://ergoemacs.org/emacs/elisp_amazon-linkify.html'
Version 2015-03-18"
  (interactive)
  (let (ξp1 ξp2 ξinputText ξasin ξproductName )
    (if (use-region-p)
        (progn
          (setq ξp1 (region-beginning))
          (setq ξp2 (region-end)))
      (save-excursion
        (let ((p0 (point)))
          (skip-chars-backward "^ \n\t")
          (setq ξp1 (point))
          (goto-char p0)
          (skip-chars-forward "^ \n\t")
          (setq ξp2 (point)))))

    (setq ξinputText (buffer-substring-no-properties ξp1 ξp2))

    (if (string-match "//amzn.to/" ξinputText)
        (progn
          (delete-region ξp1 ξp2)
          (insert
           (format
            "<a class=\"amzlnk\" href=\"%s\">amazon</a>"
            ξinputText)))
      (progn
        (setq ξasin
              (cond
               ((string-match "/dp/\\([[:alnum:]]\\{10\\}\\)/?" ξinputText) (match-string 1 ξinputText))
               ((string-match "/dp/\\([[:alnum:]]\\{10\\}\\)\\?tag=" ξinputText) (match-string 1 ξinputText))
               ((string-match "/gp/product/\\([[:alnum:]]\\{10\\}\\)" ξinputText) (match-string 1 ξinputText))
               ((string-match "/ASIN/\\([[:alnum:]]\\{10\\}\\)" ξinputText) (match-string 1 ξinputText))
               ((string-match "/tg/detail/-/\\([[:alnum:]]\\{10\\}\\)/" ξinputText) (match-string 1 ξinputText))
               ((and
                 (equal 10 (length ξinputText ))
                 (string-match "\\`\\([[:alnum:]]\\{10\\}\\)\\'" ξinputText))
                ξinputText)
               (t (error "no amazon ASIN found"))))

        (setq
         ξproductName
         (replace-regexp-in-string
          "-" " "
          (if (string-match "amazon\.com/\\([^/]+?\\)/dp/" ξinputText)
              (progn (match-string 1 ξinputText))
            (progn
              (message "no product name found" ))
            ""
            )))

        (delete-region ξp1 ξp2)
        (insert
         "<a class=\"amz\" href=\"http://www.amazon.com/dp/"
         ξasin "/?tag=xahh-20\" title=\"" ξproductName "\">amazon</a>")
        (search-backward "\">")))))

;; (defun local-linkify ()
;; "Make the path under cursor into a local link.\n
;; For Example, if you cursor is on the text “../emacs/emacs.html”,
;; then it'll become:
;; “<a href=\"../emacs/emacs.html\">Xah's Emacs Tutorial</a>”.
;; The link text is pulled from the file's <h1> tag.

;; If a region is active, use the region as file path."
;;  (interactive)
;;  (let (ξpath bounds tempBuff x1 x2 titleText ξresultStr)
;;    (setq ξpath
;;          (if (use-region-p)
;;              (buffer-substring-no-properties (region-beginning) (region-end))
;;            (thing-at-point 'filename)
;;            ))
;;    (setq bounds (bounds-of-thing-at-point 'filename))

;;    (setq tempBuff (generate-new-buffer-name " temp"))

;;    (when (file-exists-p ξpath)
;;        (progn
;;          (save-current-buffer
;;            (message ξpath)
;;            (set-buffer (get-buffer-create tempBuff))
;;            (goto-char (point-min))
;;            (insert-file-contents ξpath nil nil nil t)
;;            (setq x1 (search-forward "<title>"))
;;            (search-forward "</title>")
;;            (setq x2 (search-backward "<"))
;;            (setq titleText (buffer-substring-no-properties x1 x2))
;;            (kill-buffer tempBuff))

;;          (setq ξresultStr (concat "<a href=\"" ξpath "\">" titleText "</a>"))
;;          (save-excursion
;;            (delete-region (car bounds) (cdr bounds))
;;            (insert ξresultStr))))
;;    ))

(defun xah-file-linkify (&optional φbegin φend)
  "Make the path under cursor into a HTML link for xah site.

For Example, if you cursor is on the text “../emacs/emacs.html”,
then it'll become:
“<a href=\"../emacs/emacs.html\">Xah's Emacs Tutorial</a>”.
The link text is pulled from the file's <title> tag.

If there is text selection, use it as file path.

The file path can also be a full path or URL, See: `xahsite-web-path-to-filepath'.
"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (save-excursion
       (let (p0 p1 p2)
         (setq p0 (point))
         ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
         (skip-chars-backward "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\`")
         (setq p1 (point))
         (goto-char p0)
         (skip-chars-forward "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\'")
         (setq p2 (point))
         (list p1 p2)))))
  (let* (
         (ξinputStr (buffer-substring-no-properties φbegin φend))
         (inputStParts (split-uri-hashmark ξinputStr))
         (pt1 (aref inputStParts 0))
         (fragPart (aref inputStParts 1))
         (ξfPath (xahsite-web-path-to-filepath pt1 default-directory))
         rltvPath titleText ξresultStr
         (currentBufferFilePathOrDir (expand-file-name (or (buffer-file-name) default-directory)))
         (currentBufferFileDir (file-name-directory (or (buffer-file-name) default-directory))))

    (if (file-exists-p ξfPath)
        (progn
          (setq titleText
                (if (string-match-p ".+html\\'" ξfPath)
                    (concat (xah-html-get-html-file-title ξfPath "noerror") fragPart)
                  (file-name-nondirectory ξfPath)))
          (setq ξresultStr
                (if (string-equal
                     (xahsite-get-domain-of-local-file-path currentBufferFilePathOrDir)
                     (xahsite-get-domain-of-local-file-path ξfPath))
                    (progn
                      (setq rltvPath (file-relative-name ξfPath currentBufferFileDir))
(message "title is 「%S」" titleText)
                      (format "<a href=\"%s\">%s</a>"
                              (concat rltvPath fragPart)
                              (if (string-equal titleText "") rltvPath titleText )))
                  (progn
                    (format "<a href=\"%s\">%s</a>" (concat (xahsite-filepath-to-url ξfPath) fragPart) titleText))))
          (delete-region φbegin φend)
          (insert ξresultStr))
      (progn (message (format "Cannot locate the file: 「%s」" ξfPath))))))

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

(defun xah-clojure-word-ref-linkify ()
  "Make the path under cursor into a HTML link for xah site.
Version 2014-10-31"
  (interactive)
  (let ( ξp1 ξp2 ξwd )
    (if (use-region-p)
        (setq ξp1 (region-beginning) ξp2 (region-end))
      (progn
        (skip-chars-backward "-A-Za-z0-9*+!-_?")
        (setq ξp1 (point))
        (skip-chars-forward "-A-Za-z0-9*+!-_?")
        (setq ξp2 (point))))
    (setq ξwd (buffer-substring-no-properties ξp1 ξp2))
    (delete-region ξp1 ξp2)
    (insert (concat "<span class=\"ref\"><a href=\"../clojure-doc-1.6/clojure.core-api.html#clojure.core/" ξwd "\">clojure.core/" ξwd "</a></span>"))))

(defun xah-nodejs-ref-linkify ()
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
         (ξbds (get-selection-or-unit 'filepath))
         (ξinputStr (elt ξbds 0))
         (ξp1 (aref ξbds 1))
         (ξp2 (aref ξbds 2))
         (currentBufferFilePathOrDir (or (buffer-file-name) default-directory))
         (currentBufferFileDir (file-name-directory (or (buffer-file-name) default-directory)))

         (temp87318 (split-uri-hashmark ξinputStr))
         (urlMainPart (elt temp87318 0))
         (urlFragPart (elt temp87318 1))
         (ξfPath (xahsite-web-path-to-filepath urlMainPart default-directory))
         rltvPath titleText ξresultStr
         )

    (if (file-exists-p ξfPath)
        (progn
          (setq titleText (concat "⬢ " (nodejs-get-title ξfPath urlFragPart)))
          (setq ξresultStr
                (if (string-equal
                     (xahsite-get-domain-of-local-file-path currentBufferFilePathOrDir)
                     (xahsite-get-domain-of-local-file-path ξfPath))
                    (progn
                      (setq rltvPath (file-relative-name ξfPath currentBufferFileDir))
                      (format "<span class=\"ref\"><a href=\"%s%s\">%s</a></span>" rltvPath urlFragPart titleText))
                  (progn
                    (format "<span class=\"ref\"><a href=\"%s%s\">%s</a></span>" (xahsite-filepath-to-url ξfPath) urlFragPart titleText))))
          (delete-region ξp1 ξp2)
          (insert ξresultStr))
      (progn (message (format "Cannot locate the file: 「%s」" ξfPath))))))

(defun xah-javascript-linkify ()
  "Make the path under cursor into a HTML link.
 ⁖ <script src=\"xyz.js\"></script>"
  (interactive)
  (let* (
         (ξbds (get-selection-or-unit 'filepath))
         (ξinputStr (elt ξbds 0))
         (ξp1 (aref ξbds 1))
         (ξp2 (aref ξbds 2))
         ξfPath
         )
    (setq ξfPath (file-relative-name ξinputStr))
    (delete-region ξp1 ξp2)
    (insert (format "<script defer src=\"%s\"></script>" ξfPath))))

(defun xah-audio-file-linkify ()
  "Make the path under cursor into a HTML link.
 ⁖
 xyz.mp3
becomes
 <audio src=\"xyz.mp3\"></audio>"
  (interactive)
  (let* (
         (ξbds (get-selection-or-unit 'filepath))
         (ξinputStr (elt ξbds 0))
         (ξp1 (aref ξbds 1))
         (ξp2 (aref ξbds 2))
         ξfPath
         )
    (setq ξfPath (file-relative-name ξinputStr))
    (delete-region ξp1 ξp2)
    (insert (format "<audio src=\"%s\" controls></audio>" ξfPath))))

(defun xah-css-linkify ()
  "Make the path under cursor into a HTML link.
 ⁖
/home/xah/web/xahlee_org/lit.css
→
<link rel=\"stylesheet\" href=\"../lit.css\" />
"
  (interactive)
  (let* (
         (ξbds (get-selection-or-unit 'filepath))
         (ξinputStr (elt ξbds 0))
         (ξp1 (aref ξbds 1))
         (ξp2 (aref ξbds 2))
         ξfPath
         )
    (setq ξfPath (file-relative-name ξinputStr))
    (delete-region ξp1 ξp2)
    (insert (format "<link rel=\"stylesheet\" href=\"%s\" />" ξfPath))))

(defun xah-curve-linkify ()
  "Make the current word or text selection into a HTML link.

This function works on Xah Lee's website only.
 Example:
 “parabola” becomes
“<a href=\"../Parabola_dir/parabola.html\">parabola</a>”.

The directory to search includes:
“SpecialPlaneCurves_dir” and “surface”.
Version 2015-03-18"
  (interactive)
  (let* (
         (ξbds (bounds-of-thing-at-point 'symbol))
         (ξp1 (car ξbds))
         (ξp2 (cdr ξbds))
         (ξinputWord (if (use-region-p)
                         (progn (buffer-substring-no-properties (region-beginning) (region-end)))
                       (progn (buffer-substring-no-properties ξp1 ξp2))))
         (ξwordPath (replace-regexp-in-string " " "_" (downcase ξinputWord))) ; word for constructing possible dir
         (ξpathsToTest
          (vector
           (concat "~/web/xahlee_info/SpecialPlaneCurves_dir/" (upcase-initials ξwordPath) "_dir/" ξwordPath ".html")
           (concat "~/web/xahlee_info/surface/" ξwordPath "/" ξwordPath ".html")))
         ξi  ξfound-p ξrPath ξlinkWord)

    ;; loop thru the paths until a file is found
    (setq ξfound-p nil)
    (setq ξi 0)
    (while (and (not ξfound-p) (< ξi (length ξpathsToTest)))
      (setq ξrPath (elt ξpathsToTest ξi))
      (setq ξfound-p (file-exists-p ξrPath))
      (setq ξi (1+ ξi)))

    (if ξfound-p
        (progn
          (setq ξlinkWord (replace-regexp-in-string "_" " " ξinputWord))
          (delete-region ξp1 ξp2)
          (insert (concat "<a href=\"" (file-relative-name ξrPath) "\">" ξlinkWord "</a>")))
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
  (let (
        ξp1 ξp2
        ξpath
        )

    (if (use-region-p)
        (setq ξp1 (region-beginning) ξp2 (region-end))
      (let (ξp0)
        (setq ξp0 (point))
        ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
        (skip-chars-backward "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\`")
        (setq ξp1 (point))
        (goto-char ξp0)
        (skip-chars-forward "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\'")
        (setq ξp2 (point))))

    (setq ξpath (buffer-substring-no-properties ξp1 ξp2))

    (cond
     ((string-match-p "\\`http://xahlee\.blogspot\.com/\\|\\`http://wordy-english\.blogspot\.com/" ξpath) (xah-blogger-linkify))
     ((string-match-p "www\.amazon\.com/" ξpath) (xah-amazon-linkify))
     ((string-match-p "//amzn\.to/" ξpath) (xah-amazon-linkify))
     ((string-match-p "www\.youtube\.com/watch" ξpath) (xah-youtube-linkify))
     ((string-match-p "/emacs_manual/" ξpath) (xah-html-emacs-ref-linkify))
     ((string-match-p "/node_api/" ξpath) (xah-nodejs-ref-linkify))
     ((string-match-p "\\.js\\'" ξpath) (xah-javascript-linkify))
     ((string-match-p "\\.css\\'" ξpath) (xah-css-linkify))
     ((string-match-p "\\.mp3\\'" ξpath) (xah-audio-file-linkify))
     ((string-match-p "\\.ogg\\'" ξpath) (xah-audio-file-linkify))

     ((string-match-p "javascript_ecma-262_5.1_2011" ξpath) (xah-file-linkify ξp1 ξp2) (xah-add-reference-span-tag))
     ((string-match-p "css_transitions/CSS_Transitions.html" ξpath) (xah-file-linkify ξp1 ξp2) (xah-add-reference-span-tag))

     ((xahsite-url-is-xah-website-p ξpath) (xah-file-linkify ξp1 ξp2))
     ((string-match-p "wikipedia.org/" ξpath)
      (let ((case-fold-search nil))
        (if (xah-path-ends-in-image-suffix-p ξpath)
            (xah-html-source-url-linkify 0)
          (call-interactively 'xah-html-wikipedia-url-linkify))))

     ((and (string-match-p "\\`https?://" ξpath)) (xah-html-source-url-linkify 0)) ; generic URL

     ((xah-path-ends-in-image-suffix-p ξpath) (xah-image-file-to-html-figure-tag))

     (t (xah-file-linkify ξp1 ξp2)))))
