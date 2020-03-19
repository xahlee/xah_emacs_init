;; -*- coding: utf-8; lexical-binding: t; -*-

;; Xah Lee's personal functions for transforming cursor location's text into HTML links.
;; 2007-10, 2011-05-29
;; ∑ http://xahlee.org/



(require 'url-util)

(defun xahsite-html-image-linkify ()
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
  (let ( $p0 $p1 $p2 $inputPath $currentDir $fullPath $altText )
    (progn ; sets $p1 $p2
      (if (use-region-p)
          (setq $p1 (region-beginning) $p2 (region-end))
        (save-excursion
          (setq $p0 (point))
          ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
          (skip-chars-backward "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗〘〙«»‹›·。\\`")
          (setq $p1 (point))
          (goto-char $p0)
          (skip-chars-forward "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗〘〙«»‹›·。\\'")
          (setq $p2 (point)))))
    (progn
      (setq $inputPath (buffer-substring-no-properties $p1 $p2))
      (setq $currentDir (file-name-directory (or (buffer-file-name) default-directory )))
      (setq $fullPath (expand-file-name (xah-local-url-to-file-path $inputPath) $currentDir ))
      (setq $altText (replace-regexp-in-string "-s$" "" (replace-regexp-in-string "_" " " (file-name-sans-extension (file-name-nondirectory $fullPath)) t t))))

    (if (xahsite-is-link-to-xahsite-p (file-relative-name $fullPath (or (buffer-file-name) default-directory)))
        (progn
          (if (file-exists-p $fullPath)
              (let ($wh $w $h $whStr)
                (setq $wh
                      (cond
                       ((string-match "\.svg$" $fullPath) (xah-get-image-dimensions $fullPath))
                       (t (xah-get-image-dimensions $fullPath))))
                (setq $w (number-to-string (elt $wh 0)))
                (setq $h (number-to-string (elt $wh 1)))
                (setq $whStr
                      (if (string-match "\.svg$" $fullPath)
                          ""
                        (format "width=\"%s\" height=\"%s\"" $w $h)))
                (delete-region $p1 $p2)
                (insert
                 (format "<img src=\"%s\" alt=\"%s\" %s />"
                         (xahsite-filepath-to-href-value $fullPath (or (buffer-file-name) default-directory))
                         $altText $whStr )))
            (error "File does not exist 「%s」" $fullPath )))
      (progn
        (delete-region $p1 $p2)
        (insert "<img src=\"" $fullPath "\" alt=\"" $altText "\">")))))

(defun xah-html-full-size-img-linkify (&optional @begin @end)
  "Make image file path at cursor point into a img link.

Example:
i/cat.jpg
becomes
<a class=\"big-i\" href=\"i/cat.jpg\">4176×2366</a>

If there's a text selection, use that region as file name.
Version 2018-04-10"
  (interactive)
  (let
      ($p0 $p1 $p2 $input $imgPath $dimension $width $height $resultStr)
    (progn ; sets $p1 $p2
      (if @begin
          (progn
            (setq $p1 @begin)
            (setq $p2 @end))
        (if (use-region-p)
            (setq $p1 (region-beginning) $p2 (region-end))
          (save-excursion
            (setq $p0 (point))
            ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
            (skip-chars-backward "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗〘〙«»‹›·。\\`")
            (setq $p1 (point))
            (goto-char $p0)
            (skip-chars-forward "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗〘〙«»‹›·。\\'")
            (setq $p2 (point))))))
    (setq $input (buffer-substring-no-properties $p1 $p2))
    (setq $imgPath (xah-local-url-to-file-path $input))
    (setq $dimension (xah-get-image-dimensions $imgPath))
    (setq $width (number-to-string (elt $dimension 0)))
    (setq $height (number-to-string (elt $dimension 1)))
    (setq $resultStr
          (concat "<a class=\"big-i\" href=\"" (file-relative-name $imgPath) "\">" $width "×" $height "</a>"))
    (delete-region $p1 $p2)
    (insert $resultStr)))


;; some custom HTML markup and functions for working with HTML

(defun xah-nks-linkify ()
  "Make the current word into into a link to Wolfram Science site.
For Example, if you cursor is on the word 「p123」, then
it becomes
 「<a href=\"http://www.wolframscience.com/nksonline/page-123\">p123</a>」
Version 2015-05-15"
  (interactive)
  (let* (($bounds (bounds-of-thing-at-point 'word))
         ($p1 (car $bounds))
         ($p2 (cdr $bounds))
         ($input (buffer-substring-no-properties $p1 $p2))
         ($pageNum (substring $input 1)))
    (delete-region $p1 $p2)
    (insert
     (concat
      "<a href=\"http://www.wolframscience.com/nksonline/page-"
      $pageNum "\">p" $pageNum "</a>"))))


;; more specific to Xah Lee

(defun xah-amazon-search-linkify-url (@sString @productCat @assid)
  "Returns a URL of amazon search based on search string and product category.
@sString is the search string. e.g. “deep throat”
@productCat is a short code for amazon's product category.
See `amazon-search-linkify' for the possible code string.
Sample call:
 (xah-amazon-search-linkify-url \"debbie does dollas\" \"dvd\" \"xahh-20\")"
  (interactive)
  (let (sStrPercent)
    (setq sStrPercent @sString)
    (setq sStrPercent (replace-regexp-in-string " " "%20" sStrPercent) )
    (setq sStrPercent (replace-regexp-in-string "," "%2c" sStrPercent) )

    (concat
     "<a class=\"amzs\" href=\"http://www.amazon.com/gp/search?ie=UTF8&amp;keywords="
     sStrPercent
     "&amp;tag="
     @assid
     "&amp;index="
     @productCat
     "&amp;linkCode=ur2&amp;camp=1789&amp;creative=9325\">"
     @sString
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
  (let ($p1 $p2 mainText tmplist sstr pcato pcc)
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (setq $p1 (line-beginning-position) $p2 (line-end-position) ))
    ;; get the text
    (setq mainText (buffer-substring-no-properties $p1 $p2) )
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

    (delete-region $p1 $p2)
    (insert  (xah-amazon-search-linkify-url sstr pcc "xahh-20"))
    ))

;; (defun local-linkify ()
;; "Make the path under cursor into a local link.\n
;; For Example, if you cursor is on the text “../emacs/emacs.html”,
;; then it'll become:
;; “<a href=\"../emacs/emacs.html\">Xah's Emacs Tutorial</a>”.
;; The link text is pulled from the file's <h1> tag.

;; If a region is active, use the region as file path."
;;  (interactive)
;;  (let ($path bounds tempBuff x1 x2 titleText $resultStr)
;;    (setq $path
;;          (if (use-region-p)
;;              (buffer-substring-no-properties (region-beginning) (region-end))
;;            (thing-at-point 'filename)
;;            ))
;;    (setq bounds (bounds-of-thing-at-point 'filename))

;;    (setq tempBuff (generate-new-buffer-name " temp"))

;;    (when (file-exists-p $path)
;;        (progn
;;          (save-current-buffer
;;            (message $path)
;;            (set-buffer (get-buffer-create tempBuff))
;;            (goto-char (point-min))
;;            (insert-file-contents $path nil nil nil t)
;;            (setq x1 (search-forward "<title>"))
;;            (search-forward "</title>")
;;            (setq x2 (search-backward "<"))
;;            (setq titleText (buffer-substring-no-properties x1 x2))
;;            (kill-buffer tempBuff))

;;          (setq $resultStr (concat "<a href=\"" $path "\">" titleText "</a>"))
;;          (save-excursion
;;            (delete-region (car bounds) (cdr bounds))
;;            (insert $resultStr))))
;;    ))

(defun xah-file-linkify (&optional @begin @end)
  "Make the path under cursor into a HTML link for xah site.

For Example, if you cursor is on the text “../emacs/emacs.html”,
then it'll become:
“<a href=\"../emacs/emacs.html\">Xah's Emacs Tutorial</a>”.
The link text is pulled from the file's <title> tag.

If there is text selection, use it as file path.

The file path can also be a full path or URL, See: `xahsite-web-path-to-filepath'.
Version 2018-11-08"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (save-excursion
       (let (p0 p1 p2)
         (setq p0 (point))
         ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
         (skip-chars-backward "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗〘〙«»‹›·。\\`")
         (setq p1 (point))
         (goto-char p0)
         (skip-chars-forward "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗〘〙«»‹›·。\\'")
         (setq p2 (point))
         (list p1 p2)))))
  (let* (
         ($input (buffer-substring-no-properties @begin @end))
         ($inputStParts (xah-html-split-uri-hashmark $input))
         ($pt1 (aref $inputStParts 0))
         ($fragPart (aref $inputStParts 1))
         ($fPath (xahsite-web-path-to-filepath $pt1 default-directory))
         $rPath $title $resultStr
         ($currentBufferFilePathOrDir (expand-file-name (or (buffer-file-name) default-directory)))
         ($currentBufferFileDir (file-name-directory (or (buffer-file-name) default-directory))))

    (if (file-exists-p $fPath)
        (progn
          (setq $title
                (if (string-match-p ".+html\\'" $fPath)
                    (concat (xah-html-get-html-file-title $fPath t) $fragPart)
                  (file-name-nondirectory $fPath)))
          (setq $resultStr
                (if (string-equal
                     (xahsite-get-domain-of-local-file-path $currentBufferFilePathOrDir)
                     (xahsite-get-domain-of-local-file-path $fPath))
                    (progn
                      (setq $rPath (file-relative-name $fPath $currentBufferFileDir))
                      ;; (setq $rPath (xah-html-get-relative-path-to-webroot $fPath))
                      (format "<a href=\"%s\">%s</a>"
                              (concat $rPath $fragPart)
                              (if (string-equal $title "") $rPath $title )))
                  (progn
                    (format "<a href=\"%s\">%s</a>" (concat (xahsite-filepath-to-url $fPath) $fragPart) $title))))
          (delete-region @begin @end)
          (insert $resultStr))
      (progn (message (format "Cannot locate the file: 「%s」" $fPath))))))

(defun nodejs-get-title (@fName @fragPart)
  "Return the file frag part function title.
 (nodejs-get-title \"~/web/xahlee_info/node_api/net.html\" \"#net_server_listen_port_host_backlog_callback\" )
returns
 \"server.listen(port, [host], [backlog], [callback])\"
"
  (with-temp-buffer
    (insert-file-contents @fName nil nil nil t)
    (goto-char 1)
    (if (string= @fragPart "")
        (progn
          (search-forward "<div id=\"apicontent\">")
          (if (search-forward "<h1>" nil t)
              (progn (buffer-substring-no-properties
                      (point)
                      (-  (search-forward "<span>") 6)) )
            (progn
              (goto-char 1)
              (buffer-substring-no-properties
               (search-forward "<title>")
               (- (search-forward "</title>") 8)) ) ) )
      (progn
        (search-forward @fragPart)
        (buffer-substring-no-properties
         (search-forward "\">")
         (-  (search-forward "</a>") 4))  )
      ) ))

(defun xah-clojure-word-ref-linkify ()
  "Make the path under cursor into a HTML link for xah site.
Version 2016-03-07"
  (interactive)
  (let ( $p1 $p2 $wd )
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (progn
        (skip-chars-backward "-A-Za-z0-9*+!-_?")
        (setq $p1 (point))
        (skip-chars-forward "-A-Za-z0-9*+!-_?")
        (setq $p2 (point))))
    (setq $wd (buffer-substring-no-properties $p1 $p2))
    (delete-region $p1 $p2)
    (insert (concat "<span class=\"ref\"><a href=\"../clojure-doc-1.8/clojure.core-api.html#clojure.core/" $wd "\">clojure.core/" $wd "</a></span>"))))

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

<span class=\"ref\"><a href=\"../node_api/process.html#process_process_execpath\">Node doc process.execpath</a></span>

linkText
Version 2016-10-31"
  (interactive)
  (let* (
         ($bds (xah-get-bounds-of-thing-or-region 'filepath))
         ($p1 (car $bds))
         ($p2 (cdr $bds))
         ($input (buffer-substring-no-properties $p1 $p2))
         (currentBufferFilePathOrDir (or (buffer-file-name) default-directory))
         (currentBufferFileDir (file-name-directory (or (buffer-file-name) default-directory)))

         (temp87318 (xah-html-split-uri-hashmark $input))
         (urlMainPart (elt temp87318 0))
         (urlFragPart (elt temp87318 1))
         ($fPath (xahsite-web-path-to-filepath urlMainPart default-directory))
         rltvPath titleText $resultStr
         )

    (if (file-exists-p $fPath)
        (progn
          (setq titleText (concat "⬢ " (nodejs-get-title $fPath urlFragPart)))
          (setq $resultStr
                (if (string-equal
                     (xahsite-get-domain-of-local-file-path currentBufferFilePathOrDir)
                     (xahsite-get-domain-of-local-file-path $fPath))
                    (progn
                      (setq rltvPath (file-relative-name $fPath currentBufferFileDir))
                      (format "<span class=\"ref\"><a href=\"%s%s\">%s</a></span>" rltvPath urlFragPart titleText))
                  (progn
                    (format "<span class=\"ref\"><a href=\"%s%s\">%s</a></span>" (xahsite-filepath-to-url $fPath) urlFragPart titleText))))
          (delete-region $p1 $p2)
          (insert $resultStr))
      (progn (message (format "Cannot locate the file: 「%s」" $fPath))))))

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
         ($bds (bounds-of-thing-at-point 'symbol))
         ($p1 (car $bds))
         ($p2 (cdr $bds))
         ($inputWord (if (use-region-p)
                         (progn (buffer-substring-no-properties (region-beginning) (region-end)))
                       (progn (buffer-substring-no-properties $p1 $p2))))
         ($wordPath (replace-regexp-in-string " " "_" (downcase $inputWord))) ; word for constructing possible dir
         ($pathsToTest
          (vector
           (concat "~/web/xahlee_info/SpecialPlaneCurves_dir/" (upcase-initials $wordPath) "_dir/" $wordPath ".html")
           (concat "~/web/xahlee_info/surface/" $wordPath "/" $wordPath ".html")))
         $i  $found-p $rPath $linkWord)

    ;; loop thru the paths until a file is found
    (setq $found-p nil)
    (setq $i 0)
    (while (and (not $found-p) (< $i (length $pathsToTest)))
      (setq $rPath (elt $pathsToTest $i))
      (setq $found-p (file-exists-p $rPath))
      (setq $i (1+ $i)))

    (if $found-p
        (progn
          (setq $linkWord (replace-regexp-in-string "_" " " $inputWord))
          (delete-region $p1 $p2)
          (insert (concat "<a href=\"" (file-relative-name $rPath) "\">" $linkWord "</a>")))
      (progn (beep) (message "No file found")))))

(defun xah-all-linkify ()
  "Make the text under cursor into a HTML link for xah's sites.

text can be any of:
• relative path (file, image, or anything)
• Wikipedia link
• any URL

They will be changed into a HTML link in various formats, depending on the input.

If there is text selection, use it as input.
Version 2019-05-11"
  (interactive)
  (if (string-match (concat "^" (expand-file-name "~/" ) "web/") (or (buffer-file-name) default-directory))
      (let ( $p1 $p2 $input)
        ;; (if (string-match "%" $input )
        ;;     (decode-coding-string (url-unhex-string "https://mysticsiva.wordpress.com/2016/11/04/%E3%82%AD%E3%83%BC%E3%82%AD%E3%83%A3%E3%83%83%E3%83%97%E4%BA%A4%E6%8F%9B3/") 'utf-8)
        ;;   $input)

        (if (use-region-p)
            (setq $p1 (region-beginning) $p2 (region-end))
          (save-excursion
            (let ($p0)
              (setq $p0 (point))
              ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
              (skip-chars-backward "^  \"\t\n'|[]{}<>〔〕“”〈〉《》【】〖〗〘〙«»‹›·。\\`")
              (setq $p1 (point))
              (goto-char $p0)
              (skip-chars-forward "^  \"\t\n'|[]{}<>〔〕“”〈〉《》【】〖〗〘〙«»‹›·。\\'")
              (setq $p2 (point)))))
        (setq $input (buffer-substring-no-properties $p1 $p2))
        (cond

         ((string-match-p "js_es2011\\|js_es2015\\|js_es2015_orig\\|js_es2016\\|js_es2018" $input)
          (xah-file-linkify $p1 $p2) (xah-insert-reference-span-tag))

         ((string-match-p "css_2.1_spec" $input)
          (xah-file-linkify $p1 $p2) (xah-insert-reference-span-tag))
         ((string-match-p "clojure-doc-1.8" $input)
          (xah-file-linkify $p1 $p2) (xah-insert-reference-span-tag))

         ((string-match-p "REC-SVG11-20110816" $input)
          (xah-file-linkify $p1 $p2) (xah-insert-reference-span-tag))
         ((string-match-p "css_transitions/CSS_Transitions.html" $input)
          (xah-file-linkify $p1 $p2) (xah-insert-reference-span-tag))

         ((string-match-p "\\`http://xahlee\.blogspot\.com/\\|\\`http://wordy-english\.blogspot\.com/" $input) (xah-blogger-linkify))

         ((string-match-p "/node_api/" $input) (xah-nodejs-ref-linkify))
         ((string-match-p "/emacs_manual/" $input) (xah-html-emacs-ref-linkify))

         ((xahsite-url-is-xah-website-p $input) (xah-file-linkify $p1 $p2))

         ;; (xah-html-image-figure-linkify)

         ((xah-html-image-file-suffix-p $input) (xah-html-image-figure-linkify))

         (t (xah-file-linkify $p1 $p2))))
    (xah-html-wrap-url)))
