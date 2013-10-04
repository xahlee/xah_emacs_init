;; 2010-06-07
;; ∑ http://xahlee.org/


;; c:/Users/xah/web/xahlee_org/Periodic_dosage_dir/sanga_pemci/xx.html



(defun youtube-string-old (video-id)
  "Return HTML code for embedding video of youtube's VIDEO-ID.
Example call:
 (youtube-string \"bFSS826ETlk\")"

  (let (ξurl)
    (setq ξurl (concat "http://www.youtube.com/v/" video-id))
    (concat
     "<object type=\"application/x-shockwave-flash\" data=\"" ξurl "\" width=\"480\" height=\"385\"><param name=\"movie\" value=\"" ξurl "\"></object>")
    ) )

(defun youtube-string (video-id)
  "Return HTML code for embedding video of youtube's VIDEO-ID.
Example call:
 (youtube-string \"bFSS826ETlk\")"
(concat "<iframe width=\"640\" height=\"480\" src=\"http://www.youtube.com/embed/" video-id "?rel=0\"></iframe>")
  )

(defun youtube-linkify ()
  "Make the current line into a embeded HTML video object.
The line can be a youtube ID or full URL.
Examples of lines:

bFSS826ETlk
http://www.youtube.com/watch?v=bFSS826ETlk

url with more parameters usually will work too.

The current line is the line the cursor is on, that is enclosed by “\n”.

Here's a example result:

<iframe title=\"YouTube video player\" class=\"youtube-player\" width=\"480\" height=\"390\" src=\"http://www.youtube.com/embed/Skan0yO-qU8?rel=0\"></iframe>

here's old version output
<object type=\"application/x-shockwave-flash\" data=\"http://www.youtube.com/v/bFSS826ETlk\" width=\"480\" height=\"385\"><param name=\"movie\" value=\"http://www.youtube.com/v/bFSS826ETlk\"></object>
"
  (interactive)
  (let (p1 p2 inputStr vID)
    (setq p1 (line-beginning-position) )
    (setq p2 (line-end-position) )
    (setq inputStr (buffer-substring-no-properties p1 p2))

    (string-match "v=\\(.\\{11\\}\\)" inputStr) ; youtube id are 11 chars
    (setq vID (match-string 1 inputStr) )

    ;; (setq tmp (replace-regexp-in-string "http://youtube\\.com/watch\\?v=" "" tmp))
    ;; (setq tmp (replace-regexp-in-string "http://www\\.youtube\\.com/v/" "" tmp))
    ;; (setq tmp (replace-regexp-in-string "&.+" "" tmp))
    ;; (setq tmp (replace-regexp-in-string "^http://www\\.youtube\\.com/watch\\?v=" "" tmp))
    ;; (setq vID tmp)

    (message "%s" vID)
    (delete-region p1 p2)

    (insert "<figure>\n")
    (insert (youtube-string vID) )
    (insert "\n<figcaption>\n")
    (insert "</figcaption>\n")
    (insert "</figure>\n")
    (search-backward "</figcaption>" )
    (backward-char 1)
    ))

(defun google-video-string (video-id)
	"Return HTML code for embedding video of Google Video's VIDEO-ID.
Example call:
 (google-video-string \"2336889538700185341\")"
  (let (ξurl)

    (setq ξurl (concat "http://video\.google\.com/googleplayer\.swf\?docid=" video-id "&amp;fs=true" ))
    (concat
     "<object type=\"application/x-shockwave-flash\" data=\"" ξurl 
     "\" width=\"400\" height=\"326\"><param name=\"movie\" value=\"" ξurl "\"><param name=\"allowFullScreen\" value=\"true\"><param name=\"allowScriptAccess\" value=\"always\"></object>")
    ))

(defun google-video-linkify ()
	"Make the current line into a embeded google video HTML object.
Example:
“http://video.google.com/videoplay?docid=3685846746009919856#”
⇒
<object type=\"application/x-shockwave-flash\" data=\"http://video.google.com/googleplayer.swf?docid=3685846746009919856&amp;fs=true\" width=\"400\" height=\"326\"><param name=\"movie\" value=\"http://video.google.com/googleplayer.swf?docid=3685846746009919856&amp;fs=true\"><param name=\"allowFullScreen\" value=\"true\"><param name=\"allowScriptAccess\" value=\"always\"></object>
"
	(interactive)
	(let (p1 p2 vID inputStr)
    (setq p1 (line-beginning-position) )
    (setq p2 (line-end-position) )
    (setq inputStr (buffer-substring-no-properties p1 p2))

    (string-match "docid=\\(.\\{19\\}\\)" inputStr)
    (setq vID (match-string 1 inputStr) )

    ;; (setq vID (replace-regexp-in-string "^http://video\\.google\\.com/videoplay\\?docid=" "" vID))
    ;; (setq vID (replace-regexp-in-string "&.+" "" vID))
    ;; (setq vID (replace-regexp-in-string "#$" "" vID))

    (delete-region p1 p2)
    (insert (google-video-string vID)
            ) ))


(defun dailymotion-video-string (video-id)
	"Return HTML code for embedding video of dailymotion.com's VIDEO-ID.
Example call:
 (dailymotion-linkify \"x1af0v\")"
	(concat
   "<object type=\"application/x-shockwave-flash\" data=\"http://www.dailymotion.com/swf/" video-id "\" width=\"480\" height=\"360\"><param name=\"movie\" value=\"http://www.dailymotion.com/swf/" video-id "\"><param name=\"allowFullScreen\" value=\"true\"></object>"
   )
  )

(defun dailymotion-video-linkify ()
  "Make the current line into a embeded HTML video object.

Example, if the current line is:
http://www.dailymotion.com/video/xz3am_white-rabbit-jefferson-airplane-li_music

it becomes
<object type=\"application/x-shockwave-flash\" data=\"http://www.dailymotion.com/swf/xz3am\" width=\"480\" height=\"360\"><param name=\"movie\" value=\"http://www.dailymotion.com/swf/xz3am\"><param name=\"allowFullScreen\" value=\"true\"></object>"
	(interactive)
	(let (p1 p2 inputStr tmp vID)
    (setq p1 (line-beginning-position) )
    (setq p2 (line-end-position) )
    (setq inputStr (buffer-substring-no-properties p1 p2))

    (setq tmp (replace-regexp-in-string "http://www\\.dailymotion\\.com/video/" "" inputStr))
    (setq tmp (car (split-string tmp "_")))
    (setq vID tmp)

    (delete-region p1 p2)
    (insert (dailymotion-video-string vID)
            ) ))

(defun vimeo-linkify ()
  "Make the current line into a embeded HTML video object.

Example, if the current line is:
http://vimeo.com/27206452

it becomes

<figure>
<iframe src=\"http://player.vimeo.com/video/27206452?title=0&amp;byline=0&amp;portrait=0\" width=\"400\" height=\"300\" frameborder=\"0\" webkitAllowFullScreen mozallowfullscreen allowFullScreen></iframe>
<figcaption>
</figcaption>
</figure>
"
	(interactive)
	(let (p1 p2 inputStr vID)
    (setq p1 (line-beginning-position) )
    (setq p2 (line-end-position) )
    (setq inputStr (buffer-substring-no-properties p1 p2))

    (string-match "http://vimeo.com/\\([0-9]\\{8\\}\\)" inputStr)
    (setq vID (match-string 1 inputStr) )

    (delete-region p1 p2)
    (insert (concat 
"<figure>\n"
"<iframe src=\"http://player.vimeo.com/video/" vID "?title=0&amp;byline=0&amp;portrait=0\" width=\"400\" height=\"300\" frameborder=\"0\" webkitAllowFullScreen mozallowfullscreen allowFullScreen></iframe>\n"
"<figcaption>
</figcaption>
</figure>"
) )

(backward-char 24)
 ))


(defun tudou-video-string (video-id)
	"Return HTML code for embedding video of tudou.com's VIDEO-ID.
Example call:
 (tudou-video-string \"9OoINUl31dQ\")"
  (let (ξurl)
    (setq ξurl (concat "http://www.tudou.com/v/" video-id "/v.swf" ))
    (concat "<object type=\"application/x-shockwave-flash\" data=\"" ξurl "\" width=\"480\" height=\"400\"><param name=\"movie\" value=\"" ξurl "\"></object>" )
    ) )

(defun tudou-video-linkify ()
  "Make the current line into a embeded HTML video object.

Example, if the current line is:
http://www.tudou.com/programs/view/9OoINUl31dQ

it becomes
<object type=\"application/x-shockwave-flash\" data=\"http://www.tudou.com/v/9OoINUl31dQ/v.swf\" width=\"480\" height=\"400\"><param name=\"movie\" value=\"http://www.tudou.com/v/9OoINUl31dQ/v.swf\"></object>"

	(interactive)
	(let (p1 p2 inputStr tmp vID)
    (setq p1 (line-beginning-position) )
    (setq p2 (line-end-position) )
    (setq inputStr (buffer-substring-no-properties p1 p2))
    (setq tmp (replace-regexp-in-string "http://www\\.tudou\\.com/programs/view/" "" inputStr))
    (setq tmp (replace-regexp-in-string "/" "" tmp))
    (setq vID tmp)

    (delete-region p1 p2)
    (insert (tudou-video-string vID)
            ) ))

(defun redtube-linkify ()
  "Make the current line into a embeded HTML video object.
The line can be a youtube ID or full URL.
Examples of input line syntax:

http://redtube.com/25635
25635

The current line is the line the cursor is on, that is enclosed by “\n”.

Here's a example result:

<object type=\"application/x-shockwave-flash\" data=\"http://embed.redtube.com/player/?id=25635&amp;style=redtube\" width=\"651\" height=\"462\"><param name=\"FlashVars\" value=\"id=25635&amp;style=redtube\"><param name=\"movie\" value=\"http://embed.redtube.com/player/?id=25635&amp;style=redtube\"></object>"
	(interactive)
	(let (p1 p2 inputStr tmp vID fixedurl)
    (setq p1 (line-beginning-position) )
    (setq p2 (line-end-position) )
    (setq inputStr (buffer-substring-no-properties p1 p2))
    (setq tmp (replace-regexp-in-string "http://redtube\\.com/" "" inputStr))
    (setq tmp (replace-regexp-in-string "http://www\\.redtube\\.com/" "" tmp))
    (setq tmp (replace-regexp-in-string "http://embed\\.redtube\\.com/player/?id=" "" tmp))
    (setq vID tmp)
    (setq fixedurl "http://embed.redtube.com/player/?id=")

    (delete-region p1 p2)
    (insert
     (concat
      "<object type=\"application/x-shockwave-flash\" data=\""
      fixedurl
      vID "&amp;style=redtube"
      "\" width=\"651\" height=\"462\">"
      "<param name=\"FlashVars\" value=\"id="
      vID "&amp;style=redtube"
      "\">"
      "<param name=\"movie\" value=\"" fixedurl
      vID "&amp;style=redtube"
      "\">"
      "</object>"
      )) ))



;; (defun get-current-line-or-region ()
;;   "Return current line or text selection if there's one."
;;   (interactive)
;;   (let (p1 p2)
;;     (save-excursion
;;       (search-backward "\n" nil t) (forward-char)
;;       (setq p1 (point))
;;       (search-forward "\n" nil t) (backward-char)
;;       (setq p2 (point)))
;;     (buffer-substring-no-properties p1 p2)    
;;     )
;;   )


(defun break-video-linkify ()
  "Make the current line into a embeded HTML video object.

The current line must be a embeded video HTML code from break.com.

Example:
<object width=\"464\" height=\"376\" id=\"670986\" type=\"application/x-shockwave-flash\" classid=\"clsid:D27CDB6E-AE6D-11cf-96B8-444553540000\" alt=\"Hot Asian Chick Falls Off a Stripper Pole Funny Videos\"><param name=\"movie\" value=\"http://embed.break.com/NjcwOTg2\"></param><param name=\"allowScriptAccess\" value=\"always\"></param><embed src=\"http://embed.break.com/NjcwOTg2\" type=\"application/x-shockwave-flash\" allowScriptAccess=always width=\"464\" height=\"376\"></embed></object><br><font size=1><a href=\"http://www.break.com/index/hot-asian-chick-falls-off-stripper-pole.html\" target=\"_blank\">Hot Asian Chick Falls Off a Stripper Pole</a> - Watch more <a href=\"http://www.break.com/\" target=\"_blank\">Funny Videos</a></font>

Becomes:
<object type=\"application/x-shockwave-flash\" data=\"http://embed.break.com/NjcwOTg2\" width=\"464\" height=\"376\"><param name=\"movie\" value=\"http://embed.break.com/NjcwOTg2\"><param name=\"allowScriptAccess\" value=\"always\"><param name=\"id\" value=\"670986\"><param name=\"classid=\" value=\"clsid:D27CDB6E-AE6D-11cf-96B8-444553540000\"></object>

This is valid HTML.

See: URL  `http://xahlee.org/js/html_embed_video.html'"
	(interactive)
	(let (p1 p2 tmp width height vID classid hashcode)
    (setq p1 (line-beginning-position) )
    (setq p2 (line-end-position) )

    (setq tmp (buffer-substring-no-properties p1 p2))
    (setq tmp (replace-regexp-in-string "\"" "†" tmp)) ; change double quote to something else to avoid tooth pick syndrom in regex

    (with-temp-buffer
      (insert tmp)
      (goto-char (point-min))
      (search-forward-regexp "width=†\\([[:digit:]]+\\)†")
      (setq width (match-string 1))

      (search-forward-regexp "height=†\\([[:digit:]]+\\)†")
      (setq height (match-string 1))

      (search-forward-regexp "id=†\\([[:digit:]]+\\)†")
      (setq vID (match-string 1))

      (search-forward-regexp "classid=†\\([^†]+\\)†")
      (setq classid (match-string 1))

      (search-forward-regexp "http://embed\\.break\\.com/\\([^†]+\\)†")
      (setq hashcode (match-string 1))
      )

    (delete-region p1 p2)
    (insert (concat  "<object type=\"application/x-shockwave-flash\" data=\"http://embed.break.com/" hashcode "\" width=\"" width "\" height=\"" height "\"><param name=\"movie\" value=\"http://embed.break.com/" hashcode "\"><param name=\"allowScriptAccess\" value=\"always\"><param name=\"id\" value=\"" vID "\"><param name=\"classid=\" value=\"" classid "\"></object>")
            ) ))
