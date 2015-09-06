;; 2015-09-05
;; ∑ http://xahlee.info/

(defun google-video-linkify ()
  "Make the current line into a embeded google video HTML object.
Example:
“http://video.google.com/videoplay?docid=3685846746009919856#”
⇒
<object type=\"application/x-shockwave-flash\" data=\"http://video.google.com/googleplayer.swf?docid=3685846746009919856&amp;fs=true\" width=\"400\" height=\"326\"><param name=\"movie\" value=\"http://video.google.com/googleplayer.swf?docid=3685846746009919856&amp;fs=true\"><param name=\"allowFullScreen\" value=\"true\"><param name=\"allowScriptAccess\" value=\"always\"></object>
"
  (interactive)
  (let (ξp1 ξp2 ξvID ξinputStr)
    (setq ξp1 (line-beginning-position))
    (setq ξp2 (line-end-position))
    (setq ξinputStr (buffer-substring-no-properties ξp1 ξp2))

    (string-match "docid=\\(.\\{19\\}\\)" ξinputStr)
    (setq ξvID (match-string 1 ξinputStr))

    ;; (setq ξvID (replace-regexp-in-string "^http://video\\.google\\.com/videoplay\\?docid=" "" ξvID))
    ;; (setq ξvID (replace-regexp-in-string "&.+" "" ξvID))
    ;; (setq ξvID (replace-regexp-in-string "#$" "" ξvID))

    (delete-region ξp1 ξp2)
    (insert (google-video-string ξvID))))

(defun google-video-string (φvideo-id)
  "Return HTML code for embedding video of Google Video's φvideo-id.
Example call:
 (google-video-string \"2336889538700185341\")"
  (let (ξurl)

    (setq ξurl (concat "http://video\.google\.com/googleplayer\.swf\?docid=" φvideo-id "&amp;fs=true" ))
    (concat
     "<object type=\"application/x-shockwave-flash\" data=\"" ξurl
     "\" width=\"400\" height=\"326\"><param name=\"movie\" value=\"" ξurl "\"><param name=\"allowFullScreen\" value=\"true\"><param name=\"allowScriptAccess\" value=\"always\"></object>")))

(defun dailymotion-video-string (φvideo-id)
  "Return HTML code for embedding video of dailymotion.com's φvideo-id.
Example call:
 (dailymotion-linkify \"x1af0v\")"
  (concat
   "<object type=\"application/x-shockwave-flash\" data=\"http://www.dailymotion.com/swf/" φvideo-id "\" width=\"480\" height=\"360\"><param name=\"movie\" value=\"http://www.dailymotion.com/swf/" φvideo-id "\"><param name=\"allowFullScreen\" value=\"true\"></object>"
   ))

;; (defun dailymotion-video-linkify ()
;;   "Make the current line into a embeded HTML video object.

;; Example, if the current line is:
;; http://www.dailymotion.com/video/xz3am_white-rabbit-jefferson-airplane-li_music

;; it becomes
;; <object type=\"application/x-shockwave-flash\" data=\"http://www.dailymotion.com/swf/xz3am\" width=\"480\" height=\"360\"><param name=\"movie\" value=\"http://www.dailymotion.com/swf/xz3am\"><param name=\"allowFullScreen\" value=\"true\"></object>"
;; 	(interactive)
;; 	(let (ξp1 ξp2 ξinputStr ξtmp ξvID)
;;     (setq ξp1 (line-beginning-position) )
;;     (setq ξp2 (line-end-position) )
;;     (setq ξinputStr (buffer-substring-no-properties ξp1 ξp2))

;;     (setq ξtmp (replace-regexp-in-string "http://www\\.dailymotion\\.com/video/" "" ξinputStr))
;;     (setq ξtmp (car (split-string ξtmp "_")))
;;     (setq ξvID ξtmp)

;;     (delete-region ξp1 ξp2)
;;     (insert (dailymotion-video-string ξvID)
;;             ) ))

(defun vimeo-linkify ()
  "Make the current line into a embeded HTML video object.

Example, if the current line is:
http://vimeo.com/27206452

it becomes

<figure>
<iframe src=\"http://player.vimeo.com/video/27206452\" width=\"500\" height=\"294\" webkitAllowFullScreen mozallowfullscreen allowFullScreen></iframe>
<figcaption>
</figcaption>
</figure>
"
  (interactive)
  (let (ξp1 ξp2 ξinputStr ξvID)
    (setq ξp1 (line-beginning-position))
    (setq ξp2 (line-end-position))
    (setq ξinputStr (buffer-substring-no-properties ξp1 ξp2))

    ;; http://player.vimeo.com/video/5228616

    (setq ξvID
          (if (string-match "vimeo.com/video/" ξinputStr)
              (progn (string-match "https*://.*vimeo.com/video/\\([0-9]+\\)" ξinputStr)
                     (match-string 1 ξinputStr))
            (progn (string-match "https*://vimeo.com/\\([0-9]+\\)" ξinputStr)
                   (match-string 1 ξinputStr))))

    (delete-region ξp1 ξp2)
    (insert (concat
             "<figure>\n"
             "<iframe src=\"http://player.vimeo.com/video/" ξvID "\" width=\"650\" height=\"365\" webkitAllowFullScreen mozallowfullscreen allowFullScreen></iframe>\n"
             "<figcaption>
</figcaption>
</figure>"
             ))
    (search-backward "</figcaption>")))

(defun tudou-video-string (φvideo-id)
  "Return HTML code for embedding video of tudou.com's φvideo-id.
Example call:
 (tudou-video-string \"9OoINUl31dQ\")"
  (let ((ξurl (concat "http://www.tudou.com/v/" φvideo-id "/v.swf" )))
    (concat "<object type=\"application/x-shockwave-flash\" data=\"" ξurl "\" width=\"480\" height=\"400\"><param name=\"movie\" value=\"" ξurl "\"></object>" )))

(defun tudou-video-linkify ()
  "Make the current line into a embeded HTML video object.

Example, if the current line is:
http://www.tudou.com/programs/view/9OoINUl31dQ

it becomes
<object type=\"application/x-shockwave-flash\" data=\"http://www.tudou.com/v/9OoINUl31dQ/v.swf\" width=\"480\" height=\"400\"><param name=\"movie\" value=\"http://www.tudou.com/v/9OoINUl31dQ/v.swf\"></object>"

  (interactive)
  (let (ξp1 ξp2 ξinputStr ξtmp ξvID)
    (setq ξp1 (line-beginning-position))
    (setq ξp2 (line-end-position))
    (setq ξinputStr (buffer-substring-no-properties ξp1 ξp2))
    (setq ξtmp (replace-regexp-in-string "http://www\\.tudou\\.com/programs/view/" "" ξinputStr))
    (setq ξtmp (replace-regexp-in-string "/" "" ξtmp))
    (setq ξvID ξtmp)

    (delete-region ξp1 ξp2)
    (insert (tudou-video-string ξvID))))

(defun break-video-linkify ()
  "Make the current line into a embeded HTML video object.

The current line must be a embeded video HTML code from break.com.

Example:
<object width=\"464\" height=\"376\" id=\"670986\" type=\"application/x-shockwave-flash\" classid=\"clsid:D27CDB6E-AE6D-11cf-96B8-444553540000\" alt=\"Hot Asian Chick Falls Off a Stripper Pole Funny Videos\"><param name=\"movie\" value=\"http://embed.break.com/NjcwOTg2\"></param><param name=\"allowScriptAccess\" value=\"always\"></param><embed src=\"http://embed.break.com/NjcwOTg2\" type=\"application/x-shockwave-flash\" allowScriptAccess=always width=\"464\" height=\"376\"></embed></object><br><font size=1><a href=\"http://www.break.com/index/hot-asian-chick-falls-off-stripper-pole.html\" target=\"_blank\">Hot Asian Chick Falls Off a Stripper Pole</a> - Watch more <a href=\"http://www.break.com/\" target=\"_blank\">Funny Videos</a></font>

Becomes:
<object type=\"application/x-shockwave-flash\" data=\"http://embed.break.com/NjcwOTg2\" width=\"464\" height=\"376\"><param name=\"movie\" value=\"http://embed.break.com/NjcwOTg2\"><param name=\"allowScriptAccess\" value=\"always\"><param name=\"id\" value=\"670986\"><param name=\"classid=\" value=\"clsid:D27CDB6E-AE6D-11cf-96B8-444553540000\"></object>

This is valid HTML.

See: URL  `http://xahlee.info/js/html_embed_video.html'"
  (interactive)
  (let (ξp1 ξp2 ξtmp ξwidth ξheight ξvID ξclassid ξhashcode)
    (setq ξp1 (line-beginning-position))
    (setq ξp2 (line-end-position))

    (setq ξtmp (buffer-substring-no-properties ξp1 ξp2))

    (with-temp-buffer
      (insert 
       (replace-regexp-in-string "\"" "†" ξtmp) ; change double quote to something else to avoid tooth pick syndrom in regex
       )
      (goto-char (point-min))
      (search-forward-regexp "width=†\\([[:digit:]]+\\)†")
      (setq ξwidth (match-string 1))

      (search-forward-regexp "height=†\\([[:digit:]]+\\)†")
      (setq ξheight (match-string 1))

      (search-forward-regexp "id=†\\([[:digit:]]+\\)†")
      (setq ξvID (match-string 1))

      (search-forward-regexp "classid=†\\([^†]+\\)†")
      (setq ξclassid (match-string 1))

      (search-forward-regexp "http://embed\\.break\\.com/\\([^†]+\\)†")
      (setq ξhashcode (match-string 1)))

    (delete-region ξp1 ξp2)
    (insert
     (concat
      "<object type=\"application/x-shockwave-flash\" data=\"http://embed.break.com/"
      ξhashcode
      "\" width=\""
      ξwidth
      "\" height=\""
      ξheight
      "\"><param name=\"movie\" value=\"http://embed.break.com/"
      ξhashcode
      "\"><param name=\"allowScriptAccess\" value=\"always\"><param name=\"id\" value=\""
      ξvID
      "\"><param name=\"classid=\" value=\""
      ξclassid
      "\"></object>"))))

