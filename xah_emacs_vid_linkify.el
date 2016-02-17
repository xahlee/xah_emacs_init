;; 2010-06-07
;; ∑ http://xahlee.org/

(defun xah-youtube-linkify ()
  "Make the current line of youtube url into a embeded video.
The line can be a youtube ID or full URL.
Examples of lines:

bFSS826ETlk
http://www.youtube.com/watch?v=bFSS826ETlk

url with more parameters usually will work too.

The current line is the line the cursor is on, that is enclosed by “\n”.

Here's a example result:

<iframe width=\"640\" height=\"480\" src=\"https://www.youtube.com/embed/X7HmltUWXgs?rel=0\" allowfullscreen></iframe>

version 2016-02-10"
  (interactive)
  (let* (
         (ξp1 (line-beginning-position))
         (ξp2 (line-end-position))
         (ξinputStr (buffer-substring-no-properties ξp1 ξp2))
         ξvid)
    (string-match "v=\\(.\\{11\\}\\)" ξinputStr)
    (setq ξvid (match-string 1 ξinputStr))
    (delete-region ξp1 ξp2)
    (insert "<figure>\n")
    (insert (concat "<iframe width=\"640\" height=\"480\" src=\"https://www.youtube.com/embed/" ξvid "?rel=0\" allowfullscreen></iframe>"))
    (insert "\n<figcaption>\n")
    (insert "</figcaption>\n")
    (insert "</figure>\n")
    (search-backward "</figcaption>" )
    (backward-char 1)))

(defun xah-redtube-linkify ()
  "Make the current line into a embeded HTML video object.
The line can be a redtube ID or full URL.
Examples of input line syntax:

http://redtube.com/25635
25635

Here's a example result:
<iframe src=\"http://embed.redtube.com/?id=25635\" frameborder=\"0\" width=\"651\" height=\"462\" scrolling=\"no\"></iframe>
"
  (interactive)
  (let (ξp1 ξp2 ξinputStr ξtmp ξvid ξfixedurl)
    (setq ξp1 (line-beginning-position))
    (setq ξp2 (line-end-position))
    (setq ξinputStr (buffer-substring-no-properties ξp1 ξp2))
    (setq ξtmp (replace-regexp-in-string "http://redtube\\.com/" "" ξinputStr))
    (setq ξtmp (replace-regexp-in-string "http://www\\.redtube\\.com/" "" ξtmp))
    (setq ξtmp (replace-regexp-in-string "http://embed\\.redtube\\.com/player/?id=" "" ξtmp))
    (setq ξvid ξtmp)
    (setq ξfixedurl "http://embed.redtube.com/player/?id=")

    (delete-region ξp1 ξp2)

    (insert (format "<iframe src=\"http://embed.redtube.com/?id=%s\" frameborder=\"0\" width=\"651\" height=\"462\" scrolling=\"no\"></iframe>" ξvid))
    ))
