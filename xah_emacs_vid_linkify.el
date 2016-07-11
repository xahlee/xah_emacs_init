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
         (-p1 (line-beginning-position))
         (-p2 (line-end-position))
         (-inputStr (buffer-substring-no-properties -p1 -p2))
         -vid)
    (string-match "v=\\(.\\{11\\}\\)" -inputStr)
    (setq -vid (match-string 1 -inputStr))
    (delete-region -p1 -p2)
    (insert "<figure>\n")
    (insert (concat "<iframe width=\"640\" height=\"480\" src=\"https://www.youtube.com/embed/" -vid "?rel=0\" allowfullscreen></iframe>"))
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
  (let (-p1 -p2 -inputStr -tmp -vid -fixedurl)
    (setq -p1 (line-beginning-position))
    (setq -p2 (line-end-position))
    (setq -inputStr (buffer-substring-no-properties -p1 -p2))
    (setq -tmp (replace-regexp-in-string "http://redtube\\.com/" "" -inputStr))
    (setq -tmp (replace-regexp-in-string "http://www\\.redtube\\.com/" "" -tmp))
    (setq -tmp (replace-regexp-in-string "http://embed\\.redtube\\.com/player/?id=" "" -tmp))
    (setq -vid -tmp)
    (setq -fixedurl "http://embed.redtube.com/player/?id=")

    (delete-region -p1 -p2)

    (insert (format "<iframe src=\"http://embed.redtube.com/?id=%s\" frameborder=\"0\" width=\"651\" height=\"462\" scrolling=\"no\"></iframe>" -vid))
    ))
