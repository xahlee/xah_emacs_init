;; 2010-06-07
;; ∑ http://xahlee.org/

(defun xah-youtube-linkify ()
  "Make the current line of youtube url into a embeded video.

The line can be a youtube ID “bFSS826ETlk” or full URL e.g. “http://www.youtube.com/watch?v=bFSS826ETlk”. URL with more parameters usually will work too.

Here's sample result:

 <iframe width=\"640\" height=\"480\" src=\"https://www.youtube.com/embed/X7HmltUWXgs?rel=0\" allowfullscreen></iframe>

version 2016-12-17"
  (interactive)
  (let ( -p1 -p2 -inputStr -id
             (-youtubeLinkChars "-_?.:/=&A-Za-z0-9"))
    (skip-chars-backward -youtubeLinkChars (min 1 (- (point) 100)))
    (setq -p1 (point))

    (skip-chars-forward -youtubeLinkChars (+ -p1 100))
    (setq -p2 (point))

    (setq -inputStr (buffer-substring-no-properties -p1 -p2))

    (string-match "v=\\(.\\{11\\}\\)" -inputStr)
    (setq -id (match-string 1 -inputStr))
    (delete-region -p1 -p2)
    (insert "\n<figure>\n")
    (insert (concat "<iframe width=\"640\" height=\"480\" src=\"https://www.youtube.com/embed/" -id "?rel=0\" allowfullscreen></iframe>"))
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
  (let (-p1 -p2 -inputStr -tmp -id -fixedurl)
    (setq -p1 (line-beginning-position))
    (setq -p2 (line-end-position))
    (setq -inputStr (buffer-substring-no-properties -p1 -p2))
    (setq -tmp (replace-regexp-in-string "http://redtube\\.com/" "" -inputStr))
    (setq -tmp (replace-regexp-in-string "http://www\\.redtube\\.com/" "" -tmp))
    (setq -tmp (replace-regexp-in-string "http://embed\\.redtube\\.com/player/?id=" "" -tmp))
    (setq -id -tmp)
    (setq -fixedurl "http://embed.redtube.com/player/?id=")

    (delete-region -p1 -p2)

    (insert (format "<iframe src=\"http://embed.redtube.com/?id=%s\" frameborder=\"0\" width=\"651\" height=\"462\" scrolling=\"no\"></iframe>" -id))
    ))
