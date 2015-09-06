;; 2010-06-07
;; ∑ http://xahlee.org/

(defun youtube-string (φvideo-id)
  "Return HTML code for embedding video of youtube's φvideo-id.
Example call:
 (youtube-string \"bFSS826ETlk\")"
  ;; (let (ξurl)
  ;;     (setq ξurl (concat "http://www.youtube.com/v/" φvideo-id))
  ;;     (concat
  ;;      "<object type=\"application/x-shockwave-flash\" data=\"" ξurl "\" width=\"480\" height=\"385\"><param name=\"movie\" value=\"" ξurl "\"></object>"))

  (concat "<iframe width=\"640\" height=\"480\" src=\"http://www.youtube.com/embed/" φvideo-id "?rel=0\"></iframe>"))

(defun xah-youtube-linkify ()
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
  (let* (
         (ξboundaries (bounds-of-thing-at-point 'url))
         (ξp1 (car ξboundaries))
         (ξp2 (cdr ξboundaries))
         (ξinputStr (buffer-substring-no-properties ξp1 ξp2))
         ξvID)
    (string-match "v=\\(.\\{11\\}\\)" ξinputStr)
    (setq ξvID (match-string 1 ξinputStr))

    ;; (setq ξtmp (replace-regexp-in-string "http://youtube\\.com/watch\\?v=" "" ξtmp))
    ;; (setq ξtmp (replace-regexp-in-string "http://www\\.youtube\\.com/v/" "" ξtmp))
    ;; (setq ξtmp (replace-regexp-in-string "&.+" "" ξtmp))
    ;; (setq ξtmp (replace-regexp-in-string "^http://www\\.youtube\\.com/watch\\?v=" "" ξtmp))
    ;; (setq ξvID ξtmp)
    (message "input id 「%s」" ξinputStr)
    (message "YouTube id 「%s」" ξvID)
    (delete-region ξp1 ξp2)

    (insert "<figure>\n")
    (insert (youtube-string ξvID))
    (insert "\n<figcaption>\n")
    (insert "</figcaption>\n")
    (insert "</figure>\n")
    (search-backward "</figcaption>" )
    (backward-char 1)))

(defun redtube-linkify ()
  "Make the current line into a embeded HTML video object.
The line can be a redtube ID or full URL.
Examples of input line syntax:

http://redtube.com/25635
25635

Here's a example result:
<iframe src=\"http://embed.redtube.com/?id=25635\" frameborder=\"0\" width=\"651\" height=\"462\" scrolling=\"no\"></iframe>
"
  (interactive)
  (let (ξp1 ξp2 ξinputStr ξtmp ξvID ξfixedurl)
    (setq ξp1 (line-beginning-position))
    (setq ξp2 (line-end-position))
    (setq ξinputStr (buffer-substring-no-properties ξp1 ξp2))
    (setq ξtmp (replace-regexp-in-string "http://redtube\\.com/" "" ξinputStr))
    (setq ξtmp (replace-regexp-in-string "http://www\\.redtube\\.com/" "" ξtmp))
    (setq ξtmp (replace-regexp-in-string "http://embed\\.redtube\\.com/player/?id=" "" ξtmp))
    (setq ξvID ξtmp)
    (setq ξfixedurl "http://embed.redtube.com/player/?id=")

    (delete-region ξp1 ξp2)

    (insert (format "<iframe src=\"http://embed.redtube.com/?id=%s\" frameborder=\"0\" width=\"651\" height=\"462\" scrolling=\"no\"></iframe>" ξvID))
    ))
