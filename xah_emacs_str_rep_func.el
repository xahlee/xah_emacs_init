;-*- coding: utf-8 -*-
; some elisp string replacement functions
; used for regex replace operations

; 2007-06
;   Xah Lee
; ∑ http://xahlee.org/

(defun xah-wikipedia-link-replacement ()
  "Returns a canonical form of Wikipedia link from a regex match.

This function is used for query-replace-regexp, to turn the
following forms of links:

<a href=\"http://en.wikipedia.org/wiki/event\">event</a>
<a href=\"http://en.wikipedia.org/wiki/Middle_distance\">Middle_distance</a>
<a href=\"http://en.wikipedia.org/wiki/Middle_distance_track_event\">Middle_distance_track_event</a>
<a href=\"http://en.wikipedia.org/wiki/Sapir-Whorf_Hypothesis\">Sapir-Whorf_Hypothesis</a>

into a cannonical form. Basically, the link text needs to have
“_” replaced by space. Also, it shouldn't match links that's
already in canonical form, nor matching non-wikipedia link texts.

The regex to be used for this function is:
<a href=\"http://\\(..\\)\\.wikipedia.org/wiki/\\([^\"]+\\)\">\\(\\([-.A-Za-z0-9]+_\\)+[-.A-Za-z0-9]+ ?\\)</a>

To use a function in query-replace-regexp, do “\\,(xah-wikipedia-link-replacement)”.
."
  (let (langCode articlePath linkText linkText2 ξreturnText)
    (setq langCode (buffer-substring (match-beginning 1) (match-end 1)))
    (setq articlePath (buffer-substring (match-beginning 2) (match-end 2)))
    (setq linkText (buffer-substring (match-beginning 3) (match-end 3)))
    (setq linkText2
          (replace-regexp-in-string "_" " " articlePath))
    (setq ξreturnText
          (concat "<a href=\"http://"
                  langCode ".wikipedia.org/wiki/"
                  articlePath "\">" linkText2 "</a>" ))
    ξreturnText
    )
  )

(defun get-html-h1 ()
  "Returns the current buffer's first <h1></h1> tag content.

This function is used to get a set of HTML page's <title> tag
content in sync with the <h1> tag content. This function is used
for dired-do-query-replace-regex.

The search regex should be:
<title>\([^<]+\)</title>

The replace string should be:
<title>\,(get-html-h1)</title>"
  (interactive)
  (let (p1 p2)
  ; move to the beginning
  ; search for <h1>, get position
  ; search for </h1>, get position
  ; get the text between positions
    (save-excursion
      (goto-char (point-min))
      (search-forward "<h1>" nil t)
      (setq p1 (point))
      (search-forward "</h1>" nil t)
      (search-backward "</h1>" nil t)
      (setq p2 (point)))
    (buffer-substring-no-properties p1 p2)))

(defun xah-remove-square-brackets  ()
  "Delete any text of the form “[‹n›]”.

Works on text selection or current text block.

For example
 「… was officially announced as Blu-ray Disc [11][12], and …」
becomes
 「… was officially announced as Blu-ray Disc, and …」."
  (interactive)

  (let (p1 p2 ξinputStr ξresultStr ξchangedItems)
    (save-excursion ; set p1 p2
      (if (re-search-backward "\n[ \t]*\n" nil "move")
          (progn (re-search-forward "\n[ \t]*\n")
                 (setq p1 (point)))
        (setq p1 (point)))
      (if (re-search-forward "\n[ \t]*\n" nil "move")
          (progn (re-search-backward "\n[ \t]*\n")
                 (setq p2 (point)))
        (setq p2 (point))))

    (setq ξinputStr p1 p2 )

    (setq ξchangedItems '())

    (setq ξresultStr
          (with-temp-buffer
            (insert ξinputStr)

            (goto-char 1)
            (while (search-forward-regexp "\\(\\[[0-9]+?\\]\\)" nil t)
              (setq ξchangedItems (cons (match-string 1) ξchangedItems ))
              (replace-match "" t))

            (goto-char 1)
            (while (search-forward "[citation needed]" nil t)
              (setq ξchangedItems (cons "[citation needed]" ξchangedItems ))
              (backward-char 17)
              (delete-char 17))

            (buffer-string)))

    (if (> (length ξchangedItems) 0)
        (progn
          (delete-region p1 p2)
          (insert ξresultStr)

          (message "Removed: %S" ξchangedItems )

          ;; (message "%S"
          ;;                    (mapcar
          ;;                     (lambda (ξx)
          ;;                       (concat ξx "\n")
          ;;                       ) ξchangedItems) )

          ;; (with-output-to-temp-buffer "*changed items*"
          ;;             (mapcar
          ;;              (lambda (ξx)
          ;;                (princ ξx)
          ;;                (princ "\n") ) ξchangedItems) )

          )
      (message "No change.")  ) ) )
