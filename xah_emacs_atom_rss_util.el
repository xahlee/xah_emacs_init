;; -*- coding: utf-8; lexical-binding: t; -*-
;; 2012-04-06
;; 〈Emacs Lisp: Updating Atom Webfeed〉 http://ergoemacs.org/emacs/elisp_update_atom.html

(require 'subr-x) ; string-trim
(require 'xah-get-thing)

(defun xah-atom-datetime-string ()
  "Returns current date-time string in full ISO 8601 format.
Example: 「2012-04-05T21:08:24-07:00」.

Note, for the time zone offset, both the formats 「hhmm」 and 「hh:mm」 are valid ISO 8601. However, Atom Webfeed spec seems to require 「hh:mm」."
  (concat
   (format-time-string "%Y-%m-%dT%T")
   (funcall
    (lambda ($x)
      (format "%s:%s" (substring $x 0 3) (substring $x 3 5)))
    (format-time-string "%z"))))

(defun xah-atom-insert-entry (&optional @title @id @summary @content-xml-text @alt-link)
  "Insert a Atom webfeed entry template,
 in the current buffer's cursor position.

One of @summary or @content-xml-text must not be `nil'.

Optional argument @alt-link is used in the atom tag: <link rel=\"alternate\" href=\"…\"/>
Default value is: http://xahlee.org/Periodic_dosage_dir/pd.html"
  (interactive)
  (let* (
         ($title (if @title (concat "<title>" @title "</title>") "▮") )
         ($id (if @id @id (xah-atom-new-id) ) )
         ($summary (if @summary (concat "<summary>" @summary "</summary>\n") "") )
         ($content (if @content-xml-text (format " <content type=\"xhtml\">
 <div xmlns=\"http://www.w3.org/1999/xhtml\">%s</div>
 </content>" @content-xml-text)
  "") )
         ($updatedStr (xah-atom-datetime-string))
         ($altLink (if @alt-link @alt-link (xahsite-filepath-to-url (replace-regexp-in-string ".xml\\'" ".html" (buffer-file-name) "FIXEDCASE" "LITERAL")) ))
         )
    (insert (format "<entry>
%s
<id>%s</id>
<updated>%s</updated>
%s%s
<link rel=\"alternate\" href=\"%s\"/>
</entry>

"
                    $title
                    $id
                    $updatedStr
                    $summary
                    $content
                    $altLink
                    )) ) )

(defun xah-atom-new-id (&optional @domain-name)
  "Returns a newly generated ATOM webfeed's “id” element string.
Example of return value: 「tag:xahlee,2010-03-31:022128」

If DOMAINNAME is given, use that for the domain name.
Else, use “xahlee”."
    (format "tag:%s%s" (if @domain-name @domain-name "xahlee") (format-time-string ",%Y-%m-%d:%H%M%S" (current-time) 1)) )

(defun xah-atom-update-updated-tag (@file-path)
  "Update the <updated> tag of a ATOM webfeed file at @file-path,
to current date/time stamp.
This command leaves the file unsaved."
  (interactive
   (list (buffer-file-name)))
  (let ($p1 $p2)
    (find-file @file-path)
    (goto-char 1)
    (search-forward "<updated>")
    (setq $p1 (point))
    (search-forward "</updated>")
    (setq $p2 (- (point) 10))
    (delete-region $p1 $p2 )
    (goto-char $p1)
    (insert (xah-atom-datetime-string))))

(defun xah-atom-update-entry-date ()
  "Update the date time stamp of atom rss file
when cursor is in a atom entry,
move cursor to the <updated> tag
replace the date time by current date time

Version 2019-08-03 2020-12-23"
  (interactive)
  (let (p1 p2)
    (search-backward "<updated>" )
    (search-forward ">" )
    (setq p1 (point))
    (search-forward ">" )
    (search-backward "<" )
    (setq p2 (point))
    (delete-region p1 p2)
    (insert (xah-atom-datetime-string))
    (overlay-put (make-overlay p1 (point)) 'face 'highlight)))

(defun xah-atom-new-entry ()
  "Create a Atom (RSS) entry of the current blog file.

Use current text block (between blank lines) or selected text as input, and update the Atom file's overall “updated” tag.

If the current file is x.html, then ATOM file updated will be x.xml.
Exception:
~/web/wordyenglish_com/words/new.html
 goes to
~/web/wordyenglish_com/lit/blog.xml

Version 2020-06-24"
  (interactive)
  (let* (
         $p1 $p2
         $inputStr
         $cursorLink
         ($currentFpath (buffer-file-name))
         ($atomFilePath
          (if (string-match-p "wordyenglish_com/words/new.html\\'" $currentFpath )
              (replace-regexp-in-string "words/new.html\\'" "lit/blog.xml" $currentFpath "FIXEDCASE" "LITERAL")
            (replace-regexp-in-string "\\.html\\'" ".xml" $currentFpath "FIXEDCASE" "LITERAL")))
         ($dummyTitleText "hhhhh")
         ($titleText $dummyTitleText)
         $altURL
         )
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (save-excursion
        (search-backward "<section>" )
        (search-forward "</time></div>" )
        (setq $p1 (point))
        (search-forward "</section>" )
        (search-backward "<" )
        ;; (delete-blank-lines)
        (setq $p2 (point))))
    (setq $inputStr (buffer-substring-no-properties $p1 $p2))
    (setq $cursorLink
          (if (nth 3 (syntax-ppss))
              (save-excursion
                (let (p3 p4)
                  (search-backward "\"")
                  (forward-char 1)
                  (setq p3 (point))
                  (search-forward "\"" )
                  (backward-char 1)
                  (setq p4 (point))
                  (buffer-substring-no-properties p3 p4)))
            nil
            ))

    (setq $titleText
          (if (string-match "<h3>\\(.+?\\)</h3>" $inputStr)
              (progn (match-string 1 $inputStr ))
            (progn
              (if (string-match "<a href=\"\\([^\"]+?\\)\">\\([^<]+?\\)</a>" $inputStr)
                  (progn (match-string 2 $inputStr))
                (progn $dummyTitleText)))))

    (setq $inputStr
          ;; convert html boolean attributes to valid xml version. eg from iframes from youtube and google map
          (with-temp-buffer
            (insert $inputStr)
            (goto-char (point-min))
            (while (search-forward " allowfullscreen" (point-max) t)
              (replace-match " " ))
            (goto-char (point-min))
            (while (search-forward " controls loop></video>" (point-max) t)
              (replace-match " controls=\"\" loop=\"\"></video>" ))
            (goto-char (point-min))
            (while (search-forward " controls></video>" (point-max) t)
              (replace-match " controls=\"\"></video>" ))
            (goto-char (point-min))
            (while (search-forward " loop></video>" (point-max) t)
              (replace-match " loop=\"\"></video>" ))

            (goto-char (point-min))
            (search-forward "<h3>" nil t)
            (delete-region (line-beginning-position) (line-end-position))

            (goto-char (point-min))
            (insert "\n")
            (goto-char (point-max))
            (insert "\n")
            (buffer-string)))

    ;; (message "%s" $cursorLink)
    (setq $altURL
          (if $cursorLink
              (if (string-match "^http" $cursorLink )
                  $cursorLink
                (xahsite-filepath-to-url (xahsite-web-path-to-filepath $cursorLink)))
            (if (string-match "^http" $currentFpath )
                $currentFpath
              (xahsite-filepath-to-url (xahsite-web-path-to-filepath $currentFpath)))))

    ;; (setq $altURL ; use first link, else, url of current file name
    ;;       (let ( ($hrefValues (xah-html-extract-url $p1 $p2)) $firstLink1)
    ;;         (if
    ;;             (>= (length $hrefValues) 1)
    ;;             ;; (and
    ;;             ;;  (with-temp-buffer ; 1 paragraph only
    ;;             ;;    (insert $inputStr)
    ;;             ;;    (goto-char (point-min))
    ;;             ;;    (= (count-matches "<p>" (point-min) (point-max)) 1))
    ;;             ;;  (progn ; 1 link only
    ;;             ;;    (= (length $hrefValues) 1)))
    ;;             (progn
    ;;               (setq $firstLink1 (elt $hrefValues 0))
    ;;               (if (string-match-p "\\`https?://" $firstLink1)
    ;;                   $firstLink1
    ;;                 ;; (if (xahsite-url-is-xah-website-p $firstLink1)
    ;;                 ;;     (xahsite-filepath-to-href-value
    ;;                 ;;      (xahsite-url-to-filepath $firstLink1 "addFileName")
    ;;                 ;;      $currentFpath)
    ;;                 ;;   $firstLink1
    ;;                 ;;   )
    ;;                 (xahsite-filepath-to-url
    ;;                  (expand-file-name $firstLink1 (file-name-directory $currentFpath )))))
    ;;           (xahsite-filepath-to-url $currentFpath))))

    (if (file-exists-p $atomFilePath)
        (find-file $atomFilePath)
      (user-error "file doesn't exist：%s" $atomFilePath))
    (xah-atom-update-updated-tag (buffer-file-name))
    (goto-char 1)
    (search-forward "<entry>" nil t)
    (beginning-of-line)
    (xah-atom-insert-entry $titleText (xah-atom-new-id) nil $inputStr $altURL)
    (search-backward "<div xmlns=")
    (search-forward ">")
    (skip-chars-forward " \n")
    (push-mark )
    (search-backward "</title>")
    (push-mark )
    (push-mark )))
