;; -*- coding: utf-8; lexical-binding: t; -*-
;; 2012-04-06
;; 〈Emacs Lisp: Updating Atom Webfeed〉 http://ergoemacs.org/emacs/elisp_update_atom.html

(require 'subr-x) ; string-trim
(require 'xah-get-thing)

(defun insert-atom-entry (&optional @title @id @summary @content-xml-text @alt-link)
  "Insert a Atom webfeed entry template,
 in the current buffer's cursor position.

One of @summary or @content-xml-text must not be `nil'.

Optional argument @alt-link is used in the atom tag: <link rel=\"alternate\" href=\"…\"/>
Default value is: http://xahlee.org/Periodic_dosage_dir/pd.html"
  (interactive)
  (let* (
         ($title (if @title (concat "<title>" @title "</title>") "▮") )
         ($id (if @id @id (xah-new-atom-id-tag) ) )
         ($summary (if @summary (concat "<summary>" @summary "</summary>\n") "") )
         ($content (if @content-xml-text (format " <content type=\"xhtml\">
 <div xmlns=\"http://www.w3.org/1999/xhtml\">%s</div>
 </content>" @content-xml-text)
  "") )
         ($updatedStr (xah-current-date-time-string))
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

(defun xah-new-atom-id-tag (&optional @domain-name)
  "Returns a newly generated ATOM webfeed's “id” element string.
Example of return value: 「tag:xahlee.org,2010-03-31:022128」

If DOMAINNAME is given, use that for the domain name.
Else, use “xahlee.org”."
    (format "tag:%s%s" (if @domain-name @domain-name "xahlee.org") (format-time-string ",%Y-%m-%d:%H%M%S" (current-time) 1)) )

(defun update-atom-updated-tag (@file-path)
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
    (insert (xah-current-date-time-string))))

(defun xah-make-atom-entry ()
  "Create a Atom (RSS) entry of the current blog file.

Use current text block (between blank lines) or selected text as input, and update the Atom file's overall “updated” tag.

If the current file is x.html, then ATOM file updated will be x.xml.
Exception:
~/web/wordyenglish_com/words/new.html
 goes to
~/web/wordyenglish_com/lit/blog.xml

Version 2018-08-15"
  (interactive)
  (let* (
         $p1 $p2
         $inputStr
         ($currentFpath (buffer-file-name))
         ($atomFilePath
          (if (string-match-p "wordyenglish_com/words/new.html\\'" $currentFpath )
              (replace-regexp-in-string "words/new.html\\'" "lit/blog.xml" $currentFpath "FIXEDCASE" "LITERAL")
            (replace-regexp-in-string "\\.html\\'" ".xml" $currentFpath "FIXEDCASE" "LITERAL")))
         ($dummyTitleText "hhh")
         ($titleText $dummyTitleText)
         $altURL
         )
    (if (use-region-p)
        (progn
          (setq $p1 (region-beginning))
          (setq $p2 (region-end)))
      (save-excursion
        (search-backward "<section>" )
        (search-forward "</time></div>" )
        (setq $p1 (point))
        (search-forward "</section>" )
        (search-backward "<" )
        ;; (delete-blank-lines)
        (setq $p2 (point))))
    (setq $inputStr (buffer-substring-no-properties $p1 $p2))
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
            (insert "\n")
            (goto-char (point-max))
            (insert "\n")
            (buffer-string)))

    (setq $titleText
          (if (string-match "<h3>\\(.+?\\)</h3>" $inputStr)
              (progn (match-string 1 $inputStr ))
            (progn
              (if (string-match "<a href=\"\\([^\"]+?\\)\">\\([^<]+?\\)</a>" $inputStr)
                  (progn (match-string 2 $inputStr))
                (progn $dummyTitleText)))))

    (setq $altURL ; use first link, else, url of current file name
          (let ( ($hrefValues (xah-html-extract-url $p1 $p2)) $firstLink1)
            (if
                (>= (length $hrefValues) 1)
                ;; (and
                ;;  (with-temp-buffer ; 1 paragraph only
                ;;    (insert $inputStr)
                ;;    (goto-char (point-min))
                ;;    (= (count-matches "<p>" (point-min) (point-max)) 1))
                ;;  (progn ; 1 link only
                ;;    (= (length $hrefValues) 1)))
                (progn
                  (setq $firstLink1 (elt $hrefValues 0))
                  (if (string-match-p "\\`https?://" $firstLink1)
                      $firstLink1
                    ;; (if (xahsite-url-is-xah-website-p $firstLink1)
                    ;;     (xahsite-filepath-to-href-value
                    ;;      (xahsite-url-to-filepath $firstLink1 "addFileName")
                    ;;      $currentFpath)
                    ;;   $firstLink1
                    ;;   )
                    (xahsite-filepath-to-url
                     (expand-file-name $firstLink1 (file-name-directory $currentFpath )))))
              (xahsite-filepath-to-url $currentFpath))))

    (if (file-exists-p $atomFilePath)
        (find-file $atomFilePath)
      (user-error "file doesn't exist：%s" $atomFilePath))
    (update-atom-updated-tag (buffer-file-name))
    (goto-char 1)
    (search-forward "<entry>" nil t)
    (beginning-of-line)
    (insert-atom-entry $titleText (xah-new-atom-id-tag) nil $inputStr $altURL)
    (search-backward "</title>")))
