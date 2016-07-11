;; -*- coding: utf-8 -*-
;; 2012-04-06
;; http://ergoemacs.org/emacs/xah_emacs_init.html
;; 〈Emacs Lisp: Updating Atom Webfeed〉 http://ergoemacs.org/emacs/elisp_update_atom.html

(require 'xah-get-thing)

(defun insert-atom-entry (&optional *title *id *summary *content-xml-text *alt-link)
  "Insert a Atom webfeed entry template,
 in the current buffer's cursor position.

One of *summary or *content-xml-text must not be `nil'.

Optional argument *alt-link is used in the atom tag: <link rel=\"alternate\" href=\"…\"/>
Default value is: http://xahlee.org/Periodic_dosage_dir/pd.html"
  (interactive)
  (let* (
         (-title (if *title (concat "<title>" *title "</title>") "�") )
         (-id (if *id *id (new-atom-id-tag) ) )
         (-summary (if *summary (concat "<summary>" *summary "</summary>\n") "") )
         (-content (if *content-xml-text (format " <content type=\"xhtml\">
 <div xmlns=\"http://www.w3.org/1999/xhtml\">
%s
 </div>
 </content>" *content-xml-text)
  "") )
         (-updatedStr (xah-current-date-time-string))
         (-altLink (if *alt-link *alt-link (xahsite-filepath-to-url (replace-regexp-in-string ".xml\\'" ".html" (buffer-file-name) "FIXEDCASE" "LITERAL")) ))
         )
    (insert (format "<entry>
%s
<id>%s</id>
<updated>%s</updated>
%s%s
<link rel=\"alternate\" href=\"%s\"/>
</entry>

"
                    -title
                    -id
                    -updatedStr
                    -summary
                    -content
                    -altLink
                    )) ) )

(defun new-atom-id-tag (&optional *domain-name)
  "Returns a newly generated ATOM webfeed's “id” element string.
Example of return value: 「tag:xahlee.org,2010-03-31:022128」

If DOMAINNAME is given, use that for the domain name.
Else, use “xahlee.org”."
    (format "tag:%s%s" (if *domain-name *domain-name "xahlee.org") (format-time-string ",%Y-%m-%d:%H%M%S" (current-time) 1)) )

(defun update-atom-updated-tag (*file-path)
  "Update the <updated> tag of a ATOM webfeed file at *file-path,
to current date/time stamp.
This command leaves the file unsaved."
  (interactive
   (list (buffer-file-name))
   )
    (let (-p1 -p2)
      (find-file *file-path)
      (goto-char 1)
      (search-forward "<updated>")
      (setq -p1 (point) )
      (search-forward "</updated>")
      (setq -p2 (- (point) 10) )
      (delete-region -p1 -p2 )
      (goto-char -p1)
      (insert (xah-current-date-time-string)))
 )

(defun xah-make-atom-entry ()
  "Create a Atom (RSS) entry of the current blog file.

Use current text block (between blank lines) or selected text as input, and update the Atom file's overall “updated” tag.

If the current file is〔‹path›.html〕, then ATOM file updated will be 〔‹path›.xml〕.
Exception: 〔c:/Users/h3/web/wordyenglish_com/words/new.html〕 goes to 〔~/web/wordyenglish_com/lit/blog.xml〕

Other files paths for blogs are:
~/web/ergoemacs_org/emacs/blog.html
~/web/xahlee_info/comp/blog.html
~/web/xahlee_info/js/blog.html
~/web/xahlee_info/math/blog.html
~/web/wordyenglish_com/lit/blog.html
~/web/wordyenglish_com/chinese/blog.html
~/web/xahlee_org/Periodic_dosage_dir/pd.html
~/web/xahlee_org/arts/blog.html
~/web/xahlee_org/blender/blog.html
~/web/xahlee_org/piano/blog.html
~/web/xahlee_org/lit/blog.html
~/web/xahlee_org/sex/blog.html
~/web/xahlee_org/sl/blog.html

version 2016-04-03"
  (interactive)
  (let* (
         (-bds (xah-get-thing-or-selection 'block))
         (-inputStr 
          (replace-regexp-in-string " allowfullscreen" " " (elt -bds 0) 'FIXEDCASE 'LITERAL )) ; remove this from iframes from youtube and google map, they are invalid xml
         (-p1 (elt -bds 1))
         (-p2 (elt -bds 2))
         (-p3)
         (-currentFpath (buffer-file-name))
         (-atomFilePath
          (if (string-match-p "wordyenglish_com/words/new.html\\'" -currentFpath )
              (replace-regexp-in-string "words/new.html\\'" "lit/blog.xml" -currentFpath "FIXEDCASE" "LITERAL")
            (replace-regexp-in-string "\\.html\\'" ".xml" -currentFpath "FIXEDCASE" "LITERAL")))
         (-titleText
          (if (string-match "<h3>\\(.+?\\)</h3>" -inputStr)
              (progn (match-string 1 -inputStr ))
            (progn
              (if (string-match "<a href=\"\\([^\"]+?\\)\">\\([^<]+?\\)</a>" -inputStr)
                  (progn (match-string 2 -inputStr))
                (progn "�")))))

         (-altURL ; if the meat contain just one link, use that as alt url, else, url of current file name
          (let ( (-hrefValues (xah-html-extract-url -p1 -p2)) -firstLink1)
            (if (>= (length -hrefValues) 1)
                (progn
                  (setq -firstLink1 (elt -hrefValues 0))
                  (if (string-match-p "\\`https?://" -firstLink1)
                      -firstLink1
                    ;; (if (xahsite-url-is-xah-website-p -firstLink1)
                    ;;     (xahsite-filepath-to-href-value
                    ;;      (xahsite-url-to-filepath -firstLink1 "addFileName")
                    ;;      -currentFpath)
                    ;;   -firstLink1
                    ;;   )
                    (xahsite-filepath-to-url 
                     (expand-file-name -firstLink1 (file-name-directory -currentFpath )))))
              (xahsite-filepath-to-url -currentFpath)))))

    (if (file-exists-p -atomFilePath)
        (find-file -atomFilePath)
      (user-error "file doesn't exist：%s" -atomFilePath))
    (update-atom-updated-tag (buffer-file-name))
    (goto-char 1)
    (search-forward "<entry>" nil t)
    (beginning-of-line)
    (setq -p3 (point))
    (insert-atom-entry -titleText (new-atom-id-tag) nil -inputStr -altURL)
    (search-backward "</title>")
  ;    (when (not (search-forward "�" nil t) ) (progn (goto-char -p3)))
    ))
