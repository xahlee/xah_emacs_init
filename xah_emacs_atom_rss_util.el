;; -*- coding: utf-8 -*-
;; 2012-04-06
;; http://ergoemacs.org/emacs/xah_emacs_init.html
;; 〈Emacs Lisp: Updating Atom Webfeed〉 http://ergoemacs.org/emacs/elisp_update_atom.html

(require 'xah-get-thing)

(defun insert-atom-entry (&optional φtitle φid φsummary φcontent-xml-text φalt-link)
  "Insert a Atom webfeed entry template,
 in the current buffer's cursor position.

One of φsummary or φcontent-xml-text must not be `nil'.

Optional argument φalt-link is used in the atom tag: <link rel=\"alternate\" href=\"…\"/>
Default value is: http://xahlee.org/Periodic_dosage_dir/pd.html"
  (interactive)
  (let* (
         (ξtitle (if φtitle (concat "<title>" φtitle "</title>") "�") )
         (ξid (if φid φid (new-atom-id-tag) ) )
         (ξsummary (if φsummary (concat "<summary>" φsummary "</summary>\n") "") )
         (ξcontent (if φcontent-xml-text (format " <content type=\"xhtml\">
 <div xmlns=\"http://www.w3.org/1999/xhtml\">
%s
 </div>
 </content>" φcontent-xml-text)
  "") )
         (ξupdatedStr (xah-current-date-time-string))
         (ξaltLink (if φalt-link φalt-link (xahsite-filepath-to-url (replace-regexp-in-string ".xml\\'" ".html" (buffer-file-name) "FIXEDCASE" "LITERAL")) ))
         )
    (insert (format "<entry>
%s
<id>%s</id>
<updated>%s</updated>
%s%s
<link rel=\"alternate\" href=\"%s\"/>
</entry>

"
                    ξtitle
                    ξid
                    ξupdatedStr
                    ξsummary
                    ξcontent
                    ξaltLink
                    )) ) )

(defun new-atom-id-tag (&optional φdomain-name)
  "Returns a newly generated ATOM webfeed's “id” element string.
Example of return value: 「tag:xahlee.org,2010-03-31:022128」

If DOMAINNAME is given, use that for the domain name.
Else, use “xahlee.org”."
    (format "tag:%s%s" (if φdomain-name φdomain-name "xahlee.org") (format-time-string ",%Y-%m-%d:%H%M%S" (current-time) 1)) )

(defun update-atom-updated-tag (φfile-path)
  "Update the <updated> tag of a ATOM webfeed file at φfile-path,
to current date/time stamp.
This command leaves the file unsaved."
  (interactive
   (list (buffer-file-name))
   )
    (let (ξp1 ξp2)
      (find-file φfile-path)
      (goto-char 1)
      (search-forward "<updated>")
      (setq ξp1 (point) )
      (search-forward "</updated>")
      (setq ξp2 (- (point) 10) )
      (delete-region ξp1 ξp2 )
      (goto-char ξp1)
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
         (ξbds (xah-get-thing-or-selection 'block))
         (ξinputStr 
          (replace-regexp-in-string " allowfullscreen" " " (elt ξbds 0) 'FIXEDCASE 'LITERAL )) ; remove this from iframes from youtube and google map, they are invalid xml
         (ξp1 (elt ξbds 1))
         (ξp2 (elt ξbds 2))
         (ξp3)
         (ξcurrentFpath (buffer-file-name))
         (ξatomFilePath
          (if (string-match-p "wordyenglish_com/words/new.html\\'" ξcurrentFpath )
              (replace-regexp-in-string "words/new.html\\'" "lit/blog.xml" ξcurrentFpath "FIXEDCASE" "LITERAL")
            (replace-regexp-in-string "\\.html\\'" ".xml" ξcurrentFpath "FIXEDCASE" "LITERAL")))
         (ξtitleText
          (if (string-match "<h3>\\(.+?\\)</h3>" ξinputStr)
              (progn (match-string 1 ξinputStr ))
            (progn
              (if (string-match "<a href=\"\\([^\"]+?\\)\">\\([^<]+?\\)</a>" ξinputStr)
                  (progn (match-string 2 ξinputStr))
                (progn "�")))))

         (ξaltURL ; if the meat contain just one link, use that as alt url, else, url of current file name
          (let ( (ξhrefValues (xah-html-extract-url ξp1 ξp2)) ξfirstLink1)
            (if (>= (length ξhrefValues) 1)
                (progn
                  (setq ξfirstLink1 (elt ξhrefValues 0))
                  (if (string-match-p "\\`https?://" ξfirstLink1)
                      ξfirstLink1
                    ;; (if (xahsite-url-is-xah-website-p ξfirstLink1)
                    ;;     (xahsite-filepath-to-href-value
                    ;;      (xahsite-url-to-filepath ξfirstLink1 "addFileName")
                    ;;      ξcurrentFpath)
                    ;;   ξfirstLink1
                    ;;   )
                    (xahsite-filepath-to-url 
                     (expand-file-name ξfirstLink1 (file-name-directory ξcurrentFpath )))))
              (xahsite-filepath-to-url ξcurrentFpath)))))

    (if (file-exists-p ξatomFilePath)
        (find-file ξatomFilePath)
      (user-error "file doesn't exist：%s" ξatomFilePath))
    (update-atom-updated-tag (buffer-file-name))
    (goto-char 1)
    (search-forward "<entry>" nil t)
    (beginning-of-line)
    (setq ξp3 (point))
    (insert-atom-entry ξtitleText (new-atom-id-tag) nil ξinputStr ξaltURL)
    (search-backward "</title>")
  ;    (when (not (search-forward "�" nil t) ) (progn (goto-char ξp3)))
    ))
