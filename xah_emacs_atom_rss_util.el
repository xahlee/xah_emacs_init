;; -*- coding: utf-8 -*-
;; 2012-04-06
;; http://ergoemacs.org/emacs/xah_emacs_init.html
;; 〈Emacs Lisp: Updating Atom Webfeed〉 http://ergoemacs.org/emacs/elisp_update_atom.html

(defun insert-atom-entry (&optional ξtitle ξid ξsummery ξcontentHTML-text altLinkUrl)
  "Insert a Atom webfeed entry template,
 in the current buffer's cursor position.

Optional argument altLinkUrl is used in the atom tag: <link rel=\"alternate\" href=\"…\"/>
Default value is: http://xahlee.org/Periodic_dosage_dir/pd.html"
  (interactive)
  (let* (
         (βtitle (if ξtitle ξtitle "�") )
         (βid (if ξid ξid (new-atom-id-tag) ) )
         (βsummery (if ξsummery ξsummery "�") )
         (βcontent (if ξcontentHTML-text (format " <content type=\"xhtml\">
 <div xmlns=\"http://www.w3.org/1999/xhtml\">
%s
 </div>
 </content>" ξcontentHTML-text)
  "") )
         (βupdatedStr (current-date-time-string))
         (βaltLink (if altLinkUrl altLinkUrl (xahsite-filepath-to-url (replace-regexp-in-string ".xml\\'" ".html" (buffer-file-name) "FIXEDCASE" "LITERAL")) ))
         )
    (insert (format "<entry>
<title>%s</title>
<id>%s</id>
<updated>%s</updated>
<summary>%s</summary>
%s
<link rel=\"alternate\" href=\"%s\"/>
</entry>

"
                    βtitle
                    ξid
                    βupdatedStr
                    βsummery
                    βcontent
                    βaltLink
                    )) ) )

(defun new-atom-id-tag (&optional domainName)
  "Returns a newly generated ATOM webfeed's “id” element string.
Example of return value: 「tag:xahlee.org,2010-03-31:022128」

If DOMAINNAME is given, use that for the domain name.
Else, use “xahlee.org”."
    (format "tag:%s%s" (if domainName domainName "xahlee.org") (format-time-string ",%Y-%m-%d:%H%M%S" (current-time) 1)) )

(defun update-atom-updated-tag (filePath)
  "Update the <updated> tag of a ATOM webfeed file at filePath,
to current date/time stamp.
This command leaves the file unsaved."
  (interactive
   (list (buffer-file-name))
   )
    (let (p1 p2)
      (goto-char 1)
      (search-forward "<updated>")
      (setq p1 (point) )
      (search-forward "</updated>")
      (setq p2 (- (point) 10) )
      (delete-region p1 p2 )
      (goto-char p1)
      (insert (current-date-time-string)))
 )

(defun xah-make-atom-entry ()
  "Create a Atom (RSS) entry of the current blog file.

Use current text block (between empty lines) or selected text as input, and update the Atom file's overall “updated” tag.

If the current file is〔‹path›.html〕, then ATOM file updated will be 〔‹path›.xml〕.
Exception: 〔c:/Users/h3/web/wordyenglish_com/words/new.html〕 goes to 〔~/web/wordyenglish_com/lit/blog.xml〕

Other files paths for blogs are:
~/web/ergoemacs_org/emacs/blog.html
~/web/xahlee_info/comp/blog.html
~/web/xahlee_info/js/blog.html
~/web/xahlee_info/math/blog.html
~/web/wordyenglish_com/lit/blog.html
~/web/xahlee_org/Periodic_dosage_dir/pd.html
~/web/xahlee_org/arts/blog.html
~/web/xahlee_org/blender/blog.html
~/web/xahlee_org/piano/blog.html
~/web/xahlee_org/lit/blog.html
~/web/xahlee_org/sex/blog.html
~/web/xahlee_org/sl/blog.html
"
  (interactive)
  (let* (
        (bds (get-selection-or-unit 'block)) 
        (inputStr (elt bds 0))
        (p1 (elt bds 1))
        (p2 (elt bds 2))
        (p3)
        (titleText
          (if (string-match "<span class=\"b[0-9]\">\\([^<]+?\\)</span>" inputStr)
              (progn (match-string 1 inputStr ))
            (progn "�") ) )
        (summeryText "�tutorial")
        (currentFilePath (buffer-file-name))
        (atomFilePath
         (if (string-match-p "wordyenglish_com/words/new.html\\'" currentFilePath )
             (replace-regexp-in-string "words/new.html\\'" "lit/blog.xml" currentFilePath "FIXEDCASE" "LITERAL")
           (replace-regexp-in-string "\\.html\\'" ".xml" currentFilePath "FIXEDCASE" "LITERAL")
           )
         )

        (altURL ; if the meat contain just one link, use that as alt url, else, url of current file name
         (let ( (myurls (extract-url inputStr)) firstLink)
           (if (>= (length myurls) 1)
               (progn
                 (setq firstLink (elt myurls (1- (length myurls))))
                 (message "first link 「%s」" firstLink)
                 (if (string-match-p "\\`http://" firstLink)
                     (if (xahsite-url-is-xah-website-p firstLink)
                         (xahsite-filepath-to-href-value (xahsite-url-to-filepath firstLink "addFileName") currentFilePath)
                       firstLink
                       )
                   (xahsite-filepath-to-href-value
                    (expand-file-name firstLink (file-name-directory currentFilePath ))
                    currentFilePath)
                   )
                 )
             (xahsite-filepath-to-url currentFilePath) ) ) )
         )
    (find-file atomFilePath)
    (goto-char 1)
    (search-forward "<entry>" nil t)
    (beginning-of-line)
    (setq p3 (point) )
    (insert-atom-entry titleText (new-atom-id-tag) summeryText inputStr altURL)
    (update-atom-updated-tag (buffer-file-name))
    (when (not (search-forward "�" nil t) ) (progn (goto-char p3)))
    )
  )
