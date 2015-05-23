;; -*- coding: utf-8 -*-
;; 2012-04-06
;; http://ergoemacs.org/emacs/xah_emacs_init.html
;; 〈Emacs Lisp: Updating Atom Webfeed〉 http://ergoemacs.org/emacs/elisp_update_atom.html

(defun insert-atom-entry (&optional φtitle φid φsummary φcontentHTML-text φaltLinkUrl)
  "Insert a Atom webfeed entry template,
 in the current buffer's cursor position.

Optional argument φaltLinkUrl is used in the atom tag: <link rel=\"alternate\" href=\"…\"/>
Default value is: http://xahlee.org/Periodic_dosage_dir/pd.html"
  (interactive)
  (let* (
         (ξtitle (if φtitle φtitle "�") )
         (ξid (if φid φid (new-atom-id-tag) ) )
         (ξsummary (if φsummary φsummary "�") )
         (ξcontent (if φcontentHTML-text (format " <content type=\"xhtml\">
 <div xmlns=\"http://www.w3.org/1999/xhtml\">
%s
 </div>
 </content>" φcontentHTML-text)
  "") )
         (ξupdatedStr (xah-current-date-time-string))
         (ξaltLink (if φaltLinkUrl φaltLinkUrl (xahsite-filepath-to-url (replace-regexp-in-string ".xml\\'" ".html" (buffer-file-name) "FIXEDCASE" "LITERAL")) ))
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
                    ξtitle
                    ξid
                    ξupdatedStr
                    ξsummary
                    ξcontent
                    ξaltLink
                    )) ) )

(defun new-atom-id-tag (&optional φdomainName)
  "Returns a newly generated ATOM webfeed's “id” element string.
Example of return value: 「tag:xahlee.org,2010-03-31:022128」

If DOMAINNAME is given, use that for the domain name.
Else, use “xahlee.org”."
    (format "tag:%s%s" (if φdomainName φdomainName "xahlee.org") (format-time-string ",%Y-%m-%d:%H%M%S" (current-time) 1)) )

(defun update-atom-updated-tag (φfilePath)
  "Update the <updated> tag of a ATOM webfeed file at φfilePath,
to current date/time stamp.
This command leaves the file unsaved."
  (interactive
   (list (buffer-file-name))
   )
    (let (p1 p2)
      (find-file φfilePath)
      (goto-char 1)
      (search-forward "<updated>")
      (setq p1 (point) )
      (search-forward "</updated>")
      (setq p2 (- (point) 10) )
      (delete-region p1 p2 )
      (goto-char p1)
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
"
  (interactive)
  (let* (
         (bds
          (if (use-region-p)
              (vector
               (buffer-substring-no-properties (region-beginning) (region-end))
               (region-beginning) (region-end))
            (progn (let (pt1 pt2)
                     (save-excursion
                       (if (re-search-backward "\n[ \t]*\n" nil "move")
                           (progn (re-search-forward "\n[ \t]*\n")
                                  (setq pt1 (point)))
                         (setq pt1 (point)))
                       (if (re-search-forward "\n[ \t]*\n" nil "move")
                           (progn (re-search-backward "\n[ \t]*\n")
                                  (setq pt2 (point)))
                         (setq pt2 (point)))
                       (vector (buffer-substring-no-properties pt1 pt2) pt1 pt2))))))
         (ξinputStr (elt bds 0))
         (p1 (elt bds 1))
         (p2 (elt bds 2))
         (p3)
         (summaryText "…")
         (currentFilePath (buffer-file-name))
         (atomFilePath
          (if (string-match-p "wordyenglish_com/words/new.html\\'" currentFilePath )
              (replace-regexp-in-string "words/new.html\\'" "lit/blog.xml" currentFilePath "FIXEDCASE" "LITERAL")
            (replace-regexp-in-string "\\.html\\'" ".xml" currentFilePath "FIXEDCASE" "LITERAL")))
         (titleText
          (if (string-match "<h3>\\(.+?\\)</h3>" ξinputStr)
              (progn (match-string 1 ξinputStr ))
            (progn
              (if (string-match "<a href=\"\\([^\"]+?\\)\">\\([^<]+?\\)</a>" ξinputStr)
                  (progn (match-string 2 ξinputStr))
                (progn "�")))))

         (altURL ; if the meat contain just one link, use that as alt url, else, url of current file name
          (let ( (myurls (xah-html-extract-url p1 p2)) ξfirstLink1)
            (if (>= (length myurls) 1)
                (progn
                  (setq ξfirstLink1 (elt myurls 0))
                  (if (string-match-p "\\`https?://" ξfirstLink1)
                      (if (xahsite-url-is-xah-website-p ξfirstLink1)
                          (xahsite-filepath-to-href-value (xahsite-url-to-filepath ξfirstLink1 "addFileName") currentFilePath)
                        ξfirstLink1
                        )
                    (xahsite-filepath-to-href-value
                     (expand-file-name ξfirstLink1 (file-name-directory currentFilePath ))
                     currentFilePath)))
              (xahsite-filepath-to-url currentFilePath)))))

    (if (file-exists-p atomFilePath)
        (find-file atomFilePath)
      (user-error "file doesn't exist：%s" atomFilePath))
    (update-atom-updated-tag (buffer-file-name))
    (goto-char 1)
    (search-forward "<entry>" nil t)
    (beginning-of-line)
    (setq p3 (point))
    (insert-atom-entry titleText (new-atom-id-tag) summaryText ξinputStr altURL)
    (search-backward "</summary>")

  ;    (when (not (search-forward "�" nil t) ) (progn (goto-char p3)))
    ))
