;; 2010-06-07
;; ∑ http://xahlee.org/

(defun xah-view-emacs-manual-in-browser ()
  "When in `Info-mode', view the current page in browser
For example: if current node is 「(elisp) The Mark」, switch to browser and load 「http://ergoemacs.org/emacs_manual/elisp/The-Mark.html」"
  (interactive)
  (browse-url
(xahsite-url-to-filepath (emacs-info-node-string-to-url (Info-copy-current-node-name)))
 ) )

(defun emacs-info-node-string-to-url (φinfo-node-str)
  "Make the current line or selection into a emacs reference link.
For example: 「(elisp) The Mark」 ⇒ 「http://ergoemacs.org/emacs_manual/elisp/The-Mark.html」"
  (let ((domainStr "http://ergoemacs.org/")
        (tempPath
         (xah-replace-pairs-in-string φinfo-node-str [["(elisp) " ""]
                                               ["(emacs) " ""]
                                               ["-" "_002d"]
                                               [" " "-"] ] ) ) )

    (cond
     ((string-match "(elisp)" φinfo-node-str )  (format "%s%s%s.html" domainStr "emacs_manual/elisp/" tempPath))
     ((string-match "(emacs)" φinfo-node-str ) (format "%s%s%s.html" domainStr "emacs_manual/emacs/" tempPath))
     (t (user-error "φinfo-node-str 「%s」 doesn't match “(elisp)” or “(emacs)”" φinfo-node-str)) ) ) )

(defun xah-html-emacs-ref-linkify ()
  "Make the current line or selection into a emacs reference link.
For example, if the cursor is any one of the line:

 (elisp) The Mark
 file:///Users/xah/web/ergoemacs_org/emacs_manual/elisp/The-Mark.html#The-Mark
 file:///Users/xah/web/ergoemacs_org/emacs_manual/elisp/The-Mark.html
                http://ergoemacs.org/emacs_manual/elisp/The-Mark.html

                 ~/web/ergoemacs_org/emacs_manual/elisp/The-Mark.html
      c:/Users/xah/web/ergoemacs_org/emacs_manual/elisp/The-Mark.html
                               ../emacs_manual/elisp/The-Mark.html

Then it'll become:
<span class=\"ref\"><a href=\"../emacs_manual/elisp/The-Mark.html\">(info \"(elisp) The Mark\")</a></span>"
  (interactive)
  (let* (
         ξp1
         ξp2
         ξinput
         ξinfoStr linkText
         ξfPath
         ξlinkStrURL ; link string
         )

    (if (use-region-p)
        (progn (setq ξp1 (region-beginning))
               (setq ξp2 (region-end)))
      (progn (setq ξp1 (line-beginning-position))
             (setq ξp2 (line-end-position))))

    (setq ξinput (xah-trim-string (buffer-substring-no-properties ξp1 ξp2)))

    ;; generate ξinfoStr. A info string is like this: “(elisp) The Mark”
    (setq ξinfoStr
          (if
              (or
               (string-match "(emacs)" ξinput)
               (string-match "(elisp)" ξinput))
              (setq ξinfoStr ξinput)
            (let (ξfpath ξtemp)
              ;; convert local URL to file path
              (setq ξfpath
                    (cond
                     ((string-match "^file" ξinput) (local-url-to-file-path ξinput))
                     ((string-match "^http" ξinput) (xahsite-url-to-filepath ξinput))
                     (t ξinput)))

              ;; convert file path to info node syntax
              (concat
               (cond
                ((string-match "emacs_manual/elisp" ξfpath ) "(elisp) ")
                ((string-match "emacs_manual/emacs" ξfpath ) "(emacs) ")
                (t (error "ξfpath 「%s」 doesn't match “elisp” or “emacs”" ξfpath)))

               (replace-regexp-in-string "_002d" "-" (replace-regexp-in-string "-" " " (file-name-sans-extension (file-name-nondirectory ξfpath))))))))

    ;; generate link text
    (setq linkText (concat "(info \"" ξinfoStr "\")"))

    ;; generate relative file path
    (setq ξlinkStrURL
          (concat 
           (xah-replace-pairs-in-string ξinfoStr
                                    [
                                     ["(elisp) " "http://ergoemacs.org/emacs_manual/elisp/"]
                                     ["(emacs) " "http://ergoemacs.org/emacs_manual/emacs/"]
                                     [" " "-"]
                                     ["-" "_002d"]
                                     ] )
           ".html" ))

    ;; (cond
    ;;  ((string-match "(elisp)" ξinfoStr ) (setq ξlinkStrURL (concat "../emacs_manual/elisp/" ξlinkStrURL ".html")))
    ;;  ((string-match "(emacs)" ξinfoStr ) (setq ξlinkStrURL (concat "../emacs_manual/emacs/" ξlinkStrURL ".html")))
    ;;  (t (error "ξinfoStr doesn't match “(elisp)” or “(emacs)”: %s"  ξinfoStr))
    ;;  )

    (setq ξfPath (xahsite-url-to-filepath ξlinkStrURL))

    (if (file-exists-p ξfPath )
        (progn
          (delete-region ξp1 ξp2)
          (insert "<span class=\"ref\"><a href=\"" (xahsite-filepath-to-href-value ξfPath (buffer-file-name)) "\">" linkText "</a></span>"))
      (error "Generated local ξfPath 「%s」 does not point to a file" ξfPath)))
  )

(defun xah-html-php-ref-linkify ()
	"Make the current line into a PHP reference link.
If there's a text selection, use that.
For example, if the cursor is on the word:
echo
Then it'll become
<span class=\"ref\"><a href=\"http://us.php.net/echo\">echo</a></span>"
	(interactive)
	(let (bds ξp1 ξp2 swd ξurl)

    (setq bds (get-selection-or-unit 'glyphs))
    (setq swd (elt bds 0) )
    (setq ξp1 (elt bds 1) )
    (setq ξp2 (elt bds 2) )

    (setq ξurl (concat "http://us.php.net/" swd) )
    (delete-region ξp1 ξp2)
    (insert "<span class=\"ref\"><a href=\"" ξurl "\">" swd "</a></span>")))


(defun java-ref-linkify ()
	"Make the current line into a Java reference link.
If there's a text selection, use that.
For example, if the cursor is on the line:
http://java.sun.com/j2se/1.5.0/docs/api/java/lang/String.html
Then it'll become
<span class=\"ref\"><a href=\"http://java.sun.com/j2se/1.5.0/docs/api/java/lang/String.html\">Java Doc: String</a></span>

The input string can be the URL to the official Java API, Java Language Spec, the Java Tutorial."
	(interactive)
	(let (bds ξp1 ξp2 ξurl kword)

    (setq bds (get-selection-or-unit 'glyphs))
    (setq ξurl (elt bds 0) )
    (setq ξp1 (elt bds 1) )
    (setq ξp2 (elt bds 2) )

    (setq kword (file-name-sans-extension (file-name-nondirectory ξurl)))
    (delete-region ξp1 ξp2)

    (cond
     ((string-match "j2se/1.5" ξurl)
      (insert "<span class=\"ref\"><a href=\"" ξurl "\">Java Doc: " kword "</a></span>"))
     ((string-match "tutorial" ξurl)
      (insert "<span class=\"ref\"><a href=\"" ξurl "\">Java Tutorial: " kword "</a></span>"))
     ((string-match "jls" ξurl)
      (insert "<span class=\"ref\"><a href=\"" ξurl "\">Java Lang Spec: " kword "</a></span>")))))

(defun xah-html-perldoc-ref-linkify ()
  "Make the current line into a link to Perl's doc site.
For example, if the cursor is on the line:

perlop

Then it'll become

<span class=\"ref\"><a href=\"http://perldoc.perl.org/perlop.html\">perldoc perlop</a></span>"
	(interactive)
	(let (bds ξp1 ξp2 swd ξurl)
    (setq bds (get-selection-or-unit 'glyphs))
    (setq swd (elt bds 0) )
    (setq ξp1 (elt bds 1) )
    (setq ξp2 (elt bds 2) )

    (setq ξurl (concat "http://perldoc.perl.org/" swd ".html") )
    (setq ξurl (replace-regexp-in-string "::" "/" ξurl ))
    (setq ξurl (replace-regexp-in-string "-f " "functions/" ξurl ))
    (delete-region ξp1 ξp2)
    (insert "<span class=\"ref\"><a href=\"" ξurl "\">" "perldoc " swd "</a></span>")))

(defun mathematica-ref-linkify ()
	"Make the current line into a link to Mathematica ref site.
If there's a text selection, use that.
For example, if the cursor is on the line:
Table
Then it'll become:
<span class=\"ref\"><a href=\"http://reference.wolfram.com/mathematica/ref/Table.html\">Mathematica Ref: Table</a></span>"
	(interactive)
	(let (bds ξp1 ξp2 swd ξurl)

    (setq bds (get-selection-or-unit 'glyphs))
    (setq swd (elt bds 0) )
    (setq ξp1 (elt bds 1) )
    (setq ξp2 (elt bds 2) )
    (setq ξurl (concat "http://reference.wolfram.com/mathematica/ref/" swd ".html") )
    (delete-region ξp1 ξp2)
    (insert "<span class=\"ref\"><a href=\"" ξurl "\">" "Mathematica: " swd "</a></span>")))
