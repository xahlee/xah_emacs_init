;; 2010-06-07
;; ∑ http://xahlee.org/


(defun xah-view-emacs-manual-in-browser ()
  "When in `Info-mode', view the current page in browser
For example: if current node is 「(elisp) The Mark」, switch to browser and load 「http://ergoemacs.org/emacs_manual/elisp/The-Mark.html」

2016-07-31"
  (interactive)
  (browse-url
   (xahsite-url-to-filepath
    (emacs-info-node-string-to-url
     (Info-copy-current-node-name)))))

(defun emacs-info-node-string-to-url (@info-node-str)
  "change the @info-node-str into a xah emacs doc link.
For example: 「(elisp) The Mark」 ⇒ 「http://ergoemacs.org/emacs_manual/elisp/The-Mark.html」"
  (let (($domainStr "http://ergoemacs.org/")
        ($tempPath
         (xah-replace-pairs-in-string
          @info-node-str
          [["(elisp) " ""]
           ["(emacs) " ""]
           ["-" "_002d"]
           [" " "-"] ] )))

    (cond
     ((string-match "(elisp)" @info-node-str )
      (format "%s%s%s.html" $domainStr "emacs_manual/elisp/" $tempPath))
     ((string-match "(emacs)" @info-node-str )
      (format "%s%s%s.html" $domainStr "emacs_manual/emacs/" $tempPath))
     (t
      (user-error "@info-node-str 「%s」 doesn't match “(elisp)” or “(emacs)”" @info-node-str)))))

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
<span class=\"ref\"><a href=\"../emacs_manual/elisp/The-Mark.html\">(info \"(elisp) The Mark\")</a></span>
Version 2016-12-22"
  (interactive)
  (require 'subr-x) ; for string-trim
  (let* (
         ($bounds (xah-get-bounds-of-thing-or-region 'url))
         ($p1 (car $bounds))
         ($p2 (cdr $bounds))
         ( $input (string-trim (buffer-substring-no-properties $p1 $p2)))
         $infoStr $linkText
         $fPath
         $linkStrURL ; link string
         )
    ;; generate -infoStr. A info string is like this: “(elisp) The Mark”
    (setq $infoStr
          (if
              (or
               (string-match "(emacs)" $input)
               (string-match "(elisp)" $input))
              (setq $infoStr $input)
            (let ($fpath $temp)
              ;; convert local URL to file path
              (setq $fpath
                    (cond
                     ((string-match "^file" $input) (xah-local-url-to-file-path $input))
                     ((string-match "^http" $input) (xahsite-url-to-filepath $input))
                     (t $input)))

              ;; convert file path to info node syntax
              (concat
               (cond
                ((string-match "emacs_manual/elisp" $fpath ) "(elisp) ")
                ((string-match "emacs_manual/emacs" $fpath ) "(emacs) ")
                (t (error "$fpath 「%s」 doesn't match “elisp” or “emacs”" $fpath)))

               (replace-regexp-in-string "_002d" "-" (replace-regexp-in-string "-" " " (file-name-sans-extension (file-name-nondirectory $fpath))))))))

    ;; generate link text
    (setq $linkText (concat "(info \"" $infoStr "\")"))

    ;; generate relative file path
    (setq $linkStrURL
          (concat
           (xah-replace-pairs-in-string $infoStr
                                        [
                                         ["(elisp) " "http://ergoemacs.org/emacs_manual/elisp/"]
                                         ["(emacs) " "http://ergoemacs.org/emacs_manual/emacs/"]
                                         [" " "-"]
                                         ["-" "_002d"]
                                         ] )
           ".html" ))

    ;; (cond
    ;;  ((string-match "(elisp)" $infoStr ) (setq $linkStrURL (concat "../emacs_manual/elisp/" $linkStrURL ".html")))
    ;;  ((string-match "(emacs)" $infoStr ) (setq $linkStrURL (concat "../emacs_manual/emacs/" $linkStrURL ".html")))
    ;;  (t (error "$infoStr doesn't match “(elisp)” or “(emacs)”: %s"  $infoStr))
    ;;  )

    (setq $fPath (xahsite-url-to-filepath $linkStrURL))

    (if (file-exists-p $fPath )
        (progn
          (delete-region $p1 $p2)
          (insert "<span class=\"ref\"><a href=\""
                  (xahsite-filepath-to-href-value $fPath (buffer-file-name))
                  "\">" $linkText "</a></span>"))
      (error "Generated local $fPath 「%s」 does not point to a file" $fPath))))

(defun xah-html-php-ref-linkify ()
  "Make the current line into a PHP reference link.
If there's a text selection, use that.
For example, if the cursor is on the word:
echo
Then it'll become
<span class=\"ref\"><a href=\"http://us.php.net/echo\">echo</a></span>"
  (interactive)
  (let ()
    (message "todo needs rewrite" )))

(defun xah-html-perldoc-ref-linkify ()
  "Make the current line or selection into a link to Perl's doc site.
For example, if the cursor is on the line:

perlop

Then it'll become

<span class=\"ref\"><a href=\"http://perldoc.perl.org/perlop.html\">perldoc perlop</a></span>"
  (interactive)
  (let ($p1 $p2 $swd $url)
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (setq $p1 (line-beginning-position) $p2 (line-end-position)))

    (setq $swd (buffer-substring-no-properties $p1 $p2))
    (setq $url (replace-regexp-in-string
                "-f " "functions/"
                (replace-regexp-in-string
                 "::" "/"
                 (concat "http://perldoc.perl.org/" $swd ".html"))))
    (delete-region $p1 $p2)
    (insert "<span class=\"ref\"><a href=\"" $url "\">" "perldoc " $swd "</a></span>")))

(defun mathematica-ref-linkify ()
  "Make the current word into a link to Mathematica ref site.
For example, if the cursor is on the line:
Table
Then it'll become:
<span class=\"ref\"><a href=\"http://reference.wolfram.com/mathematica/ref/Table.html\">Mathematica Ref: Table</a></span>"
  (interactive)
  (let ($bds $p1 $p2 $swd $url)
    (setq $bds (thing-at-point 'word))
    (setq $p1 (car $bds))
    (setq $p2 (cdr $bds))
    (setq $swd (buffer-substring-no-properties $p1 $p2))
    (setq $url (concat "http://reference.wolfram.com/mathematica/ref/" $swd ".html"))
    (delete-region $p1 $p2)
    (insert "<span class=\"ref\"><a href=\"" $url "\">" "Mathematica: " $swd "</a></span>")))
