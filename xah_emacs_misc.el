;; -*- coding: utf-8 -*-
;; part of Xah Lee's emacs init file.
;; 2007-06, 2011-01-24
;; Xah Lee,
;; ∑ http://xahlee.org/

; some functions personal to working on XahLee.org's website
; many of these opens a particular file and insert a string



(defun xah-set-input-method-to-chinese (φn)
  "Set input method to Chinese.

Normally, set to 'chinese-py.
C-u → set to 'chinese-tonepy-punct.
C-u 2 → set to 'chinese-py-b5."
(interactive "P")
  (cond
    ((equal φn nil)     ; universal-argument not called
     (set-input-method 'chinese-py))
    ((equal φn '(4))    ; C-u
     (set-input-method 'chinese-tonepy-punct))
    ((equal φn 2)       ; C-u 2
     (set-input-method 'chinese-py-b5))
    (t                                  ; all other cases
     (set-input-method 'chinese-py))))



(defun xah-color-me-yellow ()
  "Change background color of current window to light yellow."
  (interactive)
  (set-background-color "cornsilk"))

(defun xah-color-me-pinky ()
  "Change background color of current window to light pink."
  (interactive)
  (set-background-color "lavender blush"))

(defun xah-color-me-honeydew ()
  "Change background color of current window to honeydew."
  (interactive)
  (set-background-color "honeydew"))

(defun xah-list-matching-lines2 ()
  "Show lines in the current buffer matching current word or text selection.
This command is the similar to `list-matching-lines'.
The differences are:
• The input of this command is the current word.
• If there is a text selection, that is used as input.
• The input is plain text, not regex."
  (interactive)
  (let (bds p1 p2 myStr )
    (setq bds (get-selection-or-unit 'glyphs))
    (setq myStr (elt bds 0))
    (setq p1 (elt bds 1))
    (setq p2 (elt bds 2))

    (list-matching-lines (regexp-quote myStr))))

(defun xah-make-lojban-entry ()
  "Insert a blank a-lojban-a-day HTML template in a paritcular file."
  (interactive)
(find-file "~/web/lojban/valsi_dikni/valsi_dikni.html")
(goto-char (point-min))
(re-search-forward "<!--t-->\n" nil t)
(insert (concat
"<div class=\"date\">"
(format-time-string "%Y-%m-%d")
"</div>
<p><b>renro</b> = throw = 丢 diu1</p>
<div class=\"def\">
renro:=x1 throws/launches/casts/hurls x2 to/at/in direction x3 (propulsion derives internally to x1)
</div>
<div class=\"ex\">
mi renro (le bolci ku) do = i throw ball to you = 我 丢 球qiu2 给gei3 你
</div>
<p>bolci = ball = 球. 给 = give.</p>
<pre class=\"linsi\">
• <a href=\"http://en.wiktionary.org/wiki/丢\">http://en.wiktionary.org/wiki/丢</a>
• <a href=\"http://en.wiktionary.org/wiki/给\">http://en.wiktionary.org/wiki/给</a>
</pre>
"))
(re-search-backward "<p><b>" nil t)
(re-search-forward "<p><b>" nil t))



(defvar xah-filelist nil "alist for files i need to open frequently. Key is a short abbrev, Value is file path.")
(setq xah-filelist
      '(
        ("3emacs blog" . "~/web/ergoemacs_org/emacs/blog.html" )
        ("4comp blog" . "~/web/xahlee_info/comp/blog.html" )
        ("web blog" . "~/web/xahlee_info/js/blog.html" )

        ("math blog" . "~/web/xahlee_info/math/blog.html" )
        ("wordy english blog" . "~/web/wordyenglish_com/lit/blog.html" )
        ("chinese blog" . "~/web/wordyenglish_com/chinese/blog.html" )
        ("music blog" . "~/web/xahmusic_org/music/blog.html" )
        ("arts blog" . "~/web/xaharts_org/arts/blog.html" )
        ("sl blog" . "~/web/xahsl_org/sl/blog.html" )
        ("pd blog" . "~/web/xahlee_org/Periodic_dosage_dir/pd.html" )
        ("sex blog" . "~/web/xahlee_org/sex/blog.html" )
        ("porn blog" . "~/web/xahporn_org/porn/blog.html" )

        ("twitter" . "~/Dropbox/twitter tweets.txt" )
        ("emacs keys" . "~/git/xah_emacs_init/xah_emacs_keybinding.el" )
        ("abbrev" . "~/git/xah_emacs_init/xah_emacs_abbr.el" )
        ("fly keys" . "~/git/xah-fly-keys/xah-fly-keys.el")
        ("ahk" . "~/git/xah_autohotkey_scripts/xah autohotkeys.ahk" )

        ("download" . "~/Downloads/" )
        ("pictures" . "~/Pictures/" )
        ("document" . "~/Documents/" )

        ("git" . "~/git/" )
        ("Ubuntu One" . "~/Ubuntu One/")
        ("Xah Lee Resume.html" . "~/web/xahlee_org/PageTwo_dir/Personal_dir/Xah_Lee_Resume.html")
        ("bavlamdei.txt" . "~/Dropbox/cabdei/bavlamdei.txt")
        ("check local links.pl" . "~/git/xahscripts/validate_website/check_local_links.pl")

        ("emacs tutorial update emails" . "~/Dropbox/cabdei/emacs_tutorial_update_emails.txt")
        ("xahleeinfo update emails" . "~/Dropbox/cabdei/xahlee.info_update_emails.txt")
        ("xah js tutorial update emails" . "~/Dropbox/cabdei/xah_js_tutorial_update_emails.txt")
        ("xah python tutorial update emails" . "~/Dropbox/cabdei/xah_python_tutorial_update_emails.txt")

        ("find replace.py3" . "~/git/xah_find_replace/find-replace.py3")
        ("mipri" . "~/Dropbox/zekri_open/mipri/")
        ("second life" . "/media/OS/Users/xah/Google Drive/second life/")
        ("google drive" . "/media/OS/Users/xah/Google Drive/")
        ("skydrive" . "/media/OS/Users/xah/SkyDrive/")
        ("ergoemacs init" . "~/git/ergoemacs/ergoemacs/")
        ("emacs init" . "~/.emacs.d/init.el")
        ("windows pictures" . "/media/OS/Users/xah/Pictures/")
        ("book" . "/media/OS/Users/xah/SkyDrive/books/")

        ("emacs tutorial" . "~/web/ergoemacs_org/emacs/emacs.html")
        ("elisp tutorial" . "~/web/ergoemacs_org/emacs/elisp.html")
        ("perl" . "~/web/xahlee_info/perl/perl_index.html")
        ("php" . "~/web/xahlee_info/php/php_basics.html")
        ("python" . "~/web/xahlee_info/perl-python/index.html")
        ("python3" . "~/web/xahlee_info/python/python3_basics.html")
        ("ruby" . "~/web/xahlee_info/ruby/ruby_index.html")
        ("java" . "~/web/xahlee_info/java-a-day/java.html")
        ("unicode" . "~/web/xahlee_info/comp/unicode_index.html")
        ("linux" . "~/web/xahlee_info/linux/linux_index.html" )
        ("js" . "~/web/xahlee_info/js/js.html" )
        ("svg" . "~/web/xahlee_info/js/svg.html")
        ("html" . "~/web/xahlee_info/js/index.html")
        ("css index" . "~/web/xahlee_info/js/css_index.html")
        ("comp lang index" . "~/web/xahlee_info/comp/comp_lang.html")
        ("keyboard index" . "~/web/xahlee_info/kbd/keyboarding.html")
        ("mouse index" . "~/web/xahlee_info/kbd/mouse_index.html")
        ("plane curves site" . "~/web/xahlee_info/SpecialPlaneCurves_dir/specialPlaneCurves.html")

        ("hacker cult" . "~/web/xahlee_info/comp/comp_index.html")
        ("netiquette index" . "~/web/xahlee_info/Netiquette_dir/troll.html")

        ("projection" . "~/git/mercury/emacs-projection-mode/")
        ("update_ergoemacs.sh" . "~/git/xahscripts/update_ergoemacs.sh")

        ("py2doc" . "~/web/xahlee_info/python_doc_2.7.6/index.html")
        ("py3doc" . "~/web/xahlee_info/python_doc_3.3.3/index.html")

        ("xx webdev blog" . "~/web/xahlee_info/js/xx_webdev_blog.html")
        ("xx comp blog" . "~/web/xahlee_info/comp/xx_comp_blog.html")
        ("xx emacs blog" . "~/web/ergoemacs_org/emacs/xx_emacs_blog.html")
        ("xx math blog" . "~/web/xahlee_info/math/xx_math_blog.html")
        ("xx pd blog" . "~/web/xahlee_org/Periodic_dosage_dir/xx_pd_blog.html")
        ("xx wordy english blog" . "~/web/wordyenglish_com/lit/xx_lit_blog.html")

        ("make download copy" . "~/git/xahscripts/make_download_copy/make_download_copy.el")
        ("xah site move" . "~/git/xahscripts/elisp/xah_site_move.el")

))

(defun xah-open-file-fast (φopen-code)
  "Prompt to open a file from a pre-defined set."
  (interactive
   (list (ido-completing-read "Open:" (mapcar (lambda (x) (car x)) xah-filelist))))
  (find-file (cdr (assoc φopen-code xah-filelist))))

;; (defun xah-open-file-fast (φopen-code)
;;   "Prompt to open a file from a pre-defined set."
;;   (interactive "sOpen file: [3]emacs [4]comp [j]js [m]math [l]lit [c]chinese [u]music [a]art [sl]sl [x]sex [pd]pd [k]key [h]ahk [kbd]kbd [t]tweets [uk]kbd [d]dl:")
;;   (let (ξfile )
;;     (setq ξfile
;;           (cond
;;            ((string= φopen-code "3") "~/web/ergoemacs_org/emacs/blog.html" )
;;            ((string= φopen-code "4") "~/web/xahlee_info/comp/blog.html" )
;;            ((string= φopen-code "j") "~/web/xahlee_info/js/blog.html" )
;;            ((string= φopen-code "m") "~/web/xahlee_info/math/blog.html" )
;;            ((string= φopen-code "l") "~/web/wordyenglish_com/lit/blog.html" )
;;            ((string= φopen-code "c") "~/web/wordyenglish_com/chinese/blog.html" )
;;            ((string= φopen-code "u") "~/web/xahmusic_org/music/blog.html" )
;;            ((string= φopen-code "a") "~/web/xaharts_org/arts/blog.html" )
;;            ((string= φopen-code "sl") "~/web/xahsl_org/sl/blog.html" )
;;            ((string= φopen-code "t") "~/Dropbox/twitter tweets.txt" )
;;            ((string= φopen-code "x") "~/web/xahlee_org/sex/blog.html" )
;;            ((string= φopen-code "pd") "~/web/xahlee_org/Periodic_dosage_dir/pd.html" )
;;            ((string= φopen-code "k") "~/git/xah_emacs_init/xah_emacs_keybinding.el" )
;;            ((string= φopen-code "h") "~/git/xah_autohotkey_scripts/xah autohotkeys.ahk" )
;;            ((string= φopen-code "kbd") "~/web/xahlee_info/kbd/keyboarding.html" )
;;            ((string= φopen-code "uk") "~/web/xahlee_info/kbd/keyboarding.html" )
;;            ((string= φopen-code "d") "~/Downloads/" )
;;            (t (user-error "You typed 「%s」, it doesn't associate with a file." φopen-code ))
;;            )
;;           )
;;     (find-file ξfile ) ) )

(defun xah-open-file-at-cursor ()
  "Open the file path under cursor.

If there is text selection, uses the text selection for path.

If the path is starts with “http://”, launch browser vistiting that URL, or open the corresponding file, if it's xah site.

Input path can be {relative, full path, URL}. See: `xahsite-web-path-to-filepath' for types of paths supported."
  (interactive)
  (let (
        (ξs (elt (get-selection-or-unit 'filepath) 0))
        fPath )

    (if (= (length ξs) 0)
        (progn (message "no path obtained"))
      (progn
        (setq ξs (remove-uri-fragment ξs))

        ;; convenience. if the input string start with a xah domain name, make it a url string
        (setq ξs
              (cond
               ((string-match "\\`//" ξs ) (concat "http:" ξs)) ; relative http protocal, used in css
               ((string-match "\\`ergoemacs\\.org" ξs ) (concat "http://" ξs))
               ((string-match "\\`wordyenglish\\.com" ξs ) (concat "http://" ξs))
               ((string-match "\\`xaharts\\.org" ξs ) (concat "http://" ξs))
               ((string-match "\\`xahlee\\.info" ξs ) (concat "http://" ξs))
               ((string-match "\\`xahlee\\.org" ξs ) (concat "http://" ξs))
               ((string-match "\\`xahmusic\\.org" ξs ) (concat "http://" ξs))
               ((string-match "\\`xahporn\\.org" ξs ) (concat "http://" ξs))
               ((string-match "\\`xahsl\\.org" ξs ) (concat "http://" ξs))
               (t ξs)))

        (if (string-match-p "\\`https?://" ξs)
            (if (xahsite-url-is-xah-website-p ξs)
                (let ((ξfp (xahsite-url-to-filepath ξs )))
                  (if (file-exists-p ξfp)
                      (progn (find-file ξfp ))
                    (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" ξfp)) (find-file ξs ))))
              (progn (browse-url ξs)))
          (progn ; not starting “http://”
            (let ((ξfff (xahsite-web-path-to-filepath ξs default-directory)))
              (if (file-exists-p ξfff)
                  (progn (find-file ξfff))
                (if (file-exists-p (concat ξfff ".el"))
                    (progn (find-file (concat ξfff ".el")))
                  (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" ξfff)) (find-file ξfff )))))))))))

(defun xah-open-file-from-clipboard ()
  "Open the file path from OS's clipboard.
The clipboard should contain a file path or url to xah site. Open that file in emacs."
  (interactive)
  (let (
        (ξs
         (with-temp-buffer
           (yank)
           (buffer-string)))
        fpath
        )

    (if (string-match-p "\\`http://" ξs)
        (progn
          (setq fpath (xahsite-url-to-filepath ξs "addFileName"))
          (if (file-exists-p fpath)
              (progn (find-file fpath))
            (progn (error "file doesn't exist 「%s」" fpath))))
      (progn ; not starting “http://”
        (setq ξs (remove-uri-fragment ξs))
        (setq fpath (xahsite-web-path-to-filepath ξs default-directory))
        (if (file-exists-p fpath)
            (progn (find-file fpath))
          (progn (user-error "file doesn't exist 「%s」" fpath)))))))

(defun xah-browse-url-at-point ()
"Switch to web browser and load the URL at cursor position.
This code is designed to work on Mac OS X only.

If the cursor is on a URL, visit it
http://mathforum.org/library/topics/conic_g/
for certain domain, use particular browser.

If the cursor is on like one of the following
 /somedir/somefile.html or
~/web/somedir/somefile.html
use FireFox to visit it as local file (construct the proper URL)."
 (interactive)
 (let ((myStr (elt (get-selection-or-unit 'url) 0) ))
 (setq myStr (replace-regexp-in-string "&amp;" "&" myStr))

   ;; on Mac, map specific links to particular browser
   ;; (cond
   ;;  ((string-match "flickr.com/" myStr) (shell-command (concat "open -a safari " "\"" myStr "\"")))
   ;;  ((string-match "blogspot.com/" myStr) (shell-command (concat "open -a safari " "\"" myStr "\"")))
   ;;  ((string-match "livejournal.com/" myStr) (shell-command (concat "open -a safari " "\"" myStr "\"")))
   ;;  ((string-match "yahoo.com/" myStr) (shell-command (concat "open -a safari " "\"" myStr "\"")))
   ;;  (t (browse-url myStr)))

   (browse-url myStr)
   ))



(defun xah-sync-css ()
  "copy current file to all other xahsite dirs."
  (interactive)
  (let* (
         (ξfromPath (buffer-file-name))
         (ξfromFileName (file-name-nondirectory ξfromPath ))
         ξtoPath
         )
    (mapc
     (lambda (ξx)
       (let (   )
         (setq ξtoPath (concat (xahsite-url-to-filepath (format "http://%s/" ξx)) ξfromFileName))
         (when (not (string= ξfromPath ξtoPath ))
           (if (file-exists-p ξtoPath)
               (progn
                 (copy-file ξtoPath (concat ξtoPath "~" (format-time-string "%Y%m%d_%H%M%S") "~") "OK-IF-ALREADY-EXISTS") ;backup
                 (copy-file ξfromPath ξtoPath "OK-IF-ALREADY-EXISTS")
                 (message "wrote to 「%s」." ξtoPath)
                 )
             (progn (error "logic error. The file 「%s」 doesn't exist, it should already." ξtoPath))
             )
           )
         ) ) [
   "ergoemacs.org"
   "wordyenglish.com"
   "xaharts.org"
   "xahlee.info"
   "xahlee.org"
   "xahmusic.org"
   "xahsl.org"
   ])
))

(defun xah-cite ()
  "Change the file path under cursor into title and URL.

For example, this line
 /Users/xah/web/ergoemacs_org/emacs/emacs.html
becomes
 〈Xah's Emacs Tutorial〉 @ http://ergoemacs.org/emacs/emacs.html

The title came from HTML file's title tag.
File path must be a URL scheme, full path, or relative path. See: `xahsite-web-path-to-filepath'.

This is Xah Lee's personal command assuming a particular dir structure."
  (interactive)
  (let* (
         (bds (get-selection-or-unit 'glyphs ))
         (inputStr (elt bds 0))
         (p1 (elt bds 1))
         (p2 (elt bds 2))
         (myFile (xahsite-web-path-to-filepath inputStr))
         myTitle
         )

    (if (file-exists-p myFile)
        (progn
          (setq myTitle
                (if (string-match-p ".+html\\'" myFile)
                    (xhm-get-html-file-title myFile)
                  (file-name-nondirectory myFile)))
          (setq myTitle (replace-pairs-in-string myTitle [["&amp;" "&"] ["&lt;" "<"] ["&gt;" ">" ]]))

          (delete-region p1 p2)
          (insert myTitle "\n" (xahsite-filepath-to-url myFile)))
      (progn (user-error "file doesn't exist.")))))

(defun xah-copy-url-current-file ()
  "Put the current file's URL into the kill-ring."
  (interactive)
  (let (ξurl)
    (setq ξurl (xahsite-filepath-to-url (buffer-file-name)))
    (kill-new ξurl)
    (message "URL copied %s" ξurl)))



(defun compact-uncompact-block-chinese ()
  "Remove or add line ending chars on current text block.
 (text block is delimited by blank lines)
This command is similar to a toggle of `fill-paragraph'.
When there is a text selection, act on the region."
  (interactive)

  ;; This command symbol has a property “'stateIsCompact-p”.
  (let (currentStateIsCompact (bigFillColumnVal 4333999) (deactivate-mark nil))

    (save-excursion
      ;; Determine whether the text is currently compact.
      (setq currentStateIsCompact
            (if (eq last-command this-command)
                (get this-command 'stateIsCompact-p)
              (if (> (- (line-end-position) (line-beginning-position)) fill-column) t nil)))

      (if (use-region-p)
          (if currentStateIsCompact
              (fill-region (region-beginning) (region-end))
            (save-restriction
              (narrow-to-region (region-beginning) (region-end))
              (goto-char (point-min))
              (while (search-forward "\n" nil t) (replace-match "" nil t))))
        (if currentStateIsCompact
            (fill-paragraph nil)
          (let (p1 p2) ; p1 and p2 are beginning/end of text block
            (progn
              (if (re-search-backward "\n[ \t]*\n" nil "move")
                  (progn (re-search-forward "\n[ \t]*\n")
                         (setq p1 (point)))
                (setq p1 (point)))
              (if (re-search-forward "\n[ \t]*\n" nil "move")
                  (progn (re-search-backward "\n[ \t]*\n")
                         (setq p2 (point)))
                (setq p2 (point))))
            (save-restriction
              (narrow-to-region p1 p2)
              (goto-char (point-min))
              (while (search-forward "\n" nil t) (replace-match "" nil t))))))

      (put this-command 'stateIsCompact-p (if currentStateIsCompact nil t)))))

(defcustom xah-shell-abbrev-alist nil "alist of xah's shell abbrevs" :group 'xah)
(setq xah-shell-abbrev-alist
          '(
            ("rsync1" . "rsync -z -r -v -t --delete --chmod=Dugo+x --chmod=ugo+r --exclude='*~' --exclude='.bash_history' --exclude='logs/' --exclude='xahbackup/' --exclude='.git/*' --rsh='ssh -l u40651120' ~/web/ u40651120@s168753655.onlinehome.us:~/")
            ("ssh" . "ssh -l u40651120 xahlee.org ")
            ("img1" . "convert -quality 85% ")
            ("imgScale" . "convert -scale 50% -quality 85% ")
            ("img256" . "convert +dither -colors 256 ")
            ("imgBatch" . "find . -name \"*png\" | xargs -l -i basename \"{}\" \".png\" | xargs -l -i  convert -quality 85% \"{}.png\" \"{}.jpg\"")
            ("img-bmp2png" . "find . -name \"*bmp\" | xargs -l -i basename \"{}\" \".bmp\" | xargs -l -i  convert \"{}.bmp\" \"{}.png\"")

            ("grep" . "grep -r -F 'xxx' --include='*html' ~/web")
            ("firefox" . "setsid firefox &")

            ("rm empty" . "find . -type f -empty")
            ("chmod file" . "find . -type f -exec chmod 644 {} ';'")
            ;; ("rm emacs backup~" . "find . -name \"*~\" -exec rm {} ';'")
            ("rm emacs backup~" . "find . -name \"*~\" -delete")
            ("findEmptyDir" . "find . -depth -empty -type d")
            ("rmEmptyDir" . "find . -depth -empty -type d -exec rmdir {} ';'")
            ("chmod2" . "find . -type d -exec chmod 755 {} ';'")
            ("lynx" . "lynx -dump -assume_local_charset=utf-8 -display_charset=utf-8 -width=100")
            ("viewp" . "setsid feh --randomize --recursive --auto-zoom --action \"gvfs-trash '%f'\" --geometry 1600x980+10+10 .")
            ("multimedia keys" . "<kbd>◼</kbd>, <kbd>⏯</kbd>, <kbd>⏮</kbd>, <kbd>⏭</kbd>, <kbd>🔇</kbd>")
            )
          )

(defun xah-shell-commands (φcmd-abbrev)
  "insert shell command from a list of abbrevs."
  (interactive
   (list
      (ido-completing-read "shell abbrevs:" (mapcar (lambda (x) (car x)) xah-shell-abbrev-alist) "PREDICATE" "REQUIRE-MATCH") ) )
  (progn
    (insert (cdr (assoc φcmd-abbrev xah-shell-abbrev-alist)))
    ))

(defun xah-to-xah-elisp-mode  ()
  "redo my tutorial's code elisp markup"
  (interactive)
  (xah-make-backup)
  (goto-char 1)
  (while
      (search-forward "<pre class=\"elisp\">" nil "NOERROR")
    (replace-match "<pre class=\"emacs-lisp\">" "FIXEDCASE" "LITERAL" )

    (let* (
          ( ξxx (xhm-get-precode-langCode))
          (langCode (elt ξxx 0))
          (p1 (elt ξxx 1))
          (p2 (elt ξxx 2))
          )

      (xhm-remove-span-tag-region p1 p2)
      (goto-char p1)
      (xhm-htmlize-precode xhm-lang-name-map)
       )

;; (call-interactively 'xhm-htmlize-or-de-precode)
 ) )

(defun xah-decode-percent-encoded-uri (φp1 φp2)
  "Percent decode URI for text selection."
  (interactive "r")
  (let ((myStr (buffer-substring-no-properties φp1 φp2)))
    (save-excursion
      (save-restriction
        (delete-region φp1 φp2 )
        (insert (decode-coding-string (url-unhex-string myStr ) 'utf-8))
        ) ) ))

(defun xah-decode-percent-encoded-uri-js (φp1 φp2)
  "percent decode uri for text selection
Requires a node.js script. See code."
  (interactive "r")
  (let (scriptName)
    (save-excursion
      (setq scriptName (concat "/usr/bin/node ~/git/xahscripts/emacs_uri_decode.js") )
      (shell-command-on-region φp1 φp2 scriptName nil "REPLACE" nil t)
      )
    ))

(defun xah-slide-show ()
  "start external program to do slideshow of current dir.
Linux only. Requires 「feh」 image viewer.
"
  (interactive)
  (let ()
(shell-command (format "setsid feh --randomize --recursive --auto-zoom --action \"gvfs-trash '%%f'\" --geometry 1600x1000 '%s'" (expand-file-name default-directory)) )
  ))

(defun xah-remove-paypal-unimportant-text ()
  "..."
  (interactive)
  (let ()
(replace-pairs-region 1 (point-max)
 [
["Dear Xah Lee," ""]
["Hello Xah Lee," ""]
["To see all the transaction details, please log into your PayPal account." ""]
["Thanks for using PayPal. You can now ship any items. To see all the transaction details, log in to your PayPal account." ""]
["It may take a few moments for this transaction to appear in your account." ""]
["Seller Protection - Not Eligible" ""]
["It may take a few moments for this transaction to appear in your account." ""]
["Questions? Go to the Help Center at: www.paypal.com/help." ""]
["Questions? Visit the Help Center at: www.paypal.com/help." ""]
["Thanks for using PayPal – the safer, easier way to pay and get paid online." ""]
["Please do not reply to this email. This mailbox is not monitored and you will not receive a response. For assistance, log in to your PayPal account and click Help in the top right corner of any PayPal page." ""]
["You can receive plain text emails instead of HTML emails. To change your Notifications preferences, log in to your account, go to your Profile, and click My settings." ""]
["Please keep this number for future reference, as your customer doesn't have a PayPal Transaction ID for this payment." ""]
["Lift your withdrawal and receiving limits. Log in to your PayPal account and click View limits on your Account Overview page." ""]
["Once the money's there you can:
Spend the money online at thousands of stores that accept PayPal.
Transfer it to your bank account (takes 2-3 days).
Get a PayPal Debit MasterCard." ""]
["Don't see the money in your account?" ""]
["Don’t worry - sometimes it just takes a few minutes for it to show up." ""]
["Don't worry - sometimes it just takes a few minutes for it to show up." ""]
["Sincerely,
PayPal" ""]
["Help Center:
https://www.paypal.com/us/cgi-bin/helpweb?cmd=_help" ""]
["Resolution Center:
https://www.paypal.com/us/cgi-bin/?cmd=_complaint-view
" ""]
["Security Center:
https://www.paypal.com/us/security" ""]
["Please don't reply to this email. It'll just confuse the computer that sent it and you won't get a response." ""]
["This email was sent by an automated system, so if you reply, nobody will see it. To get in touch with us, log in to your account and click \"Contact Us\" at the bottom of any page." ""]
["Copyright © 2014 PayPal, Inc. All rights reserved. PayPal is located at 2211 N. First St., San Jose, CA 95131." ""]
["Instructions to merchant:
The buyer hasn't entered any instructions." ""]
["Instructions from buyer:
None provided" ""]
["----------------------------------------------------------------" ""]
                           ]
                          )
(replace-regexp-pairs-region 1 (point-max)
 [

["Get the details
https://www.paypal.com/us/cgi-bin/\\?cmd=_view-a-trans&id=\\([0-9a-zA-Z]\\{17\\}\\)"
""]

["Important note: \\([ a-zA-Z,]+?\\) has provided an unconfirmed address. Please check the Transaction Details page for this payment to find out whether you will be covered by PayPal Seller Protection."
 ""]

["PPID PP\\([0-9]\\{3,4\\}\\) - \\([0-9a-fA-F]\\{12,13\\}\\)" ""]

["PayPal Email ID +PP\\([0-9]\\{3,4\\}\\) - \\([0-9a-fA-F]\\{12,13\\}\\)" ""]

]
"FIXEDCASE" "LITERAL")

(let* (
         (bds (get-selection-or-unit 'buffer))
         (p1 (elt bds 1))
         (p2 (elt bds 2))
         )
    (save-excursion
      (save-restriction
        (narrow-to-region p1 p2)
        (progn
          (goto-char (point-min))
          (while (search-forward-regexp "[ \t]+\n" nil "noerror")
            (replace-match "\n") ))
        (progn
          (goto-char (point-min))
          (while (search-forward-regexp "\n\n\n+" nil "noerror")
            (replace-match "\n\n") )) )) )
))

(defun xah-replace-BOM-mark-etc ()
  "Query replace Unicode some invisible Unicode chars.
The chars to be searched are:
 RIGHT-TO-LEFT MARK 8207 x200f
 ZERO WIDTH NO-BREAK SPACE 65279 xfeff

start on cursor position to end.
    "
  (interactive)
  (let ()
    (query-replace-regexp "\u200f\\|\ufeff" "")
    ))

(defun xah-show-hexadecimal-value ()
  "Prints the decimal value of a hexadecimal string under cursor.
TODO: 2014-05-23 doesn't work. something's broken.

Samples of valid input:

  ffaf
  ffff
  0xffff
  #xffff
  FFFF
  0xFFFF
  #xFFFF

Test cases
  64*0xc8+#x12c 190*0x1f4+#x258
  100 200 300   400 500 600"
  (interactive )

  (let (inputStr tempStr p1 p2
                 (case-fold-search t) )
    (save-excursion
      ;; (skip-chars-backward "0123456789abcdef")
      ;; (search-backward-regexp "[[:xdigit:]]+" nil t)
      (search-backward-regexp "[0123456789abcdef]+" nil t)
      (setq p1 (point) )
      (search-forward-regexp "[0123456789abcdef]+" nil t)
      (setq p2 (point) )

      (setq inputStr (buffer-substring-no-properties p1 p2) )

      (let ((case-fold-search nil) )
        (setq tempStr (replace-regexp-in-string "\\`0x" "" inputStr )) ; C, Perl, …
        (setq tempStr (replace-regexp-in-string "\\`#x" "" tempStr )) ; elisp …
        (setq tempStr (replace-regexp-in-string "\\`#" "" tempStr ))  ; CSS …
        )

      ;; (message "Hex 「%s」 is 「%d」" tempStr (string-to-number tempStr 16))
      (message "input 「%s」 Hex 「%s」 is 「%d」" inputStr tempStr (string-to-number tempStr 16)))))



(defun xah-uncode-chars-html ()
  "temp. print a list of unicode with special html markup.

example: put cursor on this word
U+1D400
then call this command."
  (interactive)
  (let* (
        (bds (get-selection-or-unit 'glyphs))
        (ξinput (elt bds 0) )
        (p1 (elt bds 1))
        (p2 (elt bds 2))
        (ξcodepoint (string-to-number (replace-regexp-in-string "U\\+" "" ξinput "FIXEDCASE" "LITERAL") 16) )
        )

    (insert "\n")

    (dotimes (i 26)
        (let* (
               (ξchar (+ ξcodepoint i))
               (ξu-notation (format "U+%X" ξchar) )
               (ξname (get-char-code-property ξchar 'name))
               )
              (insert (format "<mark class=\"unicode\" title=\"%s: %s\">%c</mark>\n" ξu-notation ξname ξchar))))))

