;; -*- coding: utf-8 -*-
;; part of Xah Lee's emacs init file.
;; 2007-06, 2011-01-24
;; Xah Lee,
;; ∑ http://xahlee.org/

; some functions personal to working on XahLee.org's website
; many of these opens a particular file and insert a string



(defun set-input-method-to-chinese (ξn)
  "Set input method to Chinese.

Normally, set to 'chinese-py.
C-u → set to 'chinese-tonepy-punct.
C-u 2 → set to 'chinese-py-b5."
(interactive "P")
  (cond
    ((equal ξn nil)     ; universal-argument not called
     (set-input-method 'chinese-py))
    ((equal ξn '(4))    ; C-u
     (set-input-method 'chinese-tonepy-punct))
    ((equal ξn 2)       ; C-u 2
     (set-input-method 'chinese-py-b5))
    (t                                  ; all other cases
     (set-input-method 'chinese-py)) )
 )

(defun browse-url-of-buffer-with-firefox ()
  "Same as `browse-url-of-buffer' but using Firefox.
You need Firefox's path in the path environment variable within emacs.
e.g.
 (setenv \"PATH\" (concat \"C:/Program Files (x86)/Mozilla Firefox/\" \";\" (getenv \"PATH\") ) )
On Mac OS X, you don't need to. This command makes this shell call:
 「open -a Firefox.app http://example.com/」"
  (interactive)
  (let ()
    (cond
     ((string-equal system-type "windows-nt") ; Windows
      (shell-command (concat "firefox file://" buffer-file-name))
      )
     ((string-equal system-type "gnu/linux")
      (shell-command (concat "firefox file://" buffer-file-name))
      )
     ((string-equal system-type "darwin") ; Mac
      (shell-command (concat "open -a Firefox.app file://" buffer-file-name))
       ) )
    ))

(defun browse-url-Google-Chrome (uri)
  "Same as `browse-url' but using Google Chrome."
  (interactive)
  (let ()
    (shell-command (concat "chrome " uri))
    ))

(defun browse-url-Opera (uri)
  "Same as `browse-url' but using Opera browser."
  (interactive)
  (let ()
    (shell-command (concat "opera " uri))
    ))

(defun browse-url-of-buffer-with-firefox-2 ()
  "Same as `browse-url-of-buffer' but using Firefox.
You need to have the firefox path in `exec-path'. e.g.:
 (add-to-list 'exec-path \"c:/Program Files (x86)/Mozilla Firefox/\")"
  (interactive)
  (let ()
    (require 'browse-url)
    (browse-url-firefox (concat "file:///" buffer-file-name))
    ))



(defun yellowMe ()
  "temp function. change background color of current frame to light yellow.."
  (interactive)
  (set-background-color "cornsilk")
  )

(defun pinkMe ()
  "temp function. change background color of current frame to light pink.."
  (interactive)
  (set-background-color "lavender blush")
  )

(defun list-matching-lines2 ()
  "Show lines in the current buffer matching current word or text selection.
This command is the similar to `list-matching-lines'.
The differences are:
• The input of this command is the current word.
• If there is a text selection, that is used as input.
• The input is plain text, not regex."
  (interactive)
  (let (bds p1 p2 myStr )
    (setq bds (get-selection-or-unit 'glyphs))
    (setq myStr (elt bds 0) )
    (setq p1 (elt bds 1) )
    (setq p2 (elt bds 2) )

    (list-matching-lines (regexp-quote myStr))
    )
  )

(defun delete-secondlife-cache ()
  "Delete Second Life's cache directory."
  (interactive)
  (when (string-equal system-type "windows-nt")
    (w32-shell-execute "explore" "c:/Users/xah/AppData/Local/SecondLife/")
    (w32-shell-execute "explore" "c:/Users/xah/AppData/Local/PhoenixViewer/")
;; "~/Documents/autohotkey/delete Second Life cache.ahk"
    ;; (shell-command "rm -R c:/Users/xah/AppData/Local/SecondLife/ &")
    )
 )

(defun make-lojban-entry ()
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



(defun xah-open-file-fast (openCode)
  "Prompt to open a file from a pre-defined set."
  (interactive "sOpen file: [3]emacs [4]comp [j]js [m]math [l]lit [c]chinese [u]music [a]art [sl]sl [x]sex [p]porn [pd]pd [k]key [h]ahk [kbd]kbd [t]tweets [uk]kbd [d]dl:")
  (let (ξfile )
    (setq ξfile
          (cond
           ((string= openCode "3") "~/web/ergoemacs_org/emacs/blog.html" )
           ((string= openCode "4") "~/web/xahlee_info/comp/blog.html" )
           ((string= openCode "j") "~/web/xahlee_info/js/blog.html" )
           ((string= openCode "m") "~/web/xahlee_info/math/blog.html" )
           ((string= openCode "l") "~/web/wordyenglish_com/lit/blog.html" )
           ((string= openCode "c") "~/web/wordyenglish_com/chinese/blog.html" )
           ((string= openCode "u") "~/web/xahmusic_org/music/blog.html" )
           ((string= openCode "a") "~/web/xaharts_org/arts/blog.html" )
           ((string= openCode "sl") "~/web/xahsl_org/sl/blog.html" )
           ((string= openCode "t") "~/Dropbox/twitter tweets.txt" )
           ((string= openCode "x") "~/web/xahlee_org/sex/blog.html" )
           ((string= openCode "p") "~/web/xahporn_org/porn/blog.html" )
           ((string= openCode "pd") "~/web/xahlee_org/Periodic_dosage_dir/pd.html" )
           ((string= openCode "k") "~/git/xah_emacs_init/xah_emacs_keybinding.el" )
           ((string= openCode "h") "~/git/xah_autohotkey_scripts/xah autohotkeys.ahk" )
           ((string= openCode "kbd") "~/web/xahlee_info/kbd/keyboarding.html" )
           ((string= openCode "uk") "~/web/xahlee_info/kbd/keyboarding.html" )
           ((string= openCode "d") "~/Downloads/" )
           (t (user-error "You typed 「%s」, it doesn't associate with a file." openCode ))
           )
          )
    (find-file ξfile ) ) )

(defun xah-open-file-at-cursor ()
  "Open the file path under cursor.

If there is text selection, uses the text selection for path.

If the path is starts with “http://”, and if it's xah site, convert to file path and open the file, else launch browser vistiting that URL.

Input path can be {relative, full path, URL}. See: `xahsite-web-path-to-filepath' for types of paths supported."
  (interactive)
  (let (
        (ξs (elt (get-selection-or-unit 'filepath) 0))
         fPath )
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
           (t ξs) ) )

    (if (string-match-p "\\`https*://" ξs)
        (if (xahsite-url-is-xah-website-p ξs)
            (let ((ξfp (xahsite-url-to-filepath ξs )))
              (if (file-exists-p ξfp)
                  (progn (find-file ξfp ))
                (progn (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" ξfp) )
                         (progn (find-file ξfff ))))
                )
              )
          (progn (browse-url ξs))
          )
      (progn ; not starting “http://”
        (let ((ξfff (xahsite-web-path-to-filepath ξs default-directory)) )
          (if (file-exists-p ξfff)
              (progn (find-file ξfff))
            (if (file-exists-p (concat ξfff ".el"))
                (progn (find-file (concat ξfff ".el")))
              (progn
                (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" ξfff) )
                  (progn (find-file ξfff ))) ) ) ) ) ) ) ))

(defun xah-open-file-from-clipboard ()
  "Open the file path from OS's clipboard.
The clipboard should contain a file path or url to xah site. Open that file in emacs."
  (interactive)
  (let (
        (ξs
         (with-temp-buffer
           (yank)
           (buffer-string) ) )
        fpath
        )

    (if (string-match-p "\\`http://" ξs)
        (progn
          (setq fpath (xahsite-url-to-filepath ξs "addFileName") )
          (if (file-exists-p fpath)
              (progn (find-file fpath) )
            (progn (error "file doesn't exist 「%s」" fpath))
            )
          )
      (progn ; not starting “http://”
        (setq ξs (remove-uri-fragment ξs) )
        (setq fpath (xahsite-web-path-to-filepath ξs default-directory) )
        (if (file-exists-p fpath)
            (progn (find-file fpath) )
          (progn (error "file doesn't exist 「%s」" fpath))
          )
        ) ) ))

(defun xah-browse-url-at-point ()
"Switch to web browser and load the URL at point.
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
         ) ) (xahsite-domain-names))
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
         (inputStr (elt bds 0) )
         (p1 (elt bds 1) )
         (p2 (elt bds 2) )
         myFile myTitle
         )
    (setq myFile (xahsite-web-path-to-filepath inputStr))
    (setq myTitle
          (if (string-match-p ".+html\\'" myFile)
              (xhm-get-html-file-title myFile)
            (file-name-nondirectory myFile)))
    (setq myTitle (replace-pairs-in-string myTitle [["&amp;" "&"] ["&lt;" "<"] ["&gt;" ">" ]]) )

    (delete-region p1 p2)
    (insert "〈" myTitle "〉\n" (xahsite-filepath-to-url myFile))
    ))

(defun xah-copy-url-current-file ()
  "Put the current file's URL into the kill-ring."
  (interactive)
  (let (ξurl)
    (setq ξurl (xahsite-filepath-to-url (buffer-file-name)) )
    (kill-new ξurl)
    (message "URL copied %s" ξurl)
    )
  )



(defun xah-browse-url-of-buffer ()
  "Similar to `browse-url-of-buffer' but visit xahlee.org.

save the file first.
Then, if `universal-argument' is called, visit the corresponding xahsite URL.
For example, if current buffer is of this file:
 ~/web/xahlee_info/index.html
then after calling this function,
default browser will be launched and opening this URL:
 http://xahlee.info/index.html"
  (interactive)
  (let (myURL)
    (save-buffer)
    (if current-prefix-arg
        (progn
          (setq myURL
                (xahsite-filepath-to-url (buffer-file-name))))
      (progn
        (setq myURL
              (buffer-file-name)))
      )

    (cond
     ((string-equal system-type "windows-nt") ; Windows
      (when (string-match "^c:/" myURL) (setq myURL (concat "file:///" myURL)))
      )
     ((string-equal system-type "gnu/linux")
      t
      )
     ((string-equal system-type "darwin") ; Mac
      t
      ) )
    (browse-url-firefox myURL )
    ;; (browse-url myURL )
    )
  )



(defun xah-compact-uncompact-block ()
  "Remove or insert newline characters on the current block of text.
This is similar to a toggle for `fill-paragraph' and `unfill-paragraph'.

When there is a text selection, act on the the selection, else, act on a block of text separated by newlines."
  (interactive)

  ;; This command symbol has a property “'stateIsCompact-p”, the possible values are t and nil. This property is used to easily determine whether to compact or uncompact, when this command is called again

  (let ( currentStateIsCompact
         (deactivate-mark nil)
         (bigFillColumnVal 4333999)
         (myWhiteSpaces "\n[ \t]*\n")
         )

    (save-excursion

      ;; Determine whether the text is currently compact.
      (setq currentStateIsCompact
            (if (eq last-command this-command)
                (get this-command 'stateIsCompact-p)
              (if (> (- (line-end-position) (line-beginning-position)) fill-column) t nil) ))

      (if (region-active-p)
          (if currentStateIsCompact
              (fill-region (region-beginning) (region-end))
            (let ((fill-column bigFillColumnVal))
              (fill-region (region-beginning) (region-end)))
            )

        (let (p1 p2)
          (progn
            ;; set p1 p2 as boundary of text block
            (if (re-search-backward myWhiteSpaces nil "move")
                (progn (re-search-forward myWhiteSpaces)
                       (setq p1 (point) ) )
              (setq p1 (point) )
              )
            (if (re-search-forward myWhiteSpaces nil "move")
                (progn (re-search-backward myWhiteSpaces)
                       (setq p2 (point) ))
              (setq p2 (point) ) ) )

          (if currentStateIsCompact
              (fill-region p1 p2)
            (let ((fill-column bigFillColumnVal))
              (fill-region p1 p2))
            )
          ) )

      (put this-command 'stateIsCompact-p (if currentStateIsCompact nil t)) ) ) )

(defun compact-uncompact-block-chinese ()
  "Remove or add line ending chars on current text block.
 (text block is delimited by line endings; similar to a paragraph)
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
              (if (> (- (line-end-position) (line-beginning-position)) fill-column) t nil) ) )

      (if (region-active-p)
          (if currentStateIsCompact
              (fill-region (region-beginning) (region-end))
            (save-restriction
                (narrow-to-region (region-beginning) (region-end))
                (goto-char (point-min))
                (while (search-forward "\n" nil t) (replace-match "" nil t)) ) )
        (if currentStateIsCompact
            (fill-paragraph nil)
          (let (p1 p2) ; p1 and p2 are beginning/end of text block
            (progn
              (if (re-search-backward "\n[ \t]*\n" nil "move")
                  (progn (re-search-forward "\n[ \t]*\n")
                         (setq p1 (point) ) )
                (setq p1 (point) )
                )
              (if (re-search-forward "\n[ \t]*\n" nil "move")
                  (progn (re-search-backward "\n[ \t]*\n")
                         (setq p2 (point) ))
                (setq p2 (point) ) ) )
            (save-restriction
              (narrow-to-region p1 p2)
              (goto-char (point-min))
              (while (search-forward "\n" nil t) (replace-match "" nil t)) )) ) )

      (put this-command 'stateIsCompact-p (if currentStateIsCompact nil t)) ) ) )

(defcustom xah-shell-abbrev-alist nil "alist of xah's shell abbrevs")
(setq xah-shell-abbrev-alist
          '(
            ("rsync1" . "rsync -z -r -v -t --chmod=Dugo+x --chmod=ugo+r --delete --exclude='*~' --exclude='.bash_history' --exclude='logs/'  --rsh='ssh -l u40651120' ~/web/ u40651120@s168753655.onlinehome.us:~/")

            ("ssh" . "ssh -l u40651120 xahlee.org ")
            ("img1" . "convert -quality 85% ")
            ("imgScale" . "convert -scale 50% -quality 85% ")
            ("img256" . "convert +dither -colors 256 ")
            ("imgBatch" . "find . -name \"*png\" | xargs -l -i basename \"{}\" \".png\" | xargs -l -i  convert -quality 85% \"{}.png\" \"{}.jpg\"")
            ("img-bmp2png" . "find . -name \"*bmp\" | xargs -l -i basename \"{}\" \".bmp\" | xargs -l -i  convert \"{}.bmp\" \"{}.png\"")

            ("grep" . "grep -r -F 'xxx' --include='*html' ~/web")

            ("rm_empty" . "find . -type f -empty")
            ("chmod_file" . "find . -type f -exec chmod 644 {} ';'")
            ("rm~" . "find . -name \"*~\" -exec rm {} ';'")
            ("findEmptyDir" . "find . -depth -empty -type d")
            ("rmEmptyDir" . "find . -depth -empty -type d -exec rmdir {} ';'")
            ("chmod2" . "find . -type d -exec chmod 755 {} ';'")
            ("lynx" . "lynx -dump -assume_local_charset=utf-8 -display_charset=utf-8 -width=100")
            ("vp" . "feh --randomize --recursive --auto-zoom --action \"gvfs-trash '%f'\" --geometry 1600x1000 ~/Pictures/cinse_pixra3/ &")
            )

          )

(defun xah-shell-commands (cmdAbbrev)
  "insert shell command from a selection prompt."
  (interactive
   (list
      (ido-completing-read "shell abbrevs:" (mapcar (lambda (x) (car x)) xah-shell-abbrev-alist) "PREDICATE" "REQUIRE-MATCH") ) )
  (progn
    (insert (cdr (assoc cmdAbbrev xah-shell-abbrev-alist)))
    ))

(defun xah-to-xah-elisp-mode  ()
  "redo my tutorial's code elisp markup"
  (interactive)
  (make-backup)
  (goto-char 1)
  (while
      (search-forward "<pre class=\"elisp\">" nil "NOERROR")
    (replace-match "<pre class=\"emacs-lisp\">" "FIXEDCASE" "LITERAL" )

    ;; (let* (
    ;;       ( ξxx (xhm-get-precode-langCode))
    ;;       (p1 (elt ξxx 1))
    ;;       (p2 (elt ξxx 2))
    ;;       )
    ;;    )
    (call-interactively 'xhm-htmlize-or-de-precode)
    (call-interactively 'xhm-htmlize-or-de-precode) ) )
