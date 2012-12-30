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

(defun xah-new-empty-buffer ()
  "Opens a new empty file in a particular dir.
Specific to Xah Lee."
  (interactive)
  (let ()
    (find-file "~/Documents/ciska/")
    (switch-to-buffer (generate-new-buffer "untitled"))
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)
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
  "Insert a blank a-lojban-a-day html template in a paritcular file."
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

(defun make-wiki-entry ()
  "Open pd.html, and at the right place, paste (a Wikipedia link), and save."
  (interactive)
(find-file "~/web/Periodic_dosage_dir/pd.html")
(goto-char (point-min))
(re-search-forward "wikime\n" nil t)
(yank)
(insert "\n")
(save-buffer))



(defun xah-open-file-at-cursor ()
  "Open the file path under cursor.

If there's no current text selection, this command uses path as input. If there is a text selection, it uses the text selection for path. (convenient if the file path contains space)

When called with `universal-argument', it'll try to use the `kill-ring''s last item as path. (setting useKillRing-p to t.)

If the path is url (starting with “http://”), and if it's xah site, visite the file, else launch browser vistiting that url.

input path can be {relative, full path, URL}. See: `xahsite-web-path-to-filepath' for types of paths supported."
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
            (find-file (xahsite-url-to-filepath ξs "addFileName" "ξredirect"))
          (browse-url ξs)
          )
      (progn ; not starting “http://”
        (let ((ξfff (xahsite-web-path-to-filepath ξs default-directory)) )
          (if (file-exists-p ξfff)
              (progn (find-file ξfff))
            (if (file-exists-p (concat ξfff ".el"))
                  (progn (find-file (concat ξfff ".el")))
                (progn (message "file doesn't exist: %s" ξfff))
                )
            )
          )
        ) ) ))

(defun xah-open-file-from-clipboard ()
  "Open the file path from OS's clipboard.
The clipboard should contain a file path or url to xah site. Open that file in emacs."
  (interactive)
  (let ( 
        (ξs 
         (with-temp-buffer 
           (yank)
           (buffer-string) ) ) )
    (if (string-match-p "\\`http://" ξs)
        (find-file (xahsite-url-to-filepath ξs "addFileName" "ξredirect"))
      (progn ; not starting “http://”
        (find-file (xahsite-web-path-to-filepath (remove-uri-fragment ξs) default-directory)) ) ) ))

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

(defun xah-find-word-usage (myWord)
  "Grep a dir for a word's usage."
  (interactive "sWord to search: ")
  (require 'grep)
  (grep-compute-defaults)
  (rgrep myWord "*html" "~/web/p")
;; ~/web/p
;; ~/web/flatland/
;; ~/web/Periodic_dosage_dir/_p2/russell-lecture.html
;; ~/web/Periodic_dosage_dir/_p2/why_not_christian.html
)



(defun copy-to-register-1 ()
  "Copy current line or text selection to register 1.
See also: `paste-from-register-1', `copy-to-register'."
  (interactive)
  (let* (
         (bds (get-selection-or-unit 'line ))
         (inputStr (elt bds 0) )
         (p1 (elt bds 1) )
         (p2 (elt bds 2) )
         )
    (copy-to-register ?1 p1 p2)
    (message "copied to register 1: 「%s」." inputStr)
))

(defun paste-from-register-1 ()
  "paste text from register 1.
See also: `copy-to-register-1', `insert-register'."
  (interactive)
  (insert-register ?1 t))



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
              (get-html-file-title myFile)
            (file-name-nondirectory myFile)))
    (setq myTitle (replace-pairs-in-string myTitle [["&amp;" "&"] ["&lt;" "<"] ["&gt;" ">" ]]) )

    (delete-region p1 p2)
    (insert "〈" myTitle "〉 @ " (xahsite-filepath-to-url myFile))
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

For example, if current buffer is of this file:
c:/Users/xah/web/xahlee_org/Periodic_dosage_dir/pd.html
then after calling this function,
default browser will be launched and opening this URL:
http://xahlee.org/Periodic_dosage_dir/pd.html"
  (interactive)
  (browse-url (replace-regexp-in-string "c:/Users/h3/web/xahlee_org/" "http://xahlee.org/" (buffer-file-name))) )



(defun x-query-find-then-bold ()
  "personal to xahlee.org's vocabulary pages.

Search forward a word enclosed by “<p class=\"wd\">” and “</p>”,
then search forward it inside the example body, only if it is not
already bold. Then, ask user whether that should be bold."
  (interactive)
  (let ()
    (goto-char (point-min))
    (while (search-forward-regexp "<p class=\"wd\">\\([^\<]+\\)</p>" nil t)
      (search-forward-regexp (match-string 1))
      (when (y-or-n-p "Do you want to bold the word?")
        (wrap-html-tag "span" "w")
        ;;(replace-match "<span class=\"x-w\">\\1</span>" t)
        ))
    ))

(defun 2zip ()
  "Zip the current file in `dired'.
If multiple files are marked, only zip the first one.
Require unix zip commandline tool."
  (interactive)
  (require 'dired)
  (let ( (fileName (elt (dired-get-marked-files) 0))  )
    (shell-command (format "zip -r '%s.zip' '%s'" (file-relative-name fileName) (file-relative-name fileName)))
    ))

(defun process-image (fileList argsString newNameSuffix newNameFileSuffix )
  "Create a new image.
fileList is a list of image file paths.
argsString is argument string passed to ImageMagick's “convert” command.
newNameSuffix is the string appended to file. e.g. “_new” gets you “cat_new.jpg”
newNameFileSuffix is the new file's file extension. e.g. “.png”
Requires ImageMagick shell tool."
  (require 'dired)
  (mapc
   (lambda (ξf)
     (let ( newName cmdStr )
       (setq newName (concat (file-name-sans-extension ξf) newNameSuffix newNameFileSuffix) )
       (while (file-exists-p newName)
         (setq newName (concat (file-name-sans-extension newName) newNameSuffix (file-name-extension newName t))) )

       ;; relative paths used to get around Windows/Cygwin path remapping problem
       (setq cmdStr
             (format "convert %s '%s' '%s'" argsString (file-relative-name ξf) (file-relative-name newName)) )
       (shell-command cmdStr)
       ))
   fileList ))

(defun scale-image (fileList scalePercentage)
  "Create a scaled jpg version of images of marked files in dired.
The new names have “-s” appended before the file name extension.

Requires ImageMagick shell tool."
  (interactive
   (let (
         (myFileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           ((string-equal major-mode "image-mode") (list (buffer-file-name)))
           (t (list (read-from-minibuffer "file name:") )) ) ) )
     (list myFileList (read-from-minibuffer "scale percentage:")) )
   )
  (process-image fileList (concat "-scale " scalePercentage "% -quality 85% " ) "-s" ".jpg" )
  )

(defun image-autocrop (fileList)
  "Create a new auto-cropped jpg version of images of marked files in dired.
Requires ImageMagick shell tool."
  (interactive
   (let (
         (myFileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           ((string-equal major-mode "image-mode") (list (buffer-file-name)))
           (t (list (read-from-minibuffer "file name:") )) ) ) )
     (list myFileList) )
   )
  (process-image fileList "-trim" "-c" ".jpg" )
  )

(defun 2png (fileList)
  "Create a png version of images of marked files in dired.
Requires ImageMagick shell tool."
  (interactive
   (let (
         (myFileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           ((string-equal major-mode "image-mode") (list (buffer-file-name)))
           (t (list (read-from-minibuffer "file name:") )) ) ) )
     (list myFileList) )
   )
  (process-image fileList "" "-2" ".png" )
  )

(defun 2drawing (fileList grayscale-p bitsPerPixel)
  "Create a png version of (drawing type) images of marked files in dired.
Requires ImageMagick shell tool."
  (interactive
   (let (
         (myFileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           ((string-equal major-mode "image-mode") (list (buffer-file-name)))
           (t (list (read-from-minibuffer "file name:") )) ) ) )
     (list myFileList
           (setq grayscale-p (yes-or-no-p "Grayscale?"))
           (read-string "Bits per pixel (1 2 4 8):" "4")) ) )
  (process-image fileList
                 (format "+dither %s -depth %s"
                         (if grayscale-p "-type grayscale" "")
                         ;; image magick “-colors” must be at least 8
                         ;; (if (< (string-to-number bitsPerPixel) 3)
                         ;;     8
                         ;;     (expt 2 (string-to-number bitsPerPixel)))
                         bitsPerPixel)  "-2" ".png" )
  )

(defun 2jpg (fileList)
  "Create a jpg version of images of marked files in dired.
Requires ImageMagick shell tool."
  (interactive
   (let (
         (myFileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           ((string-equal major-mode "image-mode") (list (buffer-file-name)))
           (t (list (read-from-minibuffer "file name:") )) ) ) )
     (list myFileList) )
   )
  (process-image fileList "" "-2" ".jpg" ))



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

(defun copy-rectangle-to-clipboard (p1 p2)
  "Copy region as column (rectangle) to operating system's clipboard.
This command will also put the text in register 0.

See also: `kill-rectangle', `copy-to-register'."
  (interactive "r")
  (let ((x-select-enable-clipboard t))
    (copy-rectangle-to-register ?0 p1 p2)
    (kill-new
     (with-temp-buffer
       (insert-register ?0)
       (buffer-string) )) ) )


(defun rename-html-inline-image (ξnewFilePath)
  "Replace current HTML inline image's file name.

When cursor is in HTML link file path, e.g.  <img src=\"gki/macosxlogo.png\" > and this command is called, it'll prompt user for a new name. The link path will be changed to the new name, the corresponding file will also be renamed. The operation is aborted if a name exists."
  
  (interactive
   (let (
         (defaultInput (expand-file-name 
                        (elt (get-selection-or-unit 'filepath) 0)
                        (file-name-directory (or (buffer-file-name) default-directory )) )) )
     (list (read-string "New name: " defaultInput nil defaultInput )) ) )
  (let* ( 
         (bds (get-selection-or-unit 'filepath))
         (ξinputPath (elt bds 0) )
         (p1 (aref bds 1) )
         (p2 (aref bds 2) )
         (ξffp (local-url-to-file-path (expand-file-name ξinputPath (file-name-directory (or (buffer-file-name) default-directory )) ))) ;full path
         ;; (setq ξffp (windows-style-path-to-unix (local-url-to-file-path ξffp)))
         )

    (if (file-exists-p ξnewFilePath)
        (progn (error "file 「%s」 exist." ξnewFilePath ))
      (progn
        (rename-file ξffp ξnewFilePath )
        (message "rename to %s" ξnewFilePath)
        (delete-region p1 p2)
        (insert (xahsite-filepath-to-href-value ξnewFilePath (or (buffer-file-name) default-directory)))
        )
      )
    ))


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
