;; -*- coding: utf-8; lexical-binding: t; -*-
;; stuff related to HTML
;; most things moved to xah-html-mode
;; ∑ http://xahlee.org/

(defun forward-html-end-tag ()
  "Move cursor to the next HTML tag's content."
  (interactive)
  (forward-char 1)
  (search-forward "</")
  (backward-char 2)
  )

(defun backward-html-end-tag ()
  "Move cursor to the previous HTML tag's content."
  (interactive)
  (search-backward "</")
  ;; (forward-char-char 2)
  )

(defun xah-insert-reference-span-tag ()
  "Add <span class=\"ref\">…</span> tag to current HTML element or text selection.
Version 2016-11-10"
  (interactive)
  (require 'xah-html-mode)
  (let ( $p1 $p2 )
    (if (use-region-p)
         (setq $p1 (region-beginning) $p2 (region-end))
      (progn
        (xah-html-skip-tag-backward)
        (setq $p1 (point))
        (xah-html-skip-tag-forward)
        (setq $p2 (point))))
    (set-mark $p1)
    (goto-char $p2)
    (xah-html-insert-open-close-tags "span" "ref" $p1 $p2)
    ;; (xah-html-wrap-html-tag "span" "ref")
    ))

(defun xah-update-article-timestamp ()
  "Update article's timestamp.
Add or update today's date to a line lie this
<div class=\"byline\">By Xah Lee. Date: <time>2013-04-28</time>. Last updated: <time>2021-01-11</time>.</div>
Also, move cursor there.
Also, pushes mark. You can go back to previous location `exchange-point-and-mark'.
Also, removes repeated empty lines in buffer.
Version 2018-12-07 2020-09-22"
  (interactive)
  (save-excursion
    ;; remove empty lines
    (progn
      (goto-char (point-min))
      (while (re-search-forward "\n\n\n+" nil t)
        (replace-match (make-string 2 ?\n)))))
  (let ($p1 $p2 $num $bufferTextOrig )
    (push-mark)
    (goto-char (point-min))
    (when (search-forward "<div class=\"byline\">" nil)
      (progn ;; set $p1 $p2. they are boundaries of inner text
        (setq $p1 (point))
        (backward-char 1)
        (search-forward "</div>" )
        (backward-char 6)
        (setq $p2 (point))
        (let (($bylineText (buffer-substring-no-properties $p1 $p2)))
          (when (> (length $bylineText) 110)
            (user-error "something's probably wrong. the length for the byline is long: 「%s」" $bylineText ))))
      (save-restriction ; update article timestamp
        (narrow-to-region $p1 $p2)
        (setq $bufferTextOrig (buffer-string ))
        (setq $num (count-matches "<time>" (point-min) (point-max)))
        (if (equal $num 1)
            (progn
              (goto-char (point-min))
              (search-forward "</time>")
              (insert ". Last updated: ")
              (insert (format "<time>%s</time>" (format-time-string "%Y-%m-%d")))
              (when (not (looking-at "\\.")) (insert ".")))
          (progn ;; if there are more than 1 “time” tag, delete the last one
            (let ($p3 $p4)
              (goto-char (point-max))
              (search-backward "</time>")
              (search-forward "</time>")
              (setq $p4 (point))
              (search-backward "<time>")
              (setq $p3 (point))
              (delete-region $p3 $p4 ))
            (insert (format "<time>%s</time>" (format-time-string "%Y-%m-%d")))
            (when (not (looking-at "\\.")) (insert ".")))))
      (require 'xah-fly-keys)
      (xah-make-backup-and-save)
      (message "old date line: 「%s」" $bufferTextOrig)
      (search-forward "</div>" )
      ;;
      )))

(defun xahsite-update-page-tag ()
  "Update HTML page navigation tags.

The input is a text block or text selection.
Each line should a file name/path (can be relative path)
Update each file's page navigation tag.

Each file name is a file path without dir, and relative to current dir.
Sample text selection for input:

words.html
words-2.html
words-3.html
words-4.html
Version before 2012 or so"
  (interactive)
  (require 'sgml-mode)
  (let* (
         $p1 $p2
         ($fileList (split-string (buffer-substring-no-properties $p1 $p2) "\n" t))
         $pageNavStr )
    (delete-region $p1 $p2)
    ;; generate the page nav string
    (setq $pageNavStr
          (format "<nav class=\"page\">\n%s</nav>"
                  (let ($result $linkPath $fTitle ($i 0))
                    (while (< $i (length $fileList))
                      (setq $linkPath (elt $fileList $i))
                      (setq $fTitle (xah-html-get-html-file-title $linkPath))
                      (setq $result (concat $result "<a href=\"" $linkPath "\" title=\"" $fTitle "\">" (number-to-string (1+ $i)) "</a>\n"))
                      (setq $i (1+ $i)))
                    $result
                    )))
    ;; open each file, insert the page nav string
    (mapc
     (lambda (thisFile)
       (message "%s" thisFile)
       (find-file thisFile)
       (goto-char 1)
       (if (search-forward "<nav class=\"page\">" nil t)
           (let ($p3 $p4 )
             (search-backward "<")
             (setq $p3 (point))
             (sgml-skip-tag-forward 1)
             (setq $p4 (point))
             (delete-region $p3 $p4)
             (insert $pageNavStr))
         (progn
           (search-forward "<script><!--
google_ad_client")
           (progn
             (search-backward "<script>")
             (insert $pageNavStr "\n\n")))))
     $fileList)))

(defun xah-syntax-color-hex ()
  "Syntax color text of the form 「#ff1100」 and 「#abc」 in current buffer.
URL `http://ergoemacs.org/emacs/emacs_CSS_colors.html'
Version 2017-03-12"
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[[:xdigit:]]\\{3\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background
                      (let* (
                             (ms (match-string-no-properties 0))
                             (r (substring ms 1 2))
                             (g (substring ms 2 3))
                             (b (substring ms 3 4)))
                        (concat "#" r r g g b b))))))
     ("#[[:xdigit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (match-string-no-properties 0)))))))
  (font-lock-flush))

(defun xah-syntax-color-hsl ()
  "Syntax color CSS's HSL color spec eg 「hsl(0,90%,41%)」 in current buffer.
URL `http://ergoemacs.org/emacs/emacs_CSS_colors.html'
Version 2017-02-02"
  (interactive)
  (require 'color)
  (font-lock-add-keywords
   nil
   '(("hsl( *\\([0-9]\\{1,3\\}\\) *, *\\([0-9]\\{1,3\\}\\)% *, *\\([0-9]\\{1,3\\}\\)% *)"
      (0 (put-text-property
          (+ (match-beginning 0) 3)
          (match-end 0)
          'face
          (list
           :background
           (concat
            "#"
            (mapconcat
             'identity
             (mapcar
              (lambda (x) (format "%02x" (round (* x 255))))
              (color-hsl-to-rgb
               (/ (string-to-number (match-string-no-properties 1)) 360.0)
               (/ (string-to-number (match-string-no-properties 2)) 100.0)
               (/ (string-to-number (match-string-no-properties 3)) 100.0)))
             "" )) ;  "#00aa00"
           ))))))
  (font-lock-flush))

;; (concat "#" (mapconcat 'identity
;;                         (mapcar
;;                          (lambda (x) (format "%x" (round (* x 255))))
;;                          (color-hsl-to-rgb
;;                           (/ (string-to-number "0") 360.0)
;;                           (/ (string-to-number "90") 100.0)
;;                           (/ (string-to-number "50") 100.0)
;;                           ) )
;;                         "" ))

;; (format "%2x" (round (* (/ (string-to-number "49") 100.0) 255)))
;; (format "%02x" 10)

(defun xah-python-ref-linkify ()
  "Transform current line (a file path) into a link.
For example, this line:

~/web/xahlee_info/python_doc_2.7.6/library/stdtypes.html#mapping-types-dict

becomes

<span class=\"ref\"><a href=\"../python_doc_2.7.6/library/stdtypes.html#mapping-types-dict\">5. Built-in Types — Python v2.7.6 documentation #mapping-types-dict</a></span>

The path is relative to current file. The link text is the linked file's title, plus fragment url part, if any.

Requires a python script. See code."
  (interactive)
  (let (scriptName bds)
    (setq bds (bounds-of-thing-at-point 'filename))
    (save-excursion
      (setq scriptName (format "/usr/bin/python ~/git/xahscripts/emacs_pydoc_ref_linkify.py %s" (buffer-file-name)))
      (shell-command-on-region (car bds) (cdr bds) scriptName nil "REPLACE" nil t))))

(defvar xah-move-image-file-from-dirs nil "A list of dir paths used by `xah-move-image-file' to search for temp image files.")
(setq xah-move-image-file-from-dirs
      '(
        "~/Downloads/"
        "~/Pictures/"
        "~/Desktop/"
        ;; "~/Documents/"
        "~/"
        "/tmp"
        ))

(defvar xah-move-image-file-regex-list nil "A list of regex strings used by `xah-move-image-file' to search for temp image files.")
(setq xah-move-image-file-regex-list
      '(
        "^tt"
        "^IMG_"
        "^Screen Shot"
        "^Screenshot"
        "^screenshot"
        "[+0-9A-Za-z]\\{11\\}\._[A-Z]\\{2\\}[0-9]\\{4\\}_\.jpg" ; amazon pic
        "[+0-9A-Za-z]\\{11\\}\._[A-Z]\\{2\\}_[A-Z]\\{2\\}[0-9]\\{4\\}_\.jpg" ; amazon pic
        ;; 711UVfyz+FL._AC_SL1500_.jpg
        ))

(defun xah-move-image-file ( @toDirName  )
  "Move image file to another dir.

Search a list of dirs by a list of regex. If a match is found, move it into a new file path, by prompting user to type it.

The from dirs is defined in `xah-move-image-file-from-dirs'
The regex list is defined in `xah-move-image-file-regex-list'

A random string attached (as id) is added to file name, and any uppercase file extension name is lowercased, e.g. .JPG becomes .jpg. Space in filename is replaced by the low line char “_”.

Automatically call `xah-dired-remove-all-metadata' and `xah-dired-optimize-png' afterwards.

URL `http://ergoemacs.org/emacs/move_image_file.html'
Version 2019 2021-07-05"
  (interactive (list (ido-read-directory-name "Move img to dir:" )))
  (let ($p0 $dirs $fromPath $newName1 $ext $toPath $randStr)
    (setq $p0 (point))
    (setq $dirs (if xah-move-image-file-from-dirs
                    xah-move-image-file-from-dirs
                  '( "~/Downloads/" "~/Desktop/" "/tmp" )))
    (setq $randStr
          (let ( $charset $len $randlist )
            (setq $charset "bcdfghjkmnpqrstvwxyz23456789BCDFGHJKMNPQRSTVWXYZ")
            (setq $len (length $charset))
            (setq $randlist nil)
            (dotimes (_ 5)
              (push (char-to-string (elt $charset (random $len)))  $randlist))
            (mapconcat 'identity $randlist "")))
    (setq $fromPath
          (catch 'found57804
            (dolist (xdir $dirs )
              (when (file-exists-p xdir)
                (let ($dirFiles)
                  (setq $dirFiles
                        (directory-files
                         xdir t
                         (mapconcat 'identity
                                    (if xah-move-image-file-regex-list
                                        xah-move-image-file-regex-list
                                      '(
                                        "^IMG_"
                                        "^Screen Shot"
                                        "^Screenshot"
                                        "^screenshot"
                                        ))
                                    "\\|")))
                  (when $dirFiles (throw 'found57804 (car $dirFiles))))))))
    (when (not $fromPath)
      (error "No file name matched regexes %s at dirs %s" xah-move-image-file-regex-list $dirs))
    (setq $ext
          (let ($x)
            (setq $x (file-name-extension $fromPath ))
            (if $x (downcase $x) "" )))
    (setq $newName1 (file-name-nondirectory (file-name-sans-extension $fromPath)))
    (setq $newName1 (replace-regexp-in-string "^tt[0-9]*" "" $newName1 ))
    (setq $newName1
          (replace-regexp-in-string
           "Screen Shot \\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\) at [0-9]+.[0-9]\\{2\\}.[0-9]\\{2\\} \\(AM\\|PM\\)"
           "screenshot_\\1"
           $newName1 ))
    ;; Screen Shot 2018-07-25 at 2.46.36 AM.png
    (setq $newName1 (read-string "new name:" $newName1 nil $newName1 ))
    (setq $newName1
          (concat
           (replace-regexp-in-string "^tt\\|,\\| \\|\\+\\|(\\|)" "_" $newName1)
           "_"
           $randStr
           "."
           ))
    (setq $toPath (concat (file-name-as-directory @toDirName ) $newName1 $ext))
    (when (or (string-equal $ext "jpg-large")
              (string-equal $ext "jpg_large")
              (string-equal $ext "jpg_medium")
              (string-equal $ext "jpeg")
              (string-equal $ext "jfif"))
      (setq $toPath (concat (file-name-sans-extension $toPath) ".jpg")))
    (message "xah-move-image-file: from path is 「%s」\n to path is 「%s」 " $fromPath $toPath)
    (if (file-exists-p $toPath)
        (error "move to path exist: %s" $toPath)
      (progn
        (rename-file $fromPath $toPath)
        (when (string-equal major-mode "dired-mode")
          (revert-buffer))
        (if (string-equal major-mode "xah-html-mode")
            (progn
              (kill-new $toPath)
              (goto-char $p0)
              (insert "\n\n")
              (insert $toPath)
              (insert "\n\n")
              (backward-word )
              (xah-html-any-linkify))
          (progn
            (goto-char $p0)
            (insert "\n\n")
            (insert $toPath)
            (insert "\n\n")))
        (when (fboundp 'xah-dired-remove-all-metadata)
          (xah-dired-remove-all-metadata (list $toPath)))
        (when (and (string-equal $ext "png") (fboundp 'xah-dired-optimize-png))
          (xah-dired-optimize-png (list $toPath)))))))

(defun xah-youtube-get-image ()
  "given a youtube url, get its image.
the url is taken from current line
tttttttttttttttttttttttttttttttt
todo
2016-07-01"
  (interactive)
  (let* (
         (p1 (line-beginning-position))
         (p2 (line-end-position))
         (lineStr (buffer-substring-no-properties p1 p2))
         id
         shellCmd
         (fileName (read-file-name "name:")))
    (setq id (replace-regexp-in-string "https://www.youtube.com/watch\\?v=" "" lineStr "FIXEDCASE" "LITERAL" ))
    (setq id (replace-regexp-in-string "http://www.youtube.com/watch\\?v=" "" id "FIXEDCASE" "LITERAL" ))

    (setq shellCmd
          (concat "wget " "https://i.ytimg.com/vi/" id "/maxresdefault.jpg"
                  " -O "
                  fileName
                  ".jpg "
                  ))

    (shell-command shellCmd)

    ;; (replace-regexp-in-string "https://www.youtube.com/watch\\?v=" "" "https://www.youtube.com/watch?v=aa8jTf7Xg3E" "FIXEDCASE" "LITERAL" )

    ;; https://www.youtube.com/watch?v=aa8jTf7Xg3E
    ;; https://i.ytimg.com/vi/aa8jTf7Xg3E/maxresdefault.jpg

    ;; https://www.youtube.com/watch?v=zdD_QygwRuY

    ))

;; HHH___________________________________________________________________

(defvar xah-copy-html-by-link-template nil "template for `xah-copy-html-by-link'.")

(setq xah-copy-html-by-link-template "<main>

<div class=\"ad_top_39054\"></div>

<h1>xxxxxtitle</h1>

<div class=\"byline\">By Xah Lee. Date: <time>xxxxxdate</time>.</div>

<div class=\"ads_bottom_dtpcz\"></div>

</main>")

(defun xah-copy-html-by-link ()
  "Make a copy of a html file from 2 previous html file link in current buffer.
Explanation:
current buffer is a html file,
and contains 2 href links, example:
<a href=\"cat.html\">my cat</a>
<a href=\"dog.html\">my dog▮</a>
cursor is somewhere in the link description of the second href value.
Call this command.
This command will basically use the first link as template and create the second file. Quit if second file already exist.
Version 2018-12-24 2021-06-25"
  (interactive)
  (require 'xah-html-mode)
  (save-excursion
    (let ( $p0 $p1 $p2 $newTitle  $path1 $path2 $buf )
      (skip-chars-backward "^>")
      (setq $p1 (point))
      (skip-chars-forward "^<")
      (setq $p2 (point))
      (setq $newTitle (buffer-substring-no-properties $p1 $p2 ))
      (search-backward "href=\"" )
      (setq $p0 (point))
      (forward-char 6)
      (setq $path2 (expand-file-name (thing-at-point 'filename)))
      (goto-char $p0)
      (search-backward "href=\"" )
      (forward-char 6)
      (setq $path1 (expand-file-name (thing-at-point 'filename)))
      (if (file-exists-p $path2)
          (user-error "%s" (format "file 2 「%s」 exist." $path2))
        (progn
          (setq $buf (find-file $path2))
          (erase-buffer)
          (insert-file-contents $path1 )
          (let (p1 p2)
            (goto-char (point-min))
            (search-forward "<main>" )
            (setq p1 (- (point) (length "<main>")))
            (search-forward "</main>" )
            (setq p2 (point))
            (delete-region p1 p2)
            (insert xah-copy-html-by-link-template))
          (progn
            (goto-char (point-min))
            (re-search-forward "<title>\\([^<]+?\\)</title>")
            (replace-match (format "<title>%s</title>" $newTitle ))
            (re-search-forward "<h1>\\([^<]+?\\)</h1>")
            (replace-match (format "<h1>%s</h1>" $newTitle ))
            (goto-char (point-min))
            (search-forward "xxxxxdate" )
            (delete-char (- (length "xxxxxdate")))
            (insert (format-time-string "%Y-%m-%d")))
          (save-buffer $buf)
          ;; (kill-buffer $buf)
          )))))

(defvar xahsite-new-page-paths nil
  "A list of file paths that's used as template for creating a new xah site page. Used by `xahsite-new-page'.")
(setq xahsite-new-page-paths
      '(
        "c:/Users/xah/web/ergoemacs_org/emacs/ErgoEmacs_logo.html"
        "c:/Users/xah/web/ergoemacs_org/misc/Daniel_Weinreb_died.html"
        "c:/Users/xah/web/wordyenglish_com/chinese/Zhuangzi.html"
        "c:/Users/xah/web/wordyenglish_com/lit/capitalists_vs_communists_chess_set.html"
        "c:/Users/xah/web/xaharts_org/arts/Hunger_Games_eyelash.html"
        "c:/Users/xah/web/xaharts_org/dinju/Petronas_towers.html"
        "c:/Users/xah/web/xaharts_org/movie/brazil_movie.html"
        "c:/Users/xah/web/xahlee_info/comp/artificial_neural_network.html"
        "c:/Users/xah/web/xahlee_info/golang/golang_run.html"
        "c:/Users/xah/web/xahlee_info/js/js.html"
        "c:/Users/xah/web/xahlee_info/kbd/3m_ergonomic_mouse.html"
        "c:/Users/xah/web/xahlee_info/M/wolfram_language_free.html"
        "c:/Users/xah/web/xahlee_info/math/math_books.html"
        "c:/Users/xah/web/xahlee_info/mswin/xah_windows_setup.html"
        "c:/Users/xah/web/xahlee_info/talk_show/xah_talk_show_2019-03-05_unicode.html"
        "c:/Users/xah/web/xahlee_info/w/spam_farm_2018.html"
        "c:/Users/xah/web/xahlee_org/Periodic_dosage_dir/sarah_jeong.html"
        "c:/Users/xah/web/xahlee_org/sex/Camille_Paglia_free_women_free_men.html"
        "c:/Users/xah/web/xahmusic_org/music/Disney_Frozen__let_it_go.html"
        "c:/Users/xah/web/xahporn_org/porn/birth_of_a_Nokia.html"
        ;;
        ))

(defun xahsite-new-page ()
  "Make a new blog page.
The base template depends on the variable `xahsite-new-page-paths'.
Version 2020-07-16 2021-02-02"
  (interactive)
  (let* (
         (curFile (buffer-file-name))
         (curDirPath (file-name-directory curFile))
         (templateFname (cdr (assoc curDirPath (mapcar
                                                (lambda (x)
                                                  (cons (file-name-directory x) (file-name-nondirectory x)))
                                                xahsite-new-page-paths))))
         (templateFpath (concat curDirPath templateFname))
         ($p1 (line-beginning-position))
         ($p2 (line-end-position))
         (xTitle (downcase (buffer-substring-no-properties $p1 $p2)))
         (fnameBase (replace-regexp-in-string " +\\|/" "_" xTitle ))
         (xFPath (format "%s%s.html" (file-name-directory templateFpath) fnameBase))
         p3
         )
    (if (file-exists-p xFPath)
        (message "file exist: %s" xFPath)
      (progn
        (find-file xFPath)
        (insert-file-contents templateFpath )
        (progn
          (goto-char (point-min))
          (search-forward "<title>" )
          (insert xTitle)
          (setq p3 (point))
          (skip-chars-forward "^<")
          (delete-region p3 (point))
          (search-forward "<h1>" )
          (insert xTitle)
          (setq p3 (point))
          (skip-chars-forward "^<")
          (delete-region p3 (point))
          (search-forward "</h1>" )
          (when (search-forward "<div class=\"byline\">By Xah Lee. Date: <time>" nil t)
            (insert (format-time-string "%Y-%m-%d"))
            (setq p3 (point))
            (search-forward "</div>" )
            (delete-region p3 (point))
            (insert "</time>.</div>"))
          (setq p3 (point))
          (when
              (search-forward "ads_bottom_dtpcz" nil t)
            (search-backward "<")
            (delete-region p3 (point))
            (insert "\n\n\n\n")
            (backward-char 2))
          (save-buffer )
          (kill-buffer )))
      (delete-region $p1 $p2)
      (insert xFPath)

      (xah-all-linkify)
      (search-backward "\"")
      ;; (beginning-of-line)
      ;; (insert "<p>")
      ;; (end-of-line )
      ;; (insert "</p>")

      )

    ;;
    ))

(defun xah-html-fix-youtube-description ()
  "Delete unwanted youtube description under cursor.

For example,

<figcaption>
#DESKWATCH 04 村上ゆいちさんの左手デバイス捌き！ - pixivision
74,915 views
•Aug 15, 2016
1K
17
Share
Save
pixivision
33.6K subscribers
</figcaption>

becomes

<figcaption>#DESKWATCH 04 村上ゆいちさんの左手デバイス捌き！ - pixivision<br />
Aug 15, 2016<br />
pixivision<br />
</figcaption>

Version 2020-09-05 2021-06-23"
  (interactive)
  (let ($p1 $p2)
    (save-excursion
      (search-backward "<figcaption>")
      (search-forward "<figcaption>" )
      (setq $p1 (point))
      (search-forward "</figcaption>")
      (search-backward "</figcaption>" )
      (setq $p2 (point)))
    (save-restriction
      (narrow-to-region $p1 $p2)
      (goto-char (point-min))
      (when (re-search-forward "[,0-9]+ views" nil t) (replace-match ""))
      ;; (search-forward "•" ) (replace-match "")
      (if (re-search-forward "[A-Z][a-z][a-z] [0-9][0-9]?, [0-9]\\{4,4\\}" nil t) ;; •Aug 15, 2016
          nil
        (re-search-forward "[0-9]+ hours ago" ))
      ;; thumb up/down count
      (when (re-search-forward "[.0-9]+[KM]?\n\n?[.0-9]+[KM]?\n" nil t) (replace-match ""))
      (let ((case-fold-search t))
        (when (re-search-forward "share\n\n?save" nil "NOERROR" ) (replace-match "")))
      (when (re-search-forward "[0-9]*\\.*[0-9]+[KM]? subscribers" nil t) (replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "\n\n+" nil "NOERROR") (replace-match "\n"))
      (goto-char (point-min)) (when (equal (char-after ) ?\n ))
      (skip-chars-forward "\n")
      (while (search-forward "\n" nil "NOERROR")
        (replace-match "<br />\n" )))))

(defun xah-html-wrap-big-tag ()
    "insert big tag"
  (interactive)
  (xah-html-wrap-html-tag "span" "big" ""))
