;; -*- coding: utf-8 -*-
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
  (let ( -p1 -p2 )
    (if (use-region-p)
        (progn (setq -p1 (region-beginning))
               (setq -p2 (region-end)))
      (progn
        (xah-html-skip-tag-backward)
        (setq -p1 (point))
        (xah-html-skip-tag-forward)
        (setq -p2 (point))))
    (set-mark -p1)
    (goto-char -p2)
    (xah-html-insert-open-close-tags "span" "ref" -p1 -p2)
    ;; (xah-html-wrap-html-tag "span" "ref")
    ))

(defun xahsite-update-article-timestamp ()
  "Update article's timestamp.
Add today's date to the “byline” tag of current file, also delete the last one if there are more than one.
Also, move cursor there.
Also, pushes mark. You can go back to previous location `exchange-point-and-mark'.
Also, removes repeated empty lines.
WARNING: This command saves buffer if it's a file.
Version 2016-11-23"
  (interactive)
  (save-excursion ; remove empty lines
    (progn
      (goto-char (point-min))
      (while (search-forward-regexp "\n\n\n+" nil "noerror")
        (replace-match (make-string 2 ?\n)))))
  (let (-p1 -p2 -num -bufferTextOrig -reportText)
    (push-mark)
    (goto-char 1)
    (when (search-forward "<div class=\"byline\">" nil)
      (progn ;; set -p1 -p2. they are boundaries of inner text
        (setq -p1 (point))
        (backward-char 1)
        (search-forward "</div>" )
        (backward-char 6)
        (setq -p2 (point))
        (let ((-bylineText (buffer-substring-no-properties -p1 -p2)))
          (when (> (length -bylineText) 110)
            (user-error "something's probably wrong. the length for the byline is long: 「%s」" -bylineText ))))
      (save-restriction ; update article timestamp
        (narrow-to-region -p1 -p2)
        (setq -bufferTextOrig (buffer-string ))
        (setq -num (count-matches "<time>" (point-min) (point-max)))
        (if (equal -num 1)
            (progn
              (goto-char (point-min))
              (search-forward "</time>")
              (insert ". Last updated: ")
              (insert (format "<time>%s</time>" (format-time-string "%Y-%m-%d")))
              (when (not (looking-at "\\.")) (insert ".")))
          (progn ;; if there are more than 1 “time” tag, delete the last one
            (let (-p3 -p4)
              (goto-char (point-max))
              (search-backward "</time>")
              (search-forward "</time>")
              (setq -p4 (point))
              (search-backward "<time>")
              (setq -p3 (point))
              (delete-region -p3 -p4 ))
            (insert (format "<time>%s</time>" (format-time-string "%Y-%m-%d")))
            (when (not (looking-at "\\.")) (insert "."))
            (goto-char (point-max)))))
      (let ; backup
          ((-fname (buffer-file-name)))
        (if -fname
            (let ((-backup-name
                   (concat -fname "~" (format-time-string "%Y%m%dT%H%M%S") "~")))
              (copy-file -fname -backup-name t)
              (message (concat "Backup saved at: " -backup-name)))))
      (save-buffer )
      (message "old date line: 「%s」" -bufferTextOrig)
      (when ;; open in browser
          (string-equal system-type "gnu/linux")
        (let ( (process-connection-type nil))
          (start-process "" nil "setsid" "firefox" (concat "file://" buffer-file-name )))
        ;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. eg with nautilus
        ))))

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
"
  (interactive)
  (require 'sgml-mode)
  (let* (
         -p1 -p2
         (-setBoundary
          (save-excursion
            (if (re-search-backward "\n[ \t]*\n" nil "move")
                (progn (re-search-forward "\n[ \t]*\n")
                       (setq -p1 (point)))
              (setq -p1 (point)))
            (if (re-search-forward "\n[ \t]*\n" nil "move")
                (progn (re-search-backward "\n[ \t]*\n")
                       (setq -p2 (point)))
              (setq -p2 (point)))))
         (-fileList (split-string (buffer-substring-no-properties -p1 -p2) "\n" t))
         -pageNavStr )
    (delete-region -p1 -p2)
    ;; generate the page nav string
    (setq -pageNavStr
          (format "<nav class=\"page\">\n%s</nav>"
                  (let (-result -linkPath -fTitle (-i 0))
                    (while (< -i (length -fileList))
                      (setq -linkPath (elt -fileList -i))
                      (setq -fTitle (xah-html-get-html-file-title -linkPath))
                      (setq -result (concat -result "<a href=\"" -linkPath "\" title=\"" -fTitle "\">" (number-to-string (1+ -i)) "</a>\n"))
                      (setq -i (1+ -i)))
                    -result
                    )))
    ;; open each file, insert the page nav string
    (mapc
     (lambda (thisFile)
       (message "%s" thisFile)
       (find-file thisFile)
       (goto-char 1)
       (if (search-forward "<nav class=\"page\">" nil t)
           (let (-p3 -p4 )
             (search-backward "<")
             (setq -p3 (point))
             (sgml-skip-tag-forward 1)
             (setq -p4 (point))
             (delete-region -p3 -p4)
             (insert -pageNavStr))
         (progn
           (search-forward "<script><!--
google_ad_client")
           (progn
             (search-backward "<script>")
             (insert -pageNavStr "\n\n")))))
     -fileList)))

(defun xah-syntax-color-hex ()
  "Syntax color text of the form 「#ff1100」 and 「#abc」 in current buffer.
URL `http://ergoemacs.org/emacs/emacs_CSS_colors.html'
Version 2016-07-04"
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[ABCDEFabcdef[:digit:]]\\{3\\}"
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
     ("#[ABCDEFabcdef[:digit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (match-string-no-properties 0)))))))
  (font-lock-fontify-buffer))

(defun xah-syntax-color-hsl ()
  "Syntax color CSS's HSL color spec eg 「hsl(0,90%,41%)」 in current buffer.
URL `http://ergoemacs.org/emacs/emacs_CSS_colors.html'
Version 2015-06-11"
  (interactive)
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
  (font-lock-fontify-buffer))

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

;; (defun xah-move-image-file (*dir-name *file-name)
;;   "move image file at
;; ~/Downloads/xx.jpg
;; to current dir of subdir i
;; and rename file to current line's text.

;; if xx.jpg doesn't exit, try xx.png. The dirs to try are
;;  ~/Downloads/
;;  ~/Pictures/
;;  /tmp

;; Version 2016-10-08"
;;   (interactive "DMove xx img to dir:
;; sNew file name:")
;;   (let (
;;         -from-path
;;         -to-path )
;;     (setq -from-path
;;           (cond
;;            ((file-exists-p (expand-file-name "~/Downloads/xx.jpg"))
;;             (expand-file-name "~/Downloads/xx.jpg"))
;;            ((file-exists-p (expand-file-name "~/Downloads/xx.JPG"))
;;             (expand-file-name "~/Downloads/xx.JPG"))
;;            ((file-exists-p (expand-file-name "~/Downloads/xx.png"))
;;             (expand-file-name "~/Downloads/xx.png"))
;;            ((file-exists-p (expand-file-name "~/Pictures/xx.jpg"))
;;             (expand-file-name "~/Pictures/xx.jpg"))
;;            ((file-exists-p (expand-file-name "~/Pictures/xx.png"))
;;             (expand-file-name "~/Pictures/xx.png"))
;;            ((file-exists-p (expand-file-name "~/Pictures/xx.gif"))
;;             (expand-file-name "~/Pictures/gif.png"))
;;            ((file-exists-p (expand-file-name "~/Downloads/xx.gif"))
;;             (expand-file-name "~/Downloads/xx.gif"))

;;            ((file-exists-p "/tmp/xx.jpg")
;;             "/tmp/xx.jpg")
;;            ((file-exists-p "/tmp/xx.png")
;;             "/tmp/xx.png")
;;            (t (error "no xx.jpg or xx.png at downloads dir nor pictures dir nor /tmp dir"))))
;;     (setq -to-path (concat
;;                     (file-name-as-directory *dir-name )
;;                     *file-name "."
;;                     (downcase (file-name-extension -from-path ))))
;;     (if (file-exists-p -to-path)
;;         (message "move to path exist: %s" -to-path)
;;       (progn
;;         (rename-file -from-path -to-path)
;;         (find-file -to-path )
;;         (message "move to path: %s" -to-path)))))

(defun xah-move-image-file (*dir-name *file-name)
  "move image file at
~/Downloads/
or
~/Pictures/

named any of
x.jpg
x1.jpg
x2.jpg
etc.
or with png extension.

to a different dir and rename, prompting user.

Version 2016-12-22"
  (interactive "DMove x img to dir:
sNew file name:")
  (let (
        -from-path
        -to-path
        (-dirs '( "~/Downloads/" "~/Pictures/" "/tmp" ))
        (-names '( "x" "x0" "x1" "x2" "x3" "x4" "x5" "x6" "x7" "x8" "x9" "x10" ))
        (-exts '("jpg" "png" "gif" "JPG" "PNG" "GIF" "mp4" )))
    (setq -from-path
          (let (-path)
            (catch 'x42566
              (dolist (-x-dir -dirs )
                (dolist (-x-name -names )
                  (dolist (-x-ext -exts )
                    (setq -path (expand-file-name (concat -x-dir -x-name "." -x-ext)))
                    (when (file-exists-p -path)
                      (progn
                        (throw 'x42566 -path))))))
              nil
              )))
    (when (null -from-path)
      (error "no xx.jpg or xx.png at downloads dir nor pictures dir nor /tmp dir"))
    (setq -to-path (concat
                    (file-name-as-directory *dir-name )
                    *file-name "."
                    (downcase (file-name-extension -from-path ))))
    (if (file-exists-p -to-path)
        (message "move to path exist: %s" -to-path)
      (progn
        (rename-file -from-path -to-path)
        (find-file -to-path )
        (revert-buffer)
        (message "move to path: %s" -to-path)))))

(defun xah-youtube-get-image ()
  "
given a youtube url, get its image.
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
    (setq id (replace-regexp-in-string "https://www.youtube.com/watch\\?v=" "" lineStr 'FIXEDCASE 'LITERAL ))
    (setq id (replace-regexp-in-string "http://www.youtube.com/watch\\?v=" "" id 'FIXEDCASE 'LITERAL ))

    (setq shellCmd
          (concat "wget " "https://i.ytimg.com/vi/" id "/maxresdefault.jpg"
                  " -O "
                  fileName
                  ".jpg "
                  ))

    (shell-command shellCmd)

    ;; (replace-regexp-in-string "https://www.youtube.com/watch\\?v=" "" "https://www.youtube.com/watch?v=aa8jTf7Xg3E" 'FIXEDCASE 'LITERAL )

    ;; https://www.youtube.com/watch?v=aa8jTf7Xg3E
    ;; https://i.ytimg.com/vi/aa8jTf7Xg3E/maxresdefault.jpg

    ;; https://www.youtube.com/watch?v=zdD_QygwRuY

    ))

