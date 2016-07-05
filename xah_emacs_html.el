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

(defun xah-add-reference-span-tag ()
  "Add <span class=\"ref\">…</span> tag to current HTML element or text selection.
Version 2015-08-12"
  (interactive)
  (require 'xah-html-mode)
  (let ( ξp1 ξp2 )
    (if (use-region-p)
        (progn (setq ξp1 (region-beginning))
               (setq ξp2 (region-end)))
      (progn
        (xah-html-skip-tag-backward)
        (setq ξp1 (point))
        (xah-html-skip-tag-forward)
        (setq ξp2 (point))))
    (set-mark ξp1)
    (goto-char ξp2)
    (xah-html-add-open-close-tags "span" "ref" ξp1 ξp2)
    ;; (xah-html-wrap-html-tag "span" "ref")
    ))

(defun xahsite-update-article-timestamp ()
  "Update article's timestamp.
Add today's date to the “byline” tag of current file, also delete the last one if there are more than one.
Also, move cursor there.
Also, pushes mark. You can go back to previous location `exchange-point-and-mark'.
WARNING: This command saves buffer if it's a file.
Version 2016-04-12"
  (interactive)
  (let (ξp1 ξp2 ξnum ξbufferTextOrig)
    (push-mark)
    (goto-char 1)
    (when (search-forward "<div class=\"byline\">" nil)
      (progn
        ;; set ξp1 ξp2. they are boundaries of inner text
        (setq ξp1 (point))
        (backward-char 1)
        (search-forward "</div>" )
        (backward-char 6)
        (setq ξp2 (point)))

      (let ((ξbylineText (buffer-substring-no-properties ξp1 ξp2)))
        (when (> (length ξbylineText) 110)
          (user-error "something's probably wrong. the length for the byline is long: 「%s」" ξbylineText )))

      (save-restriction
        (narrow-to-region ξp1 ξp2)

        (setq ξbufferTextOrig (buffer-string ))
        (setq ξnum (count-matches "<time>" (point-min) (point-max)))

        (if (equal ξnum 1)
            (progn
              (goto-char (point-min))
              (search-forward "</time>")
              (insert ". Last updated: ")
              (insert (format "<time>%s</time>" (format-time-string "%Y-%m-%d")))
              (when (not (looking-at "\\.")) (insert ".")))

          (progn
            ;; if there are more than 1 “time” tag, delete the last one
            (let (ξp3 ξp4)
              (goto-char (point-max))
              (search-backward "</time>")
              (search-forward "</time>")
              (setq ξp4 (point))
              (search-backward "<time>")
              (setq ξp3 (point))
              (delete-region ξp3 ξp4 ))

            (insert (format "<time>%s</time>" (format-time-string "%Y-%m-%d")))
            (when (not (looking-at "\\.")) (insert "."))
            (goto-char (point-max))))

        ;; backup
        (let ((ξfname (buffer-file-name)))
          (if ξfname
              (let ((ξbackup-name
                     (concat ξfname "~" (format-time-string "%Y%m%dT%H%M%S") "~")))
                (copy-file ξfname ξbackup-name t)
                (message (concat "Backup saved at: " ξbackup-name)))))

        (save-buffer)
        (message "%s\nchanged to\n%s" ξbufferTextOrig (buffer-string )))
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
"
  (interactive)
  (require 'sgml-mode)
  (let* (
         ξp1 ξp2
         (_setBoundary
          (save-excursion
            (if (re-search-backward "\n[ \t]*\n" nil "move")
                (progn (re-search-forward "\n[ \t]*\n")
                       (setq ξp1 (point)))
              (setq ξp1 (point)))
            (if (re-search-forward "\n[ \t]*\n" nil "move")
                (progn (re-search-backward "\n[ \t]*\n")
                       (setq ξp2 (point)))
              (setq ξp2 (point)))))
         (ξfileList (split-string (buffer-substring-no-properties ξp1 ξp2) "\n" t))
         ξpageNavStr )

    (delete-region ξp1 ξp2)

    ;; generate the page nav string
    (setq ξpageNavStr
          (format "<nav class=\"page\">\n%s</nav>"
                  (let (ξresult ξlinkPath ξfTitle (ξi 0))
                    (while (< ξi (length ξfileList))
                      (setq ξlinkPath (elt ξfileList ξi))
                      (setq ξfTitle (xah-html-get-html-file-title ξlinkPath))
                      (setq ξresult (concat ξresult "<a href=\"" ξlinkPath "\" title=\"" ξfTitle "\">" (number-to-string (1+ ξi)) "</a>\n"))
                      (setq ξi (1+ ξi)))
                    ξresult
                    )))

    ;; open each file, insert the page nav string
    (mapc
     (lambda (thisFile)
       (message "%s" thisFile)
       (find-file thisFile)
       (goto-char 1)

       (if
           (search-forward "<nav class=\"page\">" nil t)
           (let (ξp3 ξp4 )
             (search-backward "<")
             (setq ξp3 (point))
             (sgml-skip-tag-forward 1)
             (setq ξp4 (point))
             (delete-region ξp3 ξp4)
             (insert ξpageNavStr))
         (progn
           (search-forward "<script><!--
google_ad_client")
           (progn
             (search-backward "<script>")
             (insert ξpageNavStr "\n\n")))))
     ξfileList)))

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
  "Syntax color CSS's HSL color spec ⁖ 「hsl(0,90%,41%)」 in current buffer.
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

(defun xah-html-rename-html-inline-image ()
  "Replace current HTML inline image's file name.
This command is for interactive use only.
When cursor is in HTML link file path, e.g.  <img src=\"img/emacs_logo.png\" > and this command is called, it'll prompt user for a new name. The link path will be changed to the new name, the corresponding file will also be renamed. The operation is aborted if a name exists.
Version 2015-08-07"
  (interactive)
  (let* (
         (ξbounds (bounds-of-thing-at-point 'filename))
         (ξinputPath (buffer-substring-no-properties (car ξbounds) (cdr ξbounds)))
         (ξexpandedPath (expand-file-name ξinputPath (file-name-directory (or (buffer-file-name) default-directory ))))
         (ξnewPath (read-string "New name: " ξexpandedPath nil ξexpandedPath )))
    (if (file-exists-p ξnewPath)
        (progn (user-error "file 「%s」 exist." ξnewPath ))
      (progn
        (rename-file ξexpandedPath ξnewPath)
        (message "rename to %s" ξnewPath)
        (delete-region (car ξbounds) (cdr ξbounds))
        (insert (xahsite-filepath-to-href-value ξnewPath (or (buffer-file-name) default-directory)))))))

(defun xah-move-image-file (dirName fileName)
  "move image file at
~/Downloads/xx.jpg
to current dir of subdir i
and rename file to current line's text.
2016-07-01"
  (interactive "DMove to dir:
sNew file name:")
  (let ( moveToPath )
    (setq moveToPath (concat dirName fileName ".jpg"))
    (if (file-exists-p moveToPath)
        (message "move to path exist: %s" moveToPath)
      (progn
        (rename-file (expand-file-name "~/Downloads/xx.jpg") moveToPath)
        (find-file moveToPath )
        (message "move to path: %s" moveToPath)))))

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

