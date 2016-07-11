;; -*- coding: utf-8 -*-
;; part of Xah Lee's emacs init file.
;; 2011-05-28
;;   Xah Lee
;; ∑ http://xahlee.org/

(defun xah-add-to-related-links (*source-file-path *dest-file-path)
  "Add current file as a link to the related links section of filename at point.

When called interactively, *source-file-path is the path of current buffer, and *dest-file-path is the path/url under cursor.

When called interactively, the buffer for *dest-file-path is left unsaved and as current.

When called non-interactively, *source-file-path and *dest-file-path should be file full paths. If changes are made to *dest-file-path, it returns t, else nil."
  (interactive
   (let ( p1 p2  )
     (if (use-region-p)
         (progn (setq p1 (region-beginning))
                (setq p2 (region-end)))
       (progn (setq p1 (line-beginning-position))
              (setq p2 (line-end-position))))
     (list (buffer-file-name)
           (xahsite-href-value-to-filepath (thing-at-point 'filename) (buffer-file-name)))))
  (let ( -title -newHrefValue -buffer )
    (setq -buffer (find-file *dest-file-path ))
    (goto-char 1)
    (setq -newHrefValue (xahsite-filepath-to-href-value *source-file-path *dest-file-path))
    (if (search-forward -newHrefValue nil t)
        (progn
          (when (called-interactively-p 'interactive)
            (message
             (format
              "Already exists: 「%s」  at 「%s」"
              (file-name-nondirectory -newHrefValue)
              (file-name-nondirectory *dest-file-path))))
          (kill-buffer -buffer)
          nil)
      (progn
        (setq -title (xah-html-get-html-file-title *source-file-path))
        (goto-char 1)
        (if (search-forward "<div class=\"rltd\">" nil t)
            (progn (search-forward "<ul>" nil t)
                   (insert "\n" (format "<li><a href=\"%s\">%s</a></li>" -newHrefValue -title)))
          (progn
            (goto-char (point-max))
            (search-backward "<div id=\"disqus_thread\">")
            (insert (format "<div class=\"rltd\">
<ul>
<li><a href=\"%s\">%s</a></li>
</ul>
</div>

" -newHrefValue -title))))
        (when (not (called-interactively-p 'interactive))
          (write-region (point-min) (point-max) *dest-file-path)
          (kill-buffer))
        t
        ))))

(defun xahsite-update-related-links (*filePath *destFileList)
  "Update related links tags.

Add the current page (*filePath) as link to the “related pages” section at *destFileList.

When called interactively, *filePath is the current file. *destFileList is file paths extracted from current text block or text selection.

When called non-interactively, *filePath is a string. *destFileList is list of filenames. All paths should be absolute path.

The related pages are HTML “div.rltd” element, having this form

<div class=\"rltd\">
<ul>
<li><a href=\"aspell_spell_checking.html\">unix: aspell Tutorial</a></li>
<li><a href=\"hunspell_spell_checking.html\">Hunspell Tutorial</a></li>
<li><a href=\"../emacs/emacs_spell_checker_problems.html\">Emacs Spell Checker Problems</a></li>
</ul>
</div>"
  (interactive
   (let (bds p1 p2)
     (setq bds
           (if (use-region-p)
               (vector (buffer-substring-no-properties (region-beginning) (region-end)) (region-beginning) (region-end))
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
     (setq p1 (aref bds 1))
     (setq p2 (aref bds 2))
     (list
      (buffer-file-name)
      (mapcar (lambda (-x) (expand-file-name -x (file-name-directory (buffer-file-name)))) (xah-html-extract-url p1 p2)))))

  (let (p3 p4)
    (mapc
     (lambda (-y)
       (xah-add-to-related-links *filePath -y))
     *destFileList)))

(defun xah-fix-add-alts ()
  "Add the alt attribute to image tags in current file.
This code is specific to xahlee.org ."
  (interactive)
  (let (fn)
    (goto-char (point-min))
    (while (search-forward "jpg\" width=\"" nil t)
      (search-backward "jpg")
      (setq fn (thing-at-point 'filename))
      (sgml-delete-tag 1)
      (insert fn)
      (xah-html-image-linkify)
      (insert "\n"))
    (while (search-forward "png\" width=\"" nil t)
      (search-backward "png")
      (setq fn (thing-at-point 'filename))
      (sgml-delete-tag 1)
      (insert fn)
      (xah-html-image-linkify)
      (insert "\n"))
    ))

(defun xah-fix-rm-span-quote (*start *end)
  "remove “” around <span class=\"code\"></span> tags in current buffer."
  (interactive "r")

  (save-excursion
    (narrow-to-region *start *end)
    (while
        (search-forward-regexp "“<span class=\"code\">\\([^<]+?\\)</span>”" nil t)
      (replace-match "<span class=\"code\">\\1</span>" t))))

(defun xah-fix-to-html4strict (&optional *fName)
  "Change buffer content from HTML4 trans to HTML4 strict,
by performing some custome set of find-replace operations.

If FNAME is given, then process that file.

The change is based on few simple regex replacements. So, there may be errors.
 (this function is for Xah only because it assums some specific HTML formatting style)

todo:
• consecutive img tags should have single div wrap, not on each.
• img wrapped by “a href” should be wraped, not inside the img tag
This function is specific to xahlee.org. 2008-05-10."
  (interactive)

  (when *fName (find-file *fName))

;; wrap div.img to “<img …>”
  (goto-char (point-min))
  (while (search-forward-regexp "

<img +src=\"\\([^\"]+\\)\" +alt=\"\\([^\"]+\\)?\" +width=\"\\([0-9]+\\)\" +height=\"\\([0-9]+\\)\" ?>" nil t)
    (replace-match "

<div class=\"img\"><img src=\"\\1\" alt=\"\\2\" width=\"\\3\" height=\"\\4\"></div>" t))

;; fix “<a><div.img><img></div></a>” to “<div.img><a><img></a></div>”
  (goto-char (point-min))
  (while (search-forward-regexp "<a href=\"\\([^\"]+\\)\"><div class=\"img\"><img src=\"\\([^\"]+\\)\" alt=\"\\([^\"]+\\)\" width=\"\\([0-9]+\\)\" height=\"\\([0-9]+\\)\"></div></a>" nil t)
    (replace-match "<div class=\"img\"><a href=\"\\1\"><img src=\"\\2\" alt=\"\\3\" width=\"\\4\" height=\"\\5\"></a></div>" t))

;; consecutive img should just have one div.img wrap, not on each.
;; remove “</div><div.img>”
  (goto-char (point-min))
  (while (search-forward-regexp "height=\"\\([0-9]+\\)\"></div>
<div class=\"img\">" nil t)
    (replace-match "height=\"\\1\">
" t))

;; change “<img…></div><p class="cpt">…</p>” to  “<img…><p class="cpt">…</p></div>”
  (goto-char (point-min))
  (while (search-forward-regexp "<img +src=\"\\([^\"]+\\)\" +alt=\"\\([^\"]+\\)?\" +width=\"\\([0-9]+\\)\" +height=\"\\([0-9]+\\)\" ?></div>
*<p class=\"cpt\">
* *\\([^§]+?\\)</p>" nil t)
    (replace-match "<img src=\"\\1\" alt=\"\\2\" width=\"\\3\" height=\"\\4\"><p class=\"cpt\">\\5</p></div>" t))

;; wrap img tag to “<img…>\n<p>above:” pairs.
;;    (goto-char (point-min))
;;    (while (search-forward-regexp "<img +src=\"\\([^\"]+\\)\" +alt=\"\\([^\"]+\\)?\" +width=\"\\([0-9]+\\)\" +height=\"\\([0-9]+\\)\" ?>\n*<p>\n*above ?: ?\n?\\([^§]+?\\)</p>" nil t) (replace-match "<div class=\"img\"><img src=\"\\1\" alt=\"\\2\" width=\"\\3\" height=\"\\4\"><p>above: \\5</p></div>" t))

;;(goto-char (point-min))
;;(while (search-forward-regexp "<blockquote>\n?«?\n?\\([^<]+\\)\n?»?\n?</blockquote>" nil t) (replace-match "<blockquote><p>\\1</p></blockquote>"))

;;     (goto-char (point-min))
;;     (while (search-forward-regexp "</blockquote>\n+<blockquote>" nil t) (replace-match "" t t))
  )

(defun xah-fix-wrap-img-figure ()
  "Change current buffer's <div class=\"img\"> to <figure> and <p class=\"cpt\"> to <figcaption>."
  (interactive)

  (save-excursion
    (let (p1 p2 p3 p4
             -str
             -changes
             (-changedItems '())
             (-buff (current-buffer)))

      (goto-char (point-min)) ;; in case buffer already open
      (while (search-forward "<div class=\"img\">" nil t)
        (progn
          (setq p2 (point))
          (backward-char 17)
          (setq p1 (point))

          (forward-char 1)
          (sgml-skip-tag-forward 1)
          (setq p4 (point))
          (backward-char 6)
          (setq p3 (point))

          (when t
            (setq -str (buffer-substring-no-properties p1 p4))
            (setq -changedItems (cons -str -changedItems ))

            (progn
              (delete-region p3 p4 )
              (goto-char p3)
              (insert "</figure>")

              (delete-region p1 p2 )
              (goto-char p1)
              (insert "<figure>")
              (widen)))))

      (goto-char (point-min)) ;; in case buffer already open
      (while (search-forward "<div class=\"obj\">" nil t)
        (progn
          (setq p2 (point))
          (backward-char 17)
          (setq p1 (point))

          (forward-char 1)
          (sgml-skip-tag-forward 1)
          (setq p4 (point))
          (backward-char 6)
          (setq p3 (point))

          (when t
            (setq -str (buffer-substring-no-properties p1 p4))
            (setq -changedItems (cons -str -changedItems ))

            (progn
              (delete-region p3 p4 )
              (goto-char p3)
              (insert "</figure>")

              (delete-region p1 p2 )
              (goto-char p1)
              (insert "<figure>")
              (widen)))))

      (goto-char (point-min)) ;; in case buffer already open
      (while (search-forward "<p class=\"cpt\">" nil t)
        (progn
          (setq p2 (point))
          (backward-char 15)
          (setq p1 (point))

          (forward-char 1)
          (sgml-skip-tag-forward 1)
          (setq p4 (point))
          (backward-char 4)
          (setq p3 (point))

          (when t
            (setq -str (buffer-substring-no-properties p1 p4))
            (setq -changedItems (cons -str -changedItems ))

            (progn
              (delete-region p3 p4 )
              (goto-char p3)
              (insert "</figcaption>")

              (delete-region p1 p2 )
              (goto-char p1)
              (insert "<figcaption>")
              (widen)))))

      (with-output-to-temp-buffer "*changed items*"
        (mapc (lambda ( -changes) (princ -changes) (princ "\n\n")) -changedItems)
        (set-buffer "*changed items*")
        (funcall 'html-mode)
        (set-buffer -buff)))))

(defun xah-fix-ellipsis (*string &optional *from *to)
  "Change “...” to “…”.

When called interactively, work on current text block or text selection. (a “text block” is text between blank lines)

When called non-interactively, if *string is non-nil, returns a changed string.  If *string nil, change the text in the region between positions *from *to."
  (interactive
   (if (use-region-p)
       (list nil (region-beginning) (region-end))
     (let (pt1 pt2)
       (save-excursion
         (if (re-search-backward "\n[ \t]*\n" nil "move")
             (progn (re-search-forward "\n[ \t]*\n")
                    (setq pt1 (point)))
           (setq pt1 (point)))
         (if (re-search-forward "\n[ \t]*\n" nil "move")
             (progn (re-search-backward "\n[ \t]*\n")
                    (setq pt2 (point)))
           (setq pt2 (point)))
         (list nil pt1 pt2)))))

  (let (-workOnStringP -inputStr -outputStr)
    (setq -workOnStringP (if *string t nil))
    (setq -inputStr (if -workOnStringP *string (buffer-substring-no-properties *from *to)))
    (setq -outputStr (replace-regexp-in-string "\\.\\.\\." "…" -inputStr))

    (if -workOnStringP
        -outputStr
      (save-excursion
        (delete-region *from *to)
        (goto-char *from)
        (insert -outputStr)))))

(defun xah-fix-number-items-block  ()
  "Change “(1)” to “①” etc in current region or text block.
Also change 「<li>1. 」 to 「<li>① 」."
  (interactive)
  (let* (
         p1 p2
         (*setp1p2
          (save-excursion
            (if (re-search-backward "\n[ \t]*\n" nil "move")
                (progn (re-search-forward "\n[ \t]*\n")
                       (setq p1 (point)))
              (setq p1 (point)))
            (if (re-search-forward "\n[ \t]*\n" nil "move")
                (progn (re-search-backward "\n[ \t]*\n")
                       (setq p2 (point)))
              (setq p2 (point)))))
         (-inputStr (buffer-substring-no-properties p1 p2))
         (-pairs '(
                   ["(0)" "⓪"]
                   ["(1)" "①"]
                   ["(2)" "②"]
                   ["(3)" "③"]
                   ["(4)" "④"]
                   ["(5)" "⑤"]
                   ["(6)" "⑥"]
                   ["(7)" "⑦"]
                   ["(8)" "⑧"]
                   ["(9)" "⑨"]

                   ["0. " "⓪ "]
                   ["1. " "① "]
                   ["2. " "② "]
                   ["3. " "③ "]
                   ["4. " "④ "]
                   ["5. " "⑤ "]
                   ["6. " "⑥ "]
                   ["7. " "⑦ "]
                   ["8. " "⑧ "]
                   ["9. " "⑨ "]

                   ["(A)" "Ⓐ"]
                   ["(B)" "Ⓑ"]
                   ["(C)" "Ⓒ"]
                   ["(D)" "Ⓓ"]
                   ["(E)" "Ⓔ"]
                   ["(F)" "Ⓕ"]
                   ["(G)" "Ⓖ"]
                   ["(H)" "Ⓗ"]
                   ["(I)" "Ⓘ"]
                   ["(J)" "Ⓙ"]

                   ["(a)" "ⓐ"]
                   ["(b)" "ⓑ"]
                   ["(c)" "ⓒ"]
                   ["(d)" "ⓓ"]
                   ["(e)" "ⓔ"]
                   ["(f)" "ⓕ"]
                   ["(g)" "ⓖ"]
                   ["(h)" "ⓗ"]
                   ["(i)" "ⓘ"]
                   ["(j)" "ⓙ"]
                   ))
         (case-fold-search nil)
         (case-replace nil))
    (delete-region p1 p2)
    (insert (xah-replace-pairs-in-string -inputStr -pairs))))

;; ;; macro to search and fix next occurrence of “alt=""”
;; ;; search for alt="", then fix it
;; (fset 'xah-fix-img-alt
;;    "\363alt=\"\"\C-m\347\347\252\361\344\353\351\C-ci")

(defun xah-add-dstp (*fpath)
  "Insert a file creation date like
“<div class=\"dstp\">2008-12.</div>”
to file at ΦFPATH."
(let (-buffer)
    (setq -buffer (find-file *fpath))

    (xah-put-dstp)

;;     (goto-char (point-max))
;;     (search-backward "<div class=\"cpr\">")
;;     (insert "<div class=\"dstp\">1997</div>\n")

;;    (save-buffer)
;;    (kill-buffer -buffer)
))

(defun xah-put-dstp ()
  "Insert a file creation date like
“<div class=\"dstp\">2008-12.</div>”
in the appropriate footer location of the current XahLee.org HTML file.

This command requires the GetFileInfo command line util in OS X."
  (interactive)
  (let (-cmdStr -resultStr -date)
    (setq -cmdStr (concat "GetFileInfo -d " (buffer-file-name)))
    (setq -resultStr (shell-command-to-string -cmdStr))
    (setq -date (replace-regexp-in-string "\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9][0-9][0-9]\\) .+\n" "\\3-\\1" -resultStr))
    (goto-char (point-max))
    (search-backward "<div class=\"cpr\">")
    (insert "<div class=\"dstp\">" -date "</div>\n")))

(defun xah-insert-dstp ()
  "Insert the file creation date in this format:
“<div class=\"dstp\">2008-12</div>”
at cursor position.

This command requires the GetFileInfo command line util in OS X."
  (interactive)
  (let (-cmdStr -resultStr -date)
    (setq -cmdStr (concat "GetFileInfo -d " (buffer-file-name)))
    (setq -resultStr (shell-command-to-string -cmdStr))
    (setq -date (replace-regexp-in-string "\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9][0-9][0-9]\\) .+\n" "\\3-\\1" -resultStr))
    (insert "<div class=\"dstp\">" -date "</div>")))

(defun xah-fix-dstp ()
  "Remove the dstp tag and insert a xahlee at top if not exist.
e.g. remove
<div class=\"dstp\">2007-01-06</div>
at bottom.
Insert at top
<p class=\"αuth\"><a rel=\"author\" href=\"http://xahlee.org/Periodic_dosage_dir/t1/presences.html\">Xah Lee</a>, 2011-03-03</p>
"
  (interactive)
  (let (-date p1 p2 -p3ins)
    (goto-char (point-min))
    (if
        (search-forward-regexp "<div class=\"dstp\">\\([0-9][0-9][0-9][0-9]-[0-9][0-9]-?[0-9]?[0-9]?\\)</div>" nil t)
        (progn
          (setq -date (match-string 1))
          (setq p1 (line-beginning-position))
          (setq p2 (line-end-position))

          (goto-char (point-min))
          (if (search-forward-regexp "<p class=\"αuth\"><a rel=\"author\" href=\"\\([^/]+?\\)/Periodic_dosage_dir/t1/presences.html\">Xah Lee</a>" nil t)
              (progn
                (kill-new -date)
                (delete-region p1 p2)
                (message "“<p>Xah Lee, ” exits. dstp is: %s. Copied to clipboard." -date))
            (progn
              (goto-char (point-min))
              (if
                  (search-forward "<div class=\"adbnr\"><a href=\"http://xahlee.org/ads.html\">YOUR<br>AD<br>HERE</a></div>" nil t)
                  (setq -p3ins (point))
                (progn
                  (goto-char (point-min))
                  (search-forward "</h1>")
                  (setq -p3ins (point))))

              (delete-region p1 p2)
              (insert "\n\n<p class=\"αuth\"><a rel=\"author\" href=\"http://xahlee.org/Periodic_dosage_dir/t1/presences.html\">Xah Lee</a>, " -date "</p>\n" ))))
      (progn
        (message "%s" "no dstp")))))

