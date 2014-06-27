;; -*- coding: utf-8 -*-
;; part of Xah Lee's emacs init file.
;; 2011-05-28
;;   Xah Lee
;; ∑ http://xahlee.org/

(defun xah-add-to-related-links (φsource-file-path φdest-file-path)
  "Add current file as a link to the related links section of filename at point.

When called interactively, φsource-file-path is the path of current buffer, and φdest-file-path is the path/url under cursor.

When called interactively, the buffer for φdest-file-path is left unsaved and as current.

When called in lisp program, φsource-file-path and φdest-file-path should be file full paths. If changes are made to φdest-file-path, it returns t, else nil."
  (interactive
   (let* (
          (bds (get-selection-or-unit 'line))
          (p1 (elt bds 1) )
          (p2 (elt bds 2) )
          )
     (list (buffer-file-name)
           (xahsite-href-value-to-filepath (thing-at-point 'filename) (buffer-file-name)) )
     )
   )
  (let ( ξtitle ξnewHrefValue ξbuffer )
    (setq ξbuffer (find-file φdest-file-path ) )
    (goto-char 1)
    (setq ξnewHrefValue (xahsite-filepath-to-href-value φsource-file-path φdest-file-path) )
    (if (search-forward ξnewHrefValue nil t)
        (progn
          (when (called-interactively-p 'interactive) (message (format "Link 「%s」 already exists at 「%s」."  ξnewHrefValue φdest-file-path) ) )
          (kill-buffer ξbuffer)
          nil)
      (progn
        (setq ξtitle (xhm-get-html-file-title φsource-file-path))
        (goto-char 1)
        (if (search-forward "<div class=\"rltd\">" nil t)
            (progn (search-forward "<ul>" nil t)
                   (insert "\n" (format "<li><a href=\"%s\">%s</a></li>" ξnewHrefValue ξtitle))
                   )
          (progn 
            (goto-char (point-max))
            (search-backward "<div id=\"disqus_thread\">")
            (insert (format "<div class=\"rltd\">
<ul>
<li><a href=\"%s\">%s</a></li>
</ul>
</div>

" ξnewHrefValue ξtitle)) ) )
        (when (not (called-interactively-p 'interactive) )
          (write-region (point-min) (point-max) φdest-file-path)
          (kill-buffer)
          )
        t
        ) ) ) )

(defun xahsite-update-related-links (φfilePath φdestFileList)
  "Update related links tags.

Add the current page (φfilePath) as link to the “related pages” section at φdestFileList.

When called interactively, φfilePath is the current file. φdestFileList is file paths extracted from current text block or text selection.

When called in lisp program, φfilePath is a string. φdestFileList is list of filenames. All paths should be absolute path.

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
     (setq bds (get-selection-or-unit 'block))
     (setq p1 (elt bds 1) )
     (setq p2 (elt bds 2) )
     (list
      (buffer-file-name)
      (mapcar (lambda (ξx) (expand-file-name ξx (file-name-directory (buffer-file-name)) )) (xhm-extract-url (elt bds 0))) ) ) )

  (let (p3 p4 currentUrlList)
    (mapc
     (lambda (ξy)
       (xah-add-to-related-links φfilePath ξy)
       )
     φdestFileList)
    ))

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
      (insert "\n")
      )
    (while (search-forward "png\" width=\"" nil t)
      (search-backward "png")
      (setq fn (thing-at-point 'filename))
      (sgml-delete-tag 1)
      (insert fn)
      (xah-html-image-linkify)
      (insert "\n")
      )
))

(defun xah-fix-rm-span-quote (φstart φend)
  "remove “” around <span class=\"code\"></span> tags in current buffer."
  (interactive "r")

  (save-excursion
    (narrow-to-region φstart φend)
    (while
        (search-forward-regexp "“<span class=\"code\">\\([^<]+?\\)</span>”" nil t)
      (replace-match "<span class=\"code\">\\1</span>" t))
    )
  )

(defun xah-fix-to-html4strict (&optional φfName)
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

  (when φfName (find-file φfName))

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
             mystr
             ξchanges
             (changedItems '())
             (mybuff (current-buffer))
             )

      (goto-char (point-min)) ;; in case buffer already open
      (while (search-forward "<div class=\"img\">" nil t)
        (progn
          (setq p2 (point) )
          (backward-char 17)
          (setq p1 (point) )

          (forward-char 1)
          (sgml-skip-tag-forward 1)
          (setq p4 (point) )
          (backward-char 6)
          (setq p3 (point) )

          (when t
            (setq mystr (buffer-substring-no-properties p1 p4))
            (setq changedItems (cons mystr changedItems ) )
            
            (progn 
              (delete-region p3 p4 )
              (goto-char p3)
              (insert "</figure>")

              (delete-region p1 p2 )
              (goto-char p1)
              (insert "<figure>")
              (widen) )
            ) ) )

      (goto-char (point-min)) ;; in case buffer already open
      (while (search-forward "<div class=\"obj\">" nil t)
        (progn
          (setq p2 (point) )
          (backward-char 17)
          (setq p1 (point) )

          (forward-char 1)
          (sgml-skip-tag-forward 1)
          (setq p4 (point) )
          (backward-char 6)
          (setq p3 (point) )

          (when t
            (setq mystr (buffer-substring-no-properties p1 p4))
            (setq changedItems (cons mystr changedItems ) )
            
            (progn 
              (delete-region p3 p4 )
              (goto-char p3)
              (insert "</figure>")

              (delete-region p1 p2 )
              (goto-char p1)
              (insert "<figure>")
              (widen) )
            ) ) )

      (goto-char (point-min)) ;; in case buffer already open
      (while (search-forward "<p class=\"cpt\">" nil t)
        (progn
          (setq p2 (point) )
          (backward-char 15)
          (setq p1 (point) )

          (forward-char 1)
          (sgml-skip-tag-forward 1)
          (setq p4 (point) )
          (backward-char 4)
          (setq p3 (point) )

          (when t
            (setq mystr (buffer-substring-no-properties p1 p4))
            (setq changedItems (cons mystr changedItems ) )
            
            (progn 
              (delete-region p3 p4 )
              (goto-char p3)
              (insert "</figcaption>")

              (delete-region p1 p2 )
              (goto-char p1)
              (insert "<figcaption>")
              (widen) )
            ) ) )

      (with-output-to-temp-buffer "*changed items*" 
        (mapc (lambda ( ξchanges) (princ ξchanges) (princ "\n\n") ) changedItems)
        (set-buffer "*changed items*")
        (funcall 'html-mode)
        (set-buffer mybuff)
        ) )) )

(defun xah-fix-ellipsis (φstring &optional φfrom φto)
  "Change “...” to “…”.

When called interactively, work on current text block or text selection. (a “text block” is text between empty lines)

When called in lisp code, if φstring is non-nil, returns a changed string.  If φstring nil, change the text in the region between positions φfrom φto."
  (interactive
   (if (region-active-p)
       (list nil (region-beginning) (region-end))
     (let ((bds (get-selection-or-unit 'block)) )
       (list nil (elt bds 1) (elt bds 2)) ) ) )

  (let (workOnStringP inputStr outputStr)
    (setq workOnStringP (if φstring t nil))
    (setq inputStr (if workOnStringP φstring (buffer-substring-no-properties φfrom φto)))
    (setq outputStr (replace-regexp-in-string "\\.\\.\\." "…" inputStr))

    (if workOnStringP
        outputStr
      (save-excursion
        (delete-region φfrom φto)
        (goto-char φfrom)
        (insert outputStr) )) )
  )

(defun xah-fix-number-items-block  ()
    "Change “(1)” to “①” etc in current region or text block.
Also change 「<li>1. 」 to 「<li>① 」."
  (interactive)
  (let (bds p3 p4 inputStr resultStr myPairs (case-fold-search nil) (case-replace nil))
    (setq bds (get-selection-or-unit 'block))
    (setq inputStr (elt bds 0) )
    (setq p3 (elt bds 1) )
    (setq p4 (elt bds 2) )

(setq myPairs '(
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

(setq resultStr (replace-pairs-in-string inputStr myPairs))

(delete-region p3 p4)
(insert resultStr)
    )
  )


;; ;; macro to search and fix next occurrence of “alt=""”
;; ;; search for alt="", then fix it
;; (fset 'xah-fix-img-alt
;;    "\363alt=\"\"\C-m\347\347\252\361\344\353\351\C-ci")

(defun xah-add-dstp (φfpath)
  "Insert a file creation date like
“<div class=\"dstp\">2008-12.</div>”
to file at ΦFPATH."
(let (mybuffer)
    (setq mybuffer (find-file φfpath))

    (xah-put-dstp)

;;     (goto-char (point-max))
;;     (search-backward "<div class=\"cpr\">")
;;     (insert "<div class=\"dstp\">1997</div>\n")

;;    (save-buffer)
;;    (kill-buffer mybuffer)
))

(defun xah-put-dstp ()
  "Insert a file creation date like
“<div class=\"dstp\">2008-12.</div>”
in the appropriate footer location of the current XahLee.org HTML file.

This command requires the GetFileInfo command line util in OS X."
  (interactive)
  (let (cmdStr resultStr mydate)
    (setq cmdStr (concat "GetFileInfo -d " (buffer-file-name)))
    (setq resultStr (shell-command-to-string cmdStr))
    (setq mydate (replace-regexp-in-string "\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9][0-9][0-9]\\) .+\n" "\\3-\\1" resultStr))
    (goto-char (point-max))
    (search-backward "<div class=\"cpr\">")
    (insert "<div class=\"dstp\">" mydate "</div>\n")))

(defun xah-insert-dstp ()
  "Insert the file creation date in this format:
“<div class=\"dstp\">2008-12</div>”
at cursor position.

This command requires the GetFileInfo command line util in OS X."
  (interactive)
  (let (cmdStr resultStr mydate)
    (setq cmdStr (concat "GetFileInfo -d " (buffer-file-name)))
    (setq resultStr (shell-command-to-string cmdStr))
    (setq mydate (replace-regexp-in-string "\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9][0-9][0-9]\\) .+\n" "\\3-\\1" resultStr))
    (insert "<div class=\"dstp\">" mydate "</div>")))

(defun xah-fix-dstp ()
  "Remove the dstp tag and insert a xahlee at top if not exist.
e.g. remove
<div class=\"dstp\">2007-01-06</div>
at bottom.
Insert at top
<p class=\"αuth\"><a rel=\"author\" href=\"http://xahlee.org/Periodic_dosage_dir/t1/presences.html\">Xah Lee</a>, 2011-03-03</p>
"
  (interactive)
  (let (mybuffer ξdate p1 p2 p3ins)
    (goto-char (point-min))
    (if
        (search-forward-regexp "<div class=\"dstp\">\\([0-9][0-9][0-9][0-9]-[0-9][0-9]-?[0-9]?[0-9]?\\)</div>" nil t)
        (progn 
          (setq ξdate (match-string 1) )
          (setq p1 (line-beginning-position) )
          (setq p2 (line-end-position) )

          (goto-char (point-min))
          (if (search-forward-regexp "<p class=\"αuth\"><a rel=\"author\" href=\"\\([^/]+?\\)/Periodic_dosage_dir/t1/presences.html\">Xah Lee</a>" nil t)
              (progn 
                (kill-new ξdate)
                (delete-region p1 p2)
                (message "“<p>Xah Lee, ” exits. dstp is: %s. Copied to clipboard." ξdate)
                )
            (progn
              (goto-char (point-min))
              (if 
                  (search-forward "<div class=\"adbnr\"><a href=\"http://xahlee.org/ads.html\">YOUR<br>AD<br>HERE</a></div>" nil t)
                  (setq p3ins (point) )
                (progn
                  (goto-char (point-min))
                  (search-forward "</h1>")
                  (setq p3ins (point) )
                  )
                )

              (delete-region p1 p2)
              (insert "\n\n<p class=\"αuth\"><a rel=\"author\" href=\"http://xahlee.org/Periodic_dosage_dir/t1/presences.html\">Xah Lee</a>, " ξdate "</p>\n" ) ) )
          )
      (progn
        (message "%s" "no dstp") ) ) ))

