;; -*- coding: utf-8 -*-
;; 2010-09-03

;; Replace “…” to one of 〔…〕, 「…」, 【…】 or HTML tag. Or other similar text processing.

(defun xah-corner-bracket→html-i (φp1 φp2)
       "Replace all 「…」 to <code>…</code> in current text block.
When called with `universal-argument', work on visible portion of whole buffer (i.e. respect `narrow-to-region'). When call in lisp program, the φp1 φp2 are region positions."
       (interactive
        (cond
         ((equal current-prefix-arg nil) ; universal-argument not called
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
                     (list pt1 pt2))))
         (t ; all other cases
          (list (point-min) (point-max)))))
       (save-excursion
         (save-restriction
           (narrow-to-region φp1 φp2)
           (goto-char (point-min))
           (while (search-forward-regexp "「\\([^」]+?\\)」" nil t)
             (if (y-or-n-p "Replace this one?")
                 (replace-match "<code>\\1</code>" t) ) ) )) )

(defun xah-brackets-to-html (φp1 φp2)
  "Replace bracketed text to HTML markup in current block on text selection.

• 「…」 → <code>…</code>
• 〈…〉 → <cite>…</cite>
• 《…》 → <cite class=\"book\">…</cite>
• 〔…〕 → <code class=\"path-α\">\\1</code>
•  ‹…› → <var class=\"d\">…</var>
• 〔<…〕 → 〔➤ <…〕

When called in lisp program, the arguments φp1 φp2 are region positions."
  (interactive
   (let (ξ1 ξ2)
     (if (use-region-p)
         (progn
           (setq ξ1 (region-beginning))
           (setq ξ2 (region-end)))
       (progn
         (save-excursion
           (if (re-search-backward "\n[ \t]*\n" nil "move")
               (progn (re-search-forward "\n[ \t]*\n")
                      (setq ξ1 (point)))
             (setq ξ1 (point)))
           (if (re-search-forward "\n[ \t]*\n" nil "move")
               (progn (re-search-backward "\n[ \t]*\n")
                      (setq ξ2 (point)))
             (setq ξ2 (point))))))
     (list ξ1 ξ2)))
  (let ((ξchangedItems '()))

    (save-excursion
      (save-restriction
        (narrow-to-region φp1 φp2)

        (goto-char (point-min))
        (while (search-forward-regexp "「\\([^」]+?\\)」" nil t)
          (setq ξchangedItems (cons (match-string-no-properties 1) ξchangedItems ))
          (replace-match "<code>\\1</code>" t))

        (goto-char (point-min))
        (while (search-forward-regexp "〈\\([^〉]+?\\)〉" nil t)
          (setq ξchangedItems (cons (match-string-no-properties 1) ξchangedItems ))
          (replace-match "<cite>\\1</cite>" t))

        (goto-char (point-min))
        (while (search-forward-regexp "《\\([^》]+?\\)》" nil t)
          (setq ξchangedItems (cons (match-string-no-properties 1) ξchangedItems ))
          (replace-match "<cite class=\"book\">\\1</cite>" t))

        (goto-char (point-min))
        (while (search-forward-regexp "‹\\([^›]+?\\)›" nil t)
          (setq ξchangedItems (cons (match-string-no-properties 1) ξchangedItems ))
          (replace-match "<var class=\"d\">\\1</var>" t))

        (goto-char (point-min))
        (while (search-forward-regexp "〔<a href=" nil t)
          (setq ξchangedItems (cons (match-string-no-properties 1) ξchangedItems ))
          (replace-match "〔➤ <a href=" t))

        (goto-char (point-min))
        (while (search-forward-regexp "〔\\([ -_/\\:~.A-Za-z0-9%]+?\\)〕" nil t)
          (setq ξchangedItems (cons (match-string-no-properties 1) ξchangedItems ))
          (replace-match "<code class=\"path-α\">\\1</code>" t))))

    (mapcar
     (lambda (ξx)
       (princ ξx)
       (terpri))
     (reverse ξchangedItems))))

(defun xah-angle-brackets-to-html (φp1 φp2)
  "Replace all 〈…〉 to <cite>…</cite> and 《…》 to <cite class=\"book\">…</span>.

If there's no text selection, work on current text block, else, on text selection.

When call in lisp program, the arguments φp1 φp2 are region positions.

URL `http://ergoemacs.org/emacs/elisp_replace_title_tags.html'
version 2014-11-14
"
  (interactive
   (let (ξ1 ξ2)
     (save-excursion
       (if (re-search-backward "\n[ \t]*\n" nil "move")
           (progn (re-search-forward "\n[ \t]*\n")
                  (setq ξ1 (point)))
         (setq ξ1 (point)))
       (if (re-search-forward "\n[ \t]*\n" nil "move")
           (progn (re-search-backward "\n[ \t]*\n")
                  (setq ξ2 (point)))
         (setq ξ2 (point))))
     (list ξ1 ξ2)))

  (let ((ξchangedItems '())
        (ξinputStr (buffer-substring-no-properties φp1 φp2))
        ξresultStr
        (case-fold-search nil))

    (setq ξresultStr
          (with-temp-buffer
            (insert ξinputStr)

            (goto-char 1)
            (while (search-forward-regexp "《\\([^》]+?\\)》" nil t)
              (setq ξchangedItems (cons (match-string-no-properties 1) ξchangedItems ))
              (replace-match "<cite class=\"book\">\\1</cite>" "FIXEDCASE"))

            (goto-char 1)
            (while (search-forward-regexp "〈\\([^〉]+?\\)〉" nil t)
              (setq ξchangedItems (cons (match-string-no-properties 1) ξchangedItems ))
              (replace-match "<cite>\\1</cite>" t))

            (buffer-string)))

    (if (> (length ξchangedItems) 0)
        (progn
          (delete-region φp1 φp2)
          (insert ξresultStr)

          (mapcar
           (lambda (ξx)
             (princ ξx)
             (terpri))
           (reverse ξchangedItems)))
      (message "No change needed."))))

(defun xah-remove-square-brackets ()
  "Delete any text of the form “[‹n›]”, ⁖ [1], [2], ….
Works on text selection or current text block.

For example
 「… announced as Blu-ray Disc [11][12], and …」
becomes
 「… announced as Blu-ray Disc, and …」.

URL `http://ergoemacs.org/emacs/elisp_replace_title_tags.html'
Version 2014-11-14"
  (interactive)
  (let (p1 p2 ξinputStr ξresultStr ξchangedItems)
    (save-excursion ; set p1 p2
      (if (re-search-backward "\n[ \t]*\n" nil "move")
          (progn (re-search-forward "\n[ \t]*\n")
                 (setq p1 (point)))
        (setq p1 (point)))
      (if (re-search-forward "\n[ \t]*\n" nil "move")
          (progn (re-search-backward "\n[ \t]*\n")
                 (setq p2 (point)))
        (setq p2 (point))))

    (setq ξinputStr (buffer-substring-no-properties p1 p2))

    (setq ξchangedItems '())

    (setq ξresultStr
          (with-temp-buffer
            (insert ξinputStr)

            (goto-char 1)
            (while (search-forward-regexp "\\(\\[[0-9]+?\\]\\)" nil t)
              (setq ξchangedItems (cons (match-string 1) ξchangedItems ))
              (replace-match "" t))

            (goto-char 1)
            (while (search-forward "[citation needed]" nil t)
              (setq ξchangedItems (cons "[citation needed]" ξchangedItems ))
              (backward-char 17)
              (delete-char 17))

            (buffer-string)))

    (if (> (length ξchangedItems) 0)
        (progn
          (delete-region p1 p2)
          (insert ξresultStr)
          (mapcar
           (lambda (ξx)
             (princ ξx)
             (terpri))
           (reverse ξchangedItems)))
      (message "No change needed."))))


(defun xah-curly-quotes→bracket (φleft-bracket φright-bracket)
  "Replace “…” to one of 「…」.
Which bracket is determined by the string LEFTBRACKET and RIGHTBRACKET."
  (interactive)
  (let ()
    (if (string= major-mode "dired-mode")
        (progn
          (dired-do-query-replace-regexp
           "“\\([^”]+?\\)”"
           (concat φleft-bracket "\\1" φright-bracket)
           ))
      (progn (query-replace-regexp
              "“\\([^”]+?\\)”"
           (concat φleft-bracket "\\1" φright-bracket) )) ) ))

(defun xah-curly-quotes→code-bracket ()
  "Replace “…” to 「…」"
  (interactive)
  (xah-curly-quotes→bracket "「" "」")
)

(defun xah-curly-quotes→html-code-tag ()
  "Replace 「“…”」 to 「<code>…</code>」"
  (interactive)
  (xah-curly-quotes→bracket "<code>" "</code>")
)

(defun xah-curly-quotes→html-strong-tag ()
  "Replace 「“…”」 to 「<strong>…</strong>」"
  (interactive)
  (xah-curly-quotes→bracket "<strong>" "</strong>")
)

(defun xah-curly-quotes→elisp-function-bracket ()
  "Replace “…” to ｢…｣"
  (interactive)
  (xah-curly-quotes→bracket "｢" "｣")
)

(defun xah-curly-quotes→french-quote ()
  "Replace “…” to «…»"
  (interactive)
  (xah-curly-quotes→bracket "«" "»")
)

(defun xah-curly-quotes→kbd-tag ()
  "Replace “…” to <kbd>…</kbd>"
  (interactive)
  (xah-curly-quotes→bracket "<kbd>" "</kbd>")
)

(defun xah-curly-quotes→keyboard-bracket ()
  "Replace “…” to 【…】"
  (interactive)
  (xah-curly-quotes→bracket "【" "】")
)

(defun xah-curly-quotes→menu-bracket ()
  "Replace “…” to 〖…〗"
  (interactive)
  (xah-curly-quotes→bracket "〖" "〗")
)

(defun xah-curly-quotes→book-bracket ()
  "Replace “…” to 《…》"
  (interactive)
  (xah-curly-quotes→bracket "《" "》")
)

(defun xah-curly-quotes→title-bracket ()
  "Replace “…” to 〈…〉"
  (interactive)
  (xah-curly-quotes→bracket "〈" "〉")
)

(defun xah-curly-quotes→file-path ()
  "Replace “…” to 〔…〕"
  (interactive)
  (xah-curly-quotes→bracket "〔" "〕")
)

;; (defun curly-quotes-replacement ()
;;   "to be used …
;; TODO

;; Replace “…” to one of 〔…〕, 「…」, 【…】"
;;   (interactive)
;;   (let (replacePattern)

;;     (goto-char 1)
;;     (search-forward-regexp "“\\([^”]+?\\)”" nil t)

;;     (cond
;;      ((or
;;        (string-match "^Ctrl" (match-string-no-properties 1 ) )
;;        (string-match "^Alt" (match-string-no-properties 1 ) )
;;        (string-match "^Win" (match-string-no-properties 1 ) )
;;        (string-match "^Menu" (match-string-no-properties 1 ) )
;;        (string-match "^Meta" (match-string-no-properties 1 ) )
;;        (string-match "^Cmd" (match-string-no-properties 1 ) )
;;        (string-match "^Opt" (match-string-no-properties 1 ) )
;;        (string-match "^Super" (match-string-no-properties 1 ) )
;;        (string-match "^Hyper" (match-string-no-properties 1 ) )
;;        )
;;       (setq replacePattern "【\1】" )
;;       )
;;      (CONDITION BODY)
;;      )

;;     )
;;   )

