;; -*- coding: utf-8 -*-
;; 2010-09-03

;; Replace “…” to one of 〔…〕, 「…」, 【…】 or HTML tag. Or other similar text processing.

(defun xah-corner-bracket→html-i (φbegin φend)
       "Replace all 「…」 to <code>…</code> in current text block.
When called with `universal-argument', work on visible portion of whole buffer (i.e. respect `narrow-to-region'). When call in lisp program, the φbegin φend are region positions."
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
           (narrow-to-region φbegin φend)
           (goto-char (point-min))
           (while (search-forward-regexp "「\\([^」]+?\\)」" nil t)
             (if (y-or-n-p "Replace this one?")
                 (replace-match "<code>\\1</code>" t) ) ) )) )

(defun xah-angle-brackets-to-html (φbegin φend)
  "Replace all 〈…〉 to <cite>…</cite> and 《…》 to <cite class=\"book\">…</span> in current text block or selection.

When called non-interactively, φbegin φend are region positions.

URL `http://ergoemacs.org/emacs/elisp_replace_title_tags.html'
version 2015-04-13"
  (interactive
   (let (ξp1 ξp2)
     (save-excursion
       (if (re-search-backward "\n[ \t]*\n" nil "move")
           (progn (re-search-forward "\n[ \t]*\n")
                  (setq ξp1 (point)))
         (setq ξp1 (point)))
       (if (re-search-forward "\n[ \t]*\n" nil "move")
           (progn (re-search-backward "\n[ \t]*\n")
                  (setq ξp2 (point)))
         (setq ξp2 (point))))
     (list ξp1 ξp2)))

  (let ((ξchangedItems '())
        (case-fold-search nil))
    (save-restriction
      (narrow-to-region φbegin φend)

      (goto-char (point-min))
      (while (search-forward-regexp "《\\([^》]+?\\)》" nil t)
        (push (match-string-no-properties 1) ξchangedItems)
        (replace-match "<cite class=\"book\">\\1</cite>" "FIXEDCASE"))

      (goto-char (point-min))
      (while (search-forward-regexp "〈\\([^〉]+?\\)〉" nil t)
        (push (match-string-no-properties 1) ξchangedItems)
        (replace-match "<cite>\\1</cite>" t)))

    (if (> (length ξchangedItems) 0)
        (mapcar
         (lambda (ξx)
           (princ ξx)
           (terpri))
         (reverse ξchangedItems))
      (message "No change needed."))))

(defun xah-remove-square-brackets (φbegin φend)
  "Delete any text of the form “[‹n›]”, ⁖ [1], [2], … in current text block or selection.

For example
 「… announced as Blu-ray Disc [11][12], and …」
becomes
 「… announced as Blu-ray Disc, and …」.

When called non-interactively, φbegin φend are region positions.

URL `http://ergoemacs.org/emacs/elisp_replace_title_tags.html'
Version 2015-06-04"
  (interactive
   (let (ξp1 ξp2)
     (save-excursion
       (if (re-search-backward "\n[ \t]*\n" nil "move")
           (progn (re-search-forward "\n[ \t]*\n")
                  (setq ξp1 (point)))
         (setq ξp1 (point)))
       (if (re-search-forward "\n[ \t]*\n" nil "move")
           (progn (re-search-backward "\n[ \t]*\n")
                  (setq ξp2 (point)))
         (setq ξp2 (point))))
     (list ξp1 ξp2)))
  (let (ξchangedItems)
    (save-restriction
      (narrow-to-region φbegin φend)
      (goto-char 1)
      (while (search-forward-regexp "\\(\\[[0-9]+?\\]\\)" nil t)
        (setq ξchangedItems (cons (match-string 1) ξchangedItems ))
        (replace-match "" t))

      (goto-char 1)
      (while (search-forward "[citation needed]" nil t)
        (setq ξchangedItems (cons "[citation needed]" ξchangedItems ))
        (backward-char 17)
        (delete-char 17)))

    (if (> (length ξchangedItems) 0)
        (mapcar
         (lambda (ξx)
           (princ ξx)
           (terpri))
         (reverse ξchangedItems))
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

