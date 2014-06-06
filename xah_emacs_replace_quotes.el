;; -*- coding: utf-8 -*-
;; 2010-09-03

;; Replace “…” to one of 〔…〕, 「…」, 【…】 or HTML tag. Or other similar text processing.

;; (defun xah-bracket→html ()
;;   "Replace all 「…」 to <code>…</code> in current buffer."
;;   (interactive)
;;   (let (changedItems)
;;     ;; (setq changedItems (make-hash-table :test 'equal))
;;     (setq changedItems '())

;;     (save-excursion
;;       (goto-char (point-min))
;;       (while (search-forward-regexp "《\\([^》]+?\\)》" nil t)
;;         ;; (puthash (match-string 1) "t" changedItems)
;;         (setq changedItems (cons (match-string 1) changedItems ) )
;;         (replace-match "<span class=\"bktl\">\\1</span>" t)
;;         )

;;       (goto-char (point-min))
;;       (while (search-forward-regexp "〈\\([^〉]+?\\)〉" nil t)
;;         ;; (puthash (match-string 1) "t" changedItems)
;;         (setq changedItems (cons (match-string 1) changedItems ) )
;;         (replace-match "<span class=\"atlt\">\\1</span>" t)
;;         )
;;       )

;;     (with-output-to-temp-buffer "*changed items*"
;;       ;; (maphash
;;       ;;  (lambda (myTitle myKey)
;;       ;;    (princ myTitle)
;;       ;;    (princ "\n")
;;       ;;    )
;;       ;;  changedItems)

;;       (mapcar
;;        (lambda (myTitle)
;;          (princ myTitle)
;;          (princ "\n")
;;          )
;;        changedItems)
;;       )
;;     ))

;; incomplete
;; (defun camel-case-to-understore-interactive (p1 p2)
;;   "query replace camelCase to camel_case words in current text block.
;; When called with `universal-argument', work on visible portion of whole buffer (i.e. respect `narrow-to-region'). When call in lisp program, the p1 p2 are region positions."
;;   (interactive
;;    (cond
;;     ((equal current-prefix-arg nil)    ; universal-argument not called
;;      (let ((bds (get-selection-or-unit 'block))) (list (elt bds 1) (elt bds 2) ) ))
;;     (t                                  ; all other cases
;;      (list (point-min) (point-max) )) ) )
;; (let ((case-fold-search nil))
;;  (save-excursion
;;     (save-restriction
;;       (narrow-to-region p1 p2)
;;       (goto-char (point-min))
;;       (while (search-forward-regexp "\\b\\([A-Z][a-z]+\\)\\b" nil t)
;;         (if (y-or-n-p "Replace this one?")
;;             (replace-match "\\1" t) ) ) ))
;; )
;;    )

(defun xah-corner-bracket→html-i (p1 p2)
  "Replace all 「…」 to <code>…</code> in current text block.
When called with `universal-argument', work on visible portion of whole buffer (i.e. respect `narrow-to-region'). When call in lisp program, the p1 p2 are region positions."
  (interactive
   (cond
    ((equal current-prefix-arg nil)    ; universal-argument not called
     (let ((bds (get-selection-or-unit 'block))) (list (elt bds 1) (elt bds 2) ) ))
    (t                                  ; all other cases
     (list (point-min) (point-max) )) ) )
  (save-excursion
    (save-restriction
      (narrow-to-region p1 p2)
      (goto-char (point-min))
      (while (search-forward-regexp "「\\([^」]+?\\)」" nil t)
        (if (y-or-n-p "Replace this one?")
            (replace-match "<code>\\1</code>" t) ) ) )) )

(defun xah-bracket→html (p1 p2)
  "Replace all 「…」 to <code>…</code> and others.

• 「…」 → <code>…</code>
• 〔…〕 → <code class=\"path-α\">\\1</code>
•  ‹…› → <var class=\"d\">…</var>
• 〔<…〕 → 〔☛ <…〕

Work on text selection or current text block.

When called in lisp program, the arguments p1 p2 are region positions.

Generate a report of the replaced strings in a separate buffer."
  (interactive (let ((bds (get-selection-or-unit 'block))) (list (elt bds 1) (elt bds 2) ) ) )
  (let (changedItems)
    (setq changedItems '())

    (save-excursion
      (save-restriction
        (narrow-to-region p1 p2)

        (goto-char (point-min))
        (while (search-forward-regexp "「\\([^」]+?\\)」" nil t)
          (setq changedItems (cons (match-string 1) changedItems ) )
          (replace-match "<code>\\1</code>" t) )

        (goto-char (point-min))
        (while (search-forward-regexp "‹\\([^›]+?\\)›" nil t)
          (setq changedItems (cons (match-string 1) changedItems ) )
          (replace-match "<var class=\"d\">\\1</var>" t) )

        (goto-char (point-min))
        (while (search-forward-regexp "〔<a href=" nil t)
          (setq changedItems (cons (match-string 1) changedItems ) )
          (replace-match "〔☛ <a href=" t) )

        (goto-char (point-min))
        (while (search-forward-regexp "〔\\([-_/\\:~.A-Za-z0-9]+?\\)〕" nil t)
          (setq changedItems (cons (match-string 1) changedItems ) )
          (replace-match "<code class=\"path-α\">\\1</code>" t) )
        ) )
    
    (with-output-to-temp-buffer "*changed brackets*"
      (mapcar
       (lambda (innerText)
         (princ innerText)
         (princ "\n") )
       (reverse changedItems) ) ) ))

(defun xah-title-bracket→html (p1 p2)
  "Replace all 〈…〉 to <cite>…</cite>.
Also replace 《…》 to <cite class=\"book\">…</span>.

If there's no text selection, work on current text block, else, on text selection.

When call in lisp program, the arguments p1 p2 are region positions.

Generate a report of the replaced strings in a separate buffer."
  (interactive (let ((bds (get-selection-or-unit 'block))) (list (elt bds 1) (elt bds 2) ) ) )
  (let (changedItems)

    ;; (setq changedItems (make-hash-table :test 'equal))
    (setq changedItems '())

    (save-excursion
      (save-restriction
        (narrow-to-region p1 p2)
        (goto-char (point-min))
        (while (search-forward-regexp "《\\([^》]+?\\)》" nil t)
          ;; (puthash (match-string 1) "t" changedItems)
          (setq changedItems (cons (match-string 1) changedItems ) )
          ;;       (setq case-fold-search nil)
          (replace-match "<cite class=\"book\">\\1</cite>" t)
          )

        (goto-char (point-min))
        (while (search-forward-regexp "〈\\([^〉]+?\\)〉" nil t)
          ;; (puthash (match-string 1) "t" changedItems)
          (setq changedItems (cons (match-string 1) changedItems ) )
          (replace-match "<cite>\\1</cite>" t)
          )
        ))

    (with-output-to-temp-buffer "*changed items*"
      ;; (maphash
      ;;  (lambda (myTitle myKey)
      ;;    (princ myTitle)
      ;;    (princ "\n")
      ;;    )
      ;;  changedItems)

      (mapcar
       (lambda (myTitle)
         (princ myTitle)
         (princ "\n")
         )
       changedItems)
      )
    ))


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
;;        (string-match "^Ctrl" (match-string 1 ) )
;;        (string-match "^Alt" (match-string 1 ) )
;;        (string-match "^Win" (match-string 1 ) )
;;        (string-match "^Menu" (match-string 1 ) )
;;        (string-match "^Meta" (match-string 1 ) )
;;        (string-match "^Cmd" (match-string 1 ) )
;;        (string-match "^Opt" (match-string 1 ) )
;;        (string-match "^Super" (match-string 1 ) )
;;        (string-match "^Hyper" (match-string 1 ) )
;;        )
;;       (setq replacePattern "【\1】" )
;;       )
;;      (CONDITION BODY)
;;      )

;;     )
;;   )

