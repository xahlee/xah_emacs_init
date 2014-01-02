;; -*- coding: utf-8 -*-
;; 2010-09-03

;; Replace “…” to one of 〔…〕, 「…」, 【…】 or HTML tag. Or other similar text processing.

(defun cap-first-letter ()
  "Replace “<p>a…” to cap “<p>A…”
TODO doesn't work.
"
  (interactive)
  (let ((case-fold-search nil))
    (query-replace-regexp "<p>\\([a-z]\\)" (concat "<p>" (upcase (match-string 1 )))
; "<p>\\,(upcase \\1)"
;; "<p>\\,(upcase \\1)"
)
 ))

;; (defun code-bracket-to-html-tag ()
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

(defun code-bracket-to-html-tag-interactive (p1 p2)
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

(defun code-bracket-to-html-tag (p1 p2)
  "Replace all 「…」 to <code>…</code> and others.

• 「…」 → <code>…</code>
• 〔…〕 → <code class=\"path-α\">\\1</code>
•  ‹…› → <var class=\"d\">…</var>
• 〔<…〕 → 〔☛ <…〕

Work on text selection or current text block.

When called with `universal-argument', work on whole buffer (but respect `narrow-to-region').

When called in lisp program, the arguments p1 p2 are region positions.

Generate a report of the replaced strings in a separate buffer."
  (interactive
   (cond
    ((equal current-prefix-arg nil)    ; universal-argument not called
     (let ((bds (get-selection-or-unit 'block))) 
       (message "prifx arg %s" current-prefix-arg)
       (list (elt bds 1) (elt bds 2) ) ))
    (t                                  ; all other cases
     (list (point-min) (point-max) )) ) )
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
        (while (search-forward-regexp "〔\\([-_/\\:~.A-Za-z]+?\\)〕" nil t)
          (setq changedItems (cons (match-string 1) changedItems ) )
          (replace-match "<code class=\"path-α\">\\1</code>" t) )

        ) )
    
    (with-output-to-temp-buffer "*changed brackets*"
      (mapcar
       (lambda (innerText)
         (princ innerText)
         (princ "\n") )
       (reverse changedItems) ) ) ))

(defun title-bracket-to-html-tag (p1 p2)
  "Replace all 〈…〉 to <cite>…</cite>.
Also replace 《…》 to <cite class=\"book\">…</span>.

If there's no text selection, work on current text block, else, on text selection. When called with `universal-argument', work on visible portion of whole buffer (i.e. respect `narrow-to-region'). When call in lisp program, the arguments p1 p2 are region positions.

Generate a report of the replaced strings in a separate buffer."
  (interactive
   (cond
    ((equal current-prefix-arg nil)    ; universal-argument not called
     (let ((bds (get-selection-or-unit 'block))) (list (elt bds 1) (elt bds 2) ) ))
    (t                                  ; all other cases
     (list (point-min) (point-max) )) ) )
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


(defun curly-quotes-to-bracket (leftBracket rightBracket)
  "Replace “…” to one of 「…」.
Which bracket is determined by the string LEFTBRACKET and RIGHTBRACKET."
  (interactive)
  (let ()
    (if (string= major-mode "dired-mode")
        (progn
          (dired-do-query-replace-regexp
           "“\\([^”]+?\\)”"
           (concat leftBracket "\\1" rightBracket)
           ))
      (progn (query-replace-regexp
              "“\\([^”]+?\\)”"
           (concat leftBracket "\\1" rightBracket) )) ) ))

(defun curly-quotes-to-code-bracket ()
  "Replace “…” to 「…」"
  (interactive)
  (curly-quotes-to-bracket "「" "」")
)

(defun curly-quotes-to-html-code-tag ()
  "Replace 「“…”」 to 「<code>…</code>」"
  (interactive)
  (curly-quotes-to-bracket "<code>" "</code>")
)

(defun curly-quotes-to-html-strong-tag ()
  "Replace 「“…”」 to 「<strong>…</strong>」"
  (interactive)
  (curly-quotes-to-bracket "<strong>" "</strong>")
)

(defun curly-quotes-to-elisp-function-bracket ()
  "Replace “…” to ｢…｣"
  (interactive)
  (curly-quotes-to-bracket "｢" "｣")
)

(defun curly-quotes-to-french-quote ()
  "Replace “…” to «…»"
  (interactive)
  (curly-quotes-to-bracket "«" "»")
)

(defun curly-quotes-to-kbd-tag ()
  "Replace “…” to <kbd>…</kbd>"
  (interactive)
  (curly-quotes-to-bracket "<kbd>" "</kbd>")
)

(defun curly-quotes-to-keyboard-bracket ()
  "Replace “…” to 【…】"
  (interactive)
  (curly-quotes-to-bracket "【" "】")
)

(defun curly-quotes-to-menu-bracket ()
  "Replace “…” to 〖…〗"
  (interactive)
  (curly-quotes-to-bracket "〖" "〗")
)

(defun curly-quotes-to-book-bracket ()
  "Replace “…” to 《…》"
  (interactive)
  (curly-quotes-to-bracket "《" "》")
)

(defun curly-quotes-to-title-bracket ()
  "Replace “…” to 〈…〉"
  (interactive)
  (curly-quotes-to-bracket "〈" "〉")
)

(defun curly-quotes-to-file-path ()
  "Replace “…” to 〔…〕"
  (interactive)
  (curly-quotes-to-bracket "〔" "〕")
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

