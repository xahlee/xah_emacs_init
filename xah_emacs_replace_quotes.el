;; -*- coding: utf-8 -*-
;; 2010-09-03

;; Replace “…” to one of 〔…〕, 「…」, 【…】 or html tag. Or other similar text processing.

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
  "Replace all 「…」 to <code>…</code>.

If there's no text selection, work on current text block, else, on text selection. When called with `universal-argument', work on visible portion of whole buffer (i.e. respect `narrow-to-region'). When call in lisp program, the arguments p1 p2 are region positions.

Generate a report of the replaced strings in a separate buffer."
(interactive
   (cond
    ((equal current-prefix-arg nil)    ; universal-argument not called
     (let ((bds (get-selection-or-unit 'block))) (list (elt bds 1) (elt bds 2) ) ))
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
          (replace-match "<code>\\1</code>" t) ) ) )

    (with-output-to-temp-buffer "*changed brackets*"
      (mapcar
       (lambda (innerText)
         (princ innerText)
         (princ "\n")
         )
       changedItems)
      )
    ))

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

(defun curly-quotes-to-emacs-function-tag-old-2013-01-05 (p1 p2)
  "Replace curly quoted “elisp function” names to HTML markup.

For example, the text:
 <p>Call “sort-lines” to sort.</p>
becomes
 <p>Call <var class=\"εf\">sort-lines</var> to sort.</p>

If there's no text selection, work on current text block, else, on text selection. When called with `universal-argument', work on visible portion of whole buffer (i.e. respect `narrow-to-region'). When call in lisp program, the arguments p1 p2 are region positions.

Note: a word is changed only if all of the following are true:

① It is enclosed in <p> tag, or <ul>, <ol>, <table>, <figcaption>. (For example, not inside <h1> or <title>, <a>, or other tags.)
② The function name is tightly enclosed in double curly quotes, e.g. “sort-lines” but not “use sort-lines”.
③ `fboundp' returns true.

This command assumes that all tags are closed in your HTML. e.g. <p> must be closed with </p>.

This command also makes a report of changed items.

Some issues:

• If the lisp functions name is less or equal to 2 chars, it won't be tagged. e.g. {「*」,「+」,「1+」, …}.

• Only words contaning lowercase a to z, 0-9, or hyphen, are checked, even though elisp identifier allows many other chars. e.g. “yas/reload-all”, “Info-copy-current-node-name” (note capital letter).

• Some words are common in other lang, e.g. “while”, “print”, “string”, unix “find”, “grep”, HTML's “kbd” tag, etc. But they are also built-in elisp symbols. This command will tag them, but you may not want to tag them.

• Personal emacs functions will also be tagged. You may not want them to be because they are not standard functions.

• Some functions are from 3rd party libs, and some are not bundled with GNU emacs , e.g. 「'cl」, 「'htmlize」. They may or may not be tagged depending whether they've been loaded."
  ;; (interactive (let ((bds (get-selection-or-unit 'block))) (list (elt bds 1) (elt bds 2) ) ) )
  (interactive
   (cond
    ((equal current-prefix-arg nil)    ; universal-argument not called
     (let ((bds (get-selection-or-unit 'block))) (list (elt bds 1) (elt bds 2) ) ))
    (t                                  ; all other cases
     (list (point-min) (point-max) )) ) )

  (require 'sgml-mode) ; from html-mode, needs sgml-skip-tag-forward
  (let (p3 p4 mStr (ξi 0) (case-fold-search nil) )
    (save-excursion
      (save-restriction
        (narrow-to-region p1 p2)
        (goto-char (point-min))
        (while (search-forward-regexp "<p>\\|<ul>\\|<ol>\\|<table\\|<figcaption>" nil t)
          (backward-char)
          (setq p3 (point) )
          (sgml-skip-tag-forward 1)
          (setq p4 (point) )

          (save-restriction
            (narrow-to-region p3 p4)
            (goto-char (point-min))
            (while (search-forward-regexp "“\\([-a-z0-9]+\\)”" (point-max) t)
              (setq mStr (match-string 1) )

              (when (and (fboundp (intern mStr))
                         (> (length mStr) 2))
                (replace-match (concat "<var class=\"εf\">" mStr "</var>") t t)
                (setq ξi (1+ ξi) )
                ) ) ) )
        (when (> ξi 0)
          (occur "<var class=\"εf\">[-a-z0-9]+</var>" )) ) ) ))

(defun curly-quotes-to-emacs-function-tag (p1 p2)
  "Replace curly quoted “elisp function” names to HTML markup.

For example, the text:
 Call “sort-lines” to sort.
becomes
 Call <var class=\"εf\">sort-lines</var> to sort.

Works on current text block or text selection.

When called with `universal-argument', work on whole buffer (but respect `narrow-to-region').

When call in lisp program, the arguments p1 p2 are region positions.

Note: a word is changed only if all of the following are true:

① The function name is tightly enclosed in double curly quotes, e.g. “sort-lines” but not “use sort-lines”.
① `fboundp' on the function name returns true.

This command also makes a report of changed items.

Some issues:

• If the lisp functions name is less or equal to 2 chars, it won't be tagged. e.g. { * + 1+ eq}

• Only words contaning lowercase a to z, 0-9, or hyphen, are checked, even though elisp identifier allows many other chars. e.g. “yas/reload-all”, “Info-copy-current-node-name” (note capital letter).

• Some words are common in other lang, e.g. “while”, “print”, “string”, unix “find”, “grep”, HTML's “kbd” tag, etc. But they are also built-in elisp symbols. This command will tag them, but you may not want that.

• Personal emacs functions will also be tagged. You may not want them to be because they are not standard functions.

• Some functions are from 3rd party libs, and some are not bundled with GNU emacs , e.g. 「'cl」, 「'htmlize」. They may or may not be tagged depending whether they've been loaded."
  ;; (interactive (let ((bds (get-selection-or-unit 'block))) (list (elt bds 1) (elt bds 2) ) ) )
  (interactive
   (cond
    ((equal current-prefix-arg nil)    ; universal-argument not called
     (let ((bds (get-selection-or-unit 'block))) (list (elt bds 1) (elt bds 2) ) ))
    (t                                  ; all other cases
     (list (point-min) (point-max) )) ) )
  (let ( inputStr resultStr )
    (setq inputStr (buffer-substring-no-properties p1 p2) )
    (setq resultStr (curly-quotes-to-emacs-function-string inputStr))
    (when  (not (string= inputStr resultStr))
      (progn
        (save-excursion
          (save-restriction
            (narrow-to-region p1 p2)
            (delete-region p1 p2)
            (insert resultStr)
            (occur "<var class=\"εf\">[-a-z0-9]+</var>" )
            )
          )) ) ))

(defun curly-quotes-to-emacs-function-string (ξsomeStr)
  "Replace curly quoted “elisp function” names to HTML markup."
  (let ( mStr (case-fold-search nil) )
    (with-temp-buffer
      (insert ξsomeStr)
      (goto-char 1)
      (while (search-forward-regexp "“\\([-a-z0-9]+\\)”" (point-max) t)
        (setq mStr (match-string 1) )
        (when (and (fboundp (intern mStr))
                   (> (length mStr) 2))
          (replace-match (concat "<var class=\"εf\">" mStr "</var>") t t)
          ) )
      (buffer-string)
      ) ))


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
