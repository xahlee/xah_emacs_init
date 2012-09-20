;; -*- coding: utf-8 -*-
;; part of Xah Lee's emacs init file.
;; 2007-06, 2011-01-24
;; Xah Lee,
;; ∑ http://xahlee.org/

;; some functions personal to working on XahLee.org's website
;; many of these opens a particular file and insert a string


;; § ----------------------------------------
;; xwe = xah wordy english
;; 〈Wordy English — the Making of Belles-Lettres〉
;; http://xahlee.org/PageTwo_dir/Vocabulary_dir/vocabulary.html

(defun xwe-new-word-entry ()
  "Insert a blank a-word-a-day html template in a paritcular file."
  (interactive)

  (find-file (concat (xahsite-server-root-path) "wordyenglish_com/words/new.html") )
  (goto-char 1)
  (re-search-forward
"<!-- c:/Users/h3/web/wordyenglish_com/lit/blog.html -->"
nil t)
(insert "\n\n")
  (xwe-insert-word-entry)
)

(defun xwe-insert-word-entry ()
  "Insert a blank a-word-a-day html template."
  (interactive)
  (insert "<div class=\"δdate\"><time>" (format-time-string "%Y-%m-%d") "</time></div>\n")
  (insert
   "<div class=\"ent\">
<p class=\"wd\"></p>
<div class=\"ex\">
<div class=\"bdy\"></div>
<div class=\"src\"></div>
</div>\n</div>\n\n")
  (re-search-backward "class=\"bdy\">" nil t)
  (forward-char 12)
  (yank)
)

(defun xwe-add-definition ()
  "Insert a word definition entry template.
Using current word or current text selection."
  (interactive)
  (let (bds p1 p2 ξstr str2)
    (setq bds (get-selection-or-unit 'word))
    (setq ξstr (elt bds 0) )

    (setq str2 (asciify-text ξstr) )
    (search-forward "\n\n" nil t)
    (search-backward "</div>")
    (insert "<div class=\"def\"></div>\n")
    (search-backward "</div>")
    (insert ξstr)
    (insert " = ")

    (when (fboundp 'lookup-word-definition)
        ;; (require 'lookup-word-on-internet)
        (lookup-word-dict-org ξstr)
      (lookup-word-definition ξstr)
      )

    ;; (dictionary-new-search (cons ξstr dictionary-default-dictionary))
    ))

(defun xwe-add-source ()
  "Insert a word definition entry template.
Using current word or current text selection."
  (interactive)
  (let (bds p1 p2)
    (require 'sgml-mode) ; for sgml-skip-tag-forward
    (search-backward "<div class=\"ent\">")
    (search-forward "<div class=\"bdy\">")
    (backward-char 1)
    (sgml-skip-tag-forward 1)
    (insert "\n<div class=\"src\"></div>")
    (backward-char 6)
    ))

(defun xwe-add-comment ()
  "Insert a comment entry in wordy-english."
  (interactive)
  (let ()
    (search-backward "\n\n")
    (search-forward "<div class=\"src\">" nil t)
    (search-forward "</div>" nil t)
    (insert "\n"
"<div class=\"cmt\">"
"</div>"
)
    (backward-char 6)
    ))

(defun xwe-search-next-unbold ()
  "search the next word block that isn't bolded.
Used for the files in
FILE `~/web/PageTwo_dir/Vocabulary_dir/'."
  (interactive)

  (let ( wd egText p1 p2 p3 p4 notBolded-p)
    ;; grab the word
    (search-forward "<p class=\"wd\">")
    (setq p1 (point))
    (search-forward "</p>")
    (backward-char 4)
    (setq p2 (point))
    (setq wd (buffer-substring-no-properties p1 p2))

    ;; grab the example text
    (search-forward "<div class=\"bdy\">")
    (setq p3 (point))
    (search-forward "</div>")
    (backward-char 6)
    (setq p4 (point))
    (setq egText (buffer-substring-no-properties p3 p4))

    ;; check if word is bolded in example text
    (setq notBolded-p (not (string-match (concat ">" wd) egText)) )
;;     (setq notBolded-p (string-match ">harrowing" egText) )

    (if notBolded-p
        (progn (goto-char p3)
               (search-forward wd p4))
      (xwe-search-next-unbold)
      ;; (when (y-or-n-p "Do you want to bold it?")
      ;;         (goto-char p3)
      ;;         (search-forward wd p4)
      ;;         (search-backward wd p3)
      ;;         (insert "<span class=\"w\">")
      ;;         (search-forward wd p4)
      ;;         (insert "</span>")
      ;;         )
      )))

