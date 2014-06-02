;; -*- coding: utf-8 -*-
;; part of Xah Lee's emacs init file.
;; 2007-06, 2011-01-24
;; Xah Lee,
;; ∑ http://xahlee.org/

;; some functions personal to working on XahLee.org's website
;; many of these opens a particular file and insert a string


;; xwe = xah wordy english
;; 〈Wordy English — the Making of Belles-Lettres〉
;; http://wordyenglish.com/words/vocabulary.html

(defun xwe-move-word-to-page (φmoveCode)
  "take current selection or block of text, ask which page to move it to."
  (interactive "sEnter a character: [s]sat [g]gre [w]writer [e]easy [a]arcane [l]slang [i]informal [h]hyphen [c]combo [n]noun [t]noun things [8]noun abstract [p]poesy [f]foreign [3]special:")
  (let (p1 p2 bds ξfile ξwordText)
    (setq bds (get-selection-or-unit 'block))
    (setq ξwordText (elt bds 0) p1 (elt bds 1) p2 (elt bds 2)  )

    (cond
     ((string= φmoveCode "3") (setq ξfile "specialwords.html" ))
     ((string= φmoveCode "a") (setq ξfile "arcane.html" ))
     ((string= φmoveCode "c") (setq ξfile "combowords.html" ))
     ((string= φmoveCode "e") (setq ξfile "easy.html" ))
     ((string= φmoveCode "f") (setq ξfile "foreignwords.html" ))
     ((string= φmoveCode "g") (setq ξfile "gre.html" ))
     ((string= φmoveCode "h") (setq ξfile "hyphwords.html" ))
     ((string= φmoveCode "i") (setq ξfile "informal.html" ))
     ((string= φmoveCode "l") (setq ξfile "slang.html" ))
     ((string= φmoveCode "n") (setq ξfile "noun.html" ))
     ((string= φmoveCode "t") (setq ξfile "noun_things.html" ))
     ((string= φmoveCode "8") (setq ξfile "noun_abs.html" ))
     ((string= φmoveCode "p") (setq ξfile "poesy.html" ))
     ((string= φmoveCode "s") (setq ξfile "satwords.html" ))
     ((string= φmoveCode "w") (setq ξfile "writerwords.html" ))

     (t (user-error "Your letter 「%s」 is not one of the allowed" φmoveCode ))
     )

    (delete-region p1 p2 )

    (find-file (concat (xahsite-server-root-path) "wordyenglish_com/words/" ξfile) )
    (goto-char 1)
    (search-forward "<section class=\"word-α\">") (search-backward "<")
    (insert ξwordText "\n\n")
    (save-buffer )
    (kill-buffer )
    (message "Word moved to 「%s」" ξfile)

    (let*
      ;; save the working buffer, but make backup first
          ((currentFileName (buffer-file-name))
           (backupFileName (concat currentFileName "~" (format-time-string "%Y%m%d_%H%M%S") "~")) )
        (copy-file currentFileName backupFileName t)
        (save-buffer ) ) ) )

(defun xwe-new-word-entry ()
  "Insert a blank a-word-a-day HTML template in a paritcular file."
  (interactive)

  (find-file (concat (xahsite-server-root-path) "wordyenglish_com/words/new.html") )
  (goto-char 1)
  (search-forward "<section class=\"word-α\">") (search-backward "<")
  (insert "\n\n")
  (xwe-insert-word-entry)
  )

(defun xwe-insert-word-entry ()
  "Insert a blank a-word-a-day HTML template."
  (interactive)
  (insert
   "<section class=\"word-α\">
<p class=\"wd\"></p>
<div class=\"ex\">
<div class=\"bdy\"></div>
<div class=\"src\"></div>
</div>\n</section>\n\n")
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
    (insert ξstr " = ")
    ))

(defun xwe-add-source ()
  "Insert a word definition entry template.
Using current word or current text selection."
  (interactive)
  (let (bds p1 p2)
    (require 'sgml-mode) ; for sgml-skip-tag-forward
    (search-backward "<section class=\"word-α\">")
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

(defun xwe-chinese-linkify ()
  "Make the current Chinese character into several Chinese dictionary links.
If there's a text selection, use that for input."
  (interactive)
  (let ( ξchar p1 p2 big5Code templateStr resultStr)

    (if (region-active-p)
        (progn
          (setq p1 (region-beginning) )
          (setq p2 (region-end) )
          )
      (progn
        (setq p1 (point) )
        (setq p2 (1+ (point)) ) ) )

    (setq ξchar (buffer-substring-no-properties p1 p2))

    ;; (setq big5Code (encode-char (string-to-char ξchar) 'big5) )

    (setq templateStr
          "<div class=\"cδ\"><b class=\"w\">�</b> <span class=\"en\"><a href=\"http://translate.google.com/#zh-CN|en|�\">Translate</a> ◇ <a href=\"http://en.wiktionary.org/wiki/�\">Wiktionary</a> ◇ <a href=\"http://www.chineseetymology.org/CharacterEtymology.aspx?submitButton1=Etymology&amp;characterInput=�\">history</a></span></div>"
          )

    (setq resultStr (replace-regexp-in-string "�" ξchar templateStr))
    (delete-region p1 p2)
    (insert resultStr) ))

(defun xwe-annotate ()
  "Create a annotation in HTML.
Wrap HTML “span” tag around current word or text selection, then
insert a div tag above the current paragraph."
  (interactive)
  (let (bds inputText)
    (setq bds (get-selection-or-unit 'word))
    (setq inputText (elt bds 0) )
    (xhm-wrap-html-tag "span" "xnt")
    (search-backward "<p")
    (insert "<div class=\"xnote\"></div>\n\n")
    (search-backward "</div>")
    (insert (format "<b class=\"x3nt\">%s</b> " inputText)  )
    )
  )

(defun xwe-word-etymology-linkify ()
  "Make the current word into a etymology reference link.
."
  (interactive)
  (let ( bds p1 p2 inputstr resultStr)

    (setq bds (get-selection-or-unit 'line))
    (setq inputstr (elt bds 0) p1 (elt bds 1) p2 (elt bds 2)  )
    (setq resultStr (concat "<span class=\"cδe\"><a href=\"http://www.etymonline.com/index.php?search=" inputstr "\">" inputstr "</a></span>") )
    (delete-region p1 p2)
    (insert resultStr) ))

(defun xwe-query-find-then-bold ()
  "personal to xahlee.org's vocabulary pages.
Search forward a word enclosed by “<p class=\"wd\">” and “</p>”,
then search forward it inside the example body, only if it is not
already bold. Then, ask user whether that should be bold."
  (interactive)
  (let ()
    (goto-char (point-min))
    (while (search-forward-regexp "<p class=\"wd\">\\([^\<]+\\)</p>" nil t)
      (search-forward-regexp (match-string 1))
      (when (y-or-n-p "Do you want to bold the word?")
        (xhm-wrap-html-tag "span" "w")
        ;;(replace-match "<span class=\"x-w\">\\1</span>" t)
        ))
    ))

(defun xwe-find-word-usage (φword)
  "Grep a dir for a word's usage."
  (interactive "sWord to search: ")
  (require 'grep)
  (grep-compute-defaults)
  (rgrep φword "*html" "~/web/p")
;; ~/web/p
;; ~/web/flatland/
;; ~/web/Periodic_dosage_dir/_p2/russell-lecture.html
;; ~/web/Periodic_dosage_dir/_p2/why_not_christian.html
)
