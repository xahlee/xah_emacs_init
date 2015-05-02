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

(defun xah-words-chinese-linkify ()
  "Make the Chinese character before cursor into Chinese dictionary reference links.
URL `http://ergoemacs.org/emacs/elisp_chinese_char_linkify.html'
Version 2015-05-01"
  (interactive)
  (let ( 
        (ξtemplate
         "<div class=\"chinese-etymology-96656\"><b class=\"w\">�</b> <span class=\"en\"><a href=\"http://translate.google.com/#zh-CN|en|�\">Translate</a> ◇ <a href=\"http://en.wiktionary.org/wiki/�\">Wiktionary</a> ◇ <a href=\"http://www.chineseetymology.org/CharacterEtymology.aspx?submitButton1=Etymology&amp;characterInput=�\">history</a></span></div>"
         )
        (ξchar (buffer-substring-no-properties (- (point) 1) (point))))
    (delete-char -1)
    (insert (replace-regexp-in-string "�" ξchar ξtemplate))))

(defun xah-words-bold-word ()
  "wrap b tag with class w.
personal to xahlee.org's vocabulary pages.
Version 2015-03-11"
  (interactive)
  (progn
    (xah-html-wrap-html-tag "b" "w")))

(defun xah-words-move-word-to-page (φcategory)
  "Take current selection or block of text, ask which page to move it to."
  (interactive
   (list (ido-completing-read "Which:" '("specialwords"
                                         "arcane"
                                         "combowords"
                                         "easy"
                                         "foreignwords"
                                         "gre"
                                         "hyphwords"
                                         "informal"
                                         "slang"
                                         "noun"
                                         "noun_things"
                                         "noun_abs"
                                         "poesy"
                                         "satwords"
                                         "writerwords"))))
  (let (
        ξp1
        ξp2
        ξwordText
        (ξdestFile (concat φcategory ".html")))
    (if (use-region-p)
        (progn
          (setq ξp1 (region-beginning))
          (setq ξp2 (region-end)))
      (save-excursion
        (if (re-search-backward "\n[ \t]*\n" nil "move")
            (progn (re-search-forward "\n[ \t]*\n")
                   (setq ξp1 (point)))
          (setq ξp1 (point)))
        (if (re-search-forward "\n[ \t]*\n" nil "move")
            (progn (re-search-backward "\n[ \t]*\n")
                   (setq ξp2 (point)))
          (setq ξp2 (point)))))

    (setq ξwordText (buffer-substring-no-properties ξp1 ξp2))
    (delete-region ξp1 ξp2 )

    (find-file (concat (xahsite-server-root-path) "wordyenglish_com/words/" ξdestFile))
    (goto-char 1)
    (search-forward "<section class=\"word88\">") (search-backward "<")
    (insert ξwordText "\n\n")
    (save-buffer )
    (kill-buffer )
    (message "Word moved to 「%s」" ξdestFile)

    (let*
        ;; save the working buffer, but make backup first
        ((ξfname (buffer-file-name))
         (ξbackupName (concat ξfname "~" (format-time-string "%Y%m%d_%H%M%S") "~")))
      (copy-file ξfname ξbackupName t)
      (save-buffer ))))

(defun xah-words-new-word-entry ()
  "Insert a blank a-word-a-day HTML template in a paritcular file."
  (interactive)

  (find-file (concat (xahsite-server-root-path) "wordyenglish_com/words/new.html"))
  (goto-char 1)
  (search-forward "<section class=\"word88\">") (search-backward "<")
  (insert "\n\n")
  (xah-words-insert-word-entry))

(defun xah-words-insert-word-entry ()
  "Insert a blank a-word-a-day HTML template."
  (interactive)
  (insert
   "<section class=\"word88\">
<p class=\"wd\"></p>
<div class=\"ex\">
<div class=\"bdy\"></div>
<div class=\"src\"></div>
</div>\n</section>\n\n")
  (re-search-backward "class=\"bdy\">" nil t)
  (forward-char 12)
  (yank))

(defun xah-words-add-definition ()
  "Insert a word definition entry template.
Using current word or text selection."
  (interactive)
  (let* ((ξbds (get-selection-or-unit 'word))
         (ξstr1 (aref ξbds 0))
         ξstr2)

    (setq ξstr2 (xah-asciify-string ξstr1))
    (search-forward "\n\n" nil t)
    (search-backward "</div>")
    (insert "<div class=\"def\"></div>\n")
    (search-backward "</div>")
    (insert ξstr " = ")))

(defun xah-words-add-source ()
  "Insert a word definition entry template.
Using current word or text selection."
  (interactive)
  (let ()
    (require 'sgml-mode) ; for sgml-skip-tag-forward
    (search-backward "<section class=\"word88\">")
    (search-forward "<div class=\"bdy\">")
    (backward-char 1)
    (sgml-skip-tag-forward 1)
    (insert "\n<div class=\"src\"></div>")
    (backward-char 6)))

(defun xah-words-add-comment ()
  "Insert a comment entry in wordy-english."
  (interactive)
  (progn
    (search-backward "\n\n")
    (search-forward "<div class=\"src\">" nil t)
    (search-forward "</div>" nil t)
    (insert "\n"
            "<div class=\"cmt\">"
            "</div>"
            )
    (backward-char 6)))

(defun xah-words-search-next-unbold ()
  "search the next word block that isn't bolded.
Used for the files in
FILE `~/web/PageTwo_dir/Vocabulary_dir/'."
  (interactive)

  (let (wd egText ξp1 ξp2 p3 p4 notBolded-p)
    ;; grab the word
    (search-forward "<p class=\"wd\">")
    (setq ξp1 (point))
    (search-forward "</p>")
    (backward-char 4)
    (setq ξp2 (point))
    (setq wd (buffer-substring-no-properties ξp1 ξp2))

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
      (xah-words-search-next-unbold)
      ;; (when (y-or-n-p "Do you want to bold it?")
      ;;         (goto-char p3)
      ;;         (search-forward wd p4)
      ;;         (search-backward wd p3)
      ;;         (insert "<span class=\"w\">")
      ;;         (search-forward wd p4)
      ;;         (insert "</span>")
      ;;         )
      )))

(defun xah-words-annotate ()
  "Create a annotation in HTML.
Wrap HTML “span” tag around current word or text selection, then
insert a div tag above the current paragraph."
  (interactive)
  (let (ξbds ξinputText)
    (setq ξbds (get-selection-or-unit 'word))
    (setq ξinputText (aref ξbds 0))
    (xah-html-wrap-html-tag "span" "xnt")
    (search-backward "<p")
    (insert "<div class=\"xnote\"></div>\n\n")
    (search-backward "</div>")
    (insert (format "<b class=\"x3nt\">%s</b> " ξinputText))))

(defun xah-words-word-etymology-linkify ()
  "Make the current word into a etymology reference link."
  (interactive)
  (let (ξp1 ξp2 ξinput ξresult)
    (if (use-region-p)
        (progn (setq ξp1 (region-beginning))
               (setq ξp2 (region-end)))
      (progn (setq ξp1 (line-beginning-position))
             (setq ξp2 (line-end-position))))
    (setq ξinput (buffer-substring-no-properties ξp1 ξp2))
    (setq ξresult (concat "<span class=\"english-etymology-35252\"><a href=\"http://www.etymonline.com/index.php?search=" ξinput "\">" ξinput "</a></span>"))
    (delete-region ξp1 ξp2)
    (insert ξresult)))

(defun xah-words-query-find-then-bold ()
  "personal to xahlee.org's vocabulary pages.
Search forward a word enclosed by “<p class=\"wd\">” and “</p>”,
then search forward it inside the example body, only if it is not
already bold. Then, ask user whether that should be bold."
  (interactive)
  (progn
    (goto-char (point-min))
    (while (search-forward-regexp "<p class=\"wd\">\\([^\<]+\\)</p>" nil t)
      (search-forward-regexp (match-string 1))
      (when (y-or-n-p "Do you want to bold the word?")
        (xah-html-wrap-html-tag "span" "w")
        ;;(replace-match "<span class=\"x-w\">\\1</span>" t)
        ))))

(defun xah-words-find-word-usage (φword)
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
