;; -*- coding: utf-8; lexical-binding: t; -*-
;; part of Xah Lee's emacs init file.
;; 2007-06, 2011-01-24
;; Xah Lee,
;; ∑ http://xahlee.org/

;; some functions personal to working on XahLee.org's website
;; many of these opens a particular file and insert a string

;; HHH___________________________________________________________________
;; xwe = xah wordy english
;; 〈Wordy English — the Making of Belles-Lettres〉
;; http://wordyenglish.com/words/vocabulary.html

(defun xah-words-chinese-linkify ()
  "Make the Chinese character before cursor into Chinese dictionary reference links.

URL `http://ergoemacs.org/emacs/elisp_chinese_char_linkify.html'
Version 2020-11-24 2021-02-11"
  (interactive)
  (let (
        ($template
         "<div class=\"chineseXL\"><span lang=\"zh\">▮</span> <span class=\"en\"><a rel=\"noopener\" target=\"_blank\" href=\"https://translate.google.com/#zh-CN|en|▮\">Translate</a> • <a rel=\"noopener\" target=\"_blank\" href=\"https://en.wiktionary.org/wiki/▮\">Wiktionary</a></span></div>"
         )
        ($char (buffer-substring-no-properties (- (point) 1) (point))))
    (delete-char -1)
    (insert (replace-regexp-in-string "▮" $char $template))))

(defun xah-words-bold-word ()
  "wrap b tag with class w.
personal to xahlee.org's vocabulary pages.
Version 2015-03-11"
  (interactive)
  (progn
    (xah-html-wrap-html-tag "b" "w")))

(defun xah-words-move-word-to-page (@category)
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
        $p1
        $p2
        $wordText
        ($destFile (concat @category ".html")))
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (save-excursion
        (if (re-search-backward "\n[ \t]*\n" nil "move")
            (progn (re-search-forward "\n[ \t]*\n")
                   (setq $p1 (point)))
          (setq $p1 (point)))
        (if (re-search-forward "\n[ \t]*\n" nil "move")
            (progn (re-search-backward "\n[ \t]*\n")
                   (setq $p2 (point)))
          (setq $p2 (point)))))

    (setq $wordText (buffer-substring-no-properties $p1 $p2))
    (delete-region $p1 $p2 )

    (find-file (concat (xahsite-server-root-path) "wordyenglish_com/words/" $destFile))
    (goto-char 1)
    (search-forward "<section class=\"word88\">") (search-backward "<")
    (insert $wordText "\n\n")
    (save-buffer )
    (kill-buffer )
    (message "Word moved to 「%s」" $destFile)

    (let*
        ;; save the working buffer, but make backup first
        (($fname (buffer-file-name))
         ($backupName (concat $fname "~" (format-time-string "%Y%m%d_%H%M%S") "~")))
      (copy-file $fname $backupName t)
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
  "Insert a blank a-word-a-day HTML template.
Version 2017-03-08"
  (interactive)
  (insert
   "<section class=\"word88\">
<h3 class=\"wd\"></h3>
<div class=\"ex\">
<div class=\"bdy\"></div>
<div class=\"src\"></div>
</div>\n</section>\n\n")
  (re-search-backward "class=\"bdy\">" nil t)
  (forward-char 12)
  (yank))

(defun xah-words-add-definition ()
  "Insert a word definition entry template.
Using current word or selection."
  (interactive)
  (let (($str
         (if (use-region-p)
             (buffer-substring-no-properties (region-beginning) (region-end))
           (current-word))))
    (search-forward "\n\n" nil t)
    (search-backward "</div>")
    (insert "<div class=\"def\"></div>\n")
    (search-backward "</div>")
    (insert $str " = ")))

(defun xah-words-add-source ()
  "Insert a word definition entry template."
  (interactive)
  (progn
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

  (let (wd egText $p1 $p2 p3 p4 notBolded-p)
    ;; grab the word
    (search-forward "<h3 class=\"wd\">")
    (setq $p1 (point))
    (search-forward "</p>")
    (backward-char 4)
    (setq $p2 (point))
    (setq wd (buffer-substring-no-properties $p1 $p2))

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
  (let ( ($inputText
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (current-word))))    
    (xah-html-wrap-html-tag "span" "xnt")
    (search-backward "<p")
    (insert "<div class=\"xnote\"></div>\n\n")
    (search-backward "</div>")
    (insert (format "<b class=\"x3nt\">%s</b> " $inputText))))

(defun xah-words-word-etymology-linkify ()
  "Make the current word into a etymology reference link.
Version 2020-06-16 2021-02-11"
  (interactive)
  (let ($p1 $p2 $word
            ($url "https://www.etymonline.com/word/"))
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (setq $p1 (line-beginning-position) $p2 (line-end-position)))
    (setq $word (buffer-substring-no-properties $p1 $p2))
    (delete-region $p1 $p2)
    (insert (format "[etymology of %s <a rel=\"noopener\" target=\"_blank\" href=\"%s%s\">%s%s</a>]" $word $url $word $url $word))))

(defun xah-words-query-find-then-bold ()
  "personal to xahlee.org's vocabulary pages.
Search forward a word enclosed by “<h3 class=\"wd\">” and “</h3>”,
then search forward it inside the example body, only if it is not
already bold. Then, ask user whether that should be bold.
adding date 2017-06-10"
  (interactive)
  (progn
    (goto-char (point-min))
    (while (re-search-forward "<h3 class=\"wd\">\\([^\<]+\\)</h3>" nil t)
      (re-search-forward (match-string 1))
      (when (y-or-n-p "Do you want to bold the word?")
        (xah-html-wrap-html-tag "span" "w")
        ;;(replace-match "<span class=\"x-w\">\\1</span>" t)
        ))))
