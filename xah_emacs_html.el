;; -*- coding: utf-8 -*-
;; some custome string functions for working with HTML
;; 2007-06
;;   Xah Lee
;; ∑ http://xahlee.org/

(defun forward-html-end-tag ()
  "Move cursor to the next html tag's content."
  (interactive)
  (forward-char 1)
  (search-forward "</")
  (backward-char 2)
  )

(defun backward-html-end-tag ()
  "Move cursor to the previous html tag's content."
  (interactive)
  (search-backward "</")
  ;; (forward-char-char 2)
  )


(defun compact-css-region (p1 p2)
  "Remove unnecessary whitespaces of CSS source code in region.
CSS is Cascading Style Sheet.
WARNING: not robust. Designed for my personal use only."
  (interactive "r")
  (let ()
    (save-restriction
      (narrow-to-region p1 p2)
      (replace-regexp-pairs-region (point-min) (point-max) '(["  +" " "]))
      (replace-pairs-region (point-min) (point-max)
                            '(
                              ["\n" ""]
                              [" /* " "/*"]
                              [" */ " "*/"]
                              [" {" "{"]
                              ["{ " "{"]
                              ["; " ";"]
                              [": " ":"]

                              [";}" "}"]
                              ["}" "}\n"]
                              )) ) ) )



(defun insert-tag ()
  "Insert a HTML tag based on the current line.

If current line is empty, then insert “<p></p>”.
If current line is a single word, use that word as tag name.
If current line has 2 words, use first word as tag name,
second word as value for attribute “class”.

For example,

pre poem

will become

<pre class=\"poem\"></pre>."
  (interactive)
  (let (bds p1 p2 myline goodies tagname classVal insStr)

    (setq bds (get-selection-or-unit 'line))
    (setq myline (elt bds 0) )
    (setq p1 (elt bds 1) )
    (setq p2 (elt bds 2) )

    (setq goodies (split-string myline " +" t))

    (cond
     ((equal (length goodies) 1) (progn (setq tagname "div") (setq classVal (nth 0 goodies))))
     ((equal (length goodies) 2) (progn (setq tagname (nth 0 goodies)) (setq classVal (nth 1 goodies)))))
    (delete-region p1 p2)

    (cond
     ((equal (length goodies) 0) (insert "<p>\n</p>") )
     ((and t) (insert (concat "<" tagname " class=\"" classVal "\">\n</" tagname ">")))
     )
    (search-backward "<")
    (backward-char 1)
    ))



(defun update-html-title (myTitle)
  "update html “title” and “h1” tags on current buffer.
current buffer must be a html page.
Any <title>…</title> or <h1>…</h1> tag content are changed.
If there's a text selection, work on text selection, else whole visible buffer."
  (interactive "sEnter Title:")
  (let (p1 p2 )
    (if (region-active-p)
        (progn (setq p1 (region-beginning) )
               (setq p2 (region-end) )
               )
      (progn (setq p1 (point-min) )
             (setq p2 (point-max) )
             )
      )
    (save-excursion
      (replace-regexp-pairs-region p1 p2
                                   (vector
                                    (vector "<title>\\([^<]+?\\)</title>" (format "<title>%s</title>" myTitle))
                                    (vector "<h1>\\([^<]+?\\)</h1>" (format "<h1>%s</h1>" myTitle))
                                    )
                                   "FIXEDCASE" "LITERAL") ) ) )

(defun insert-div-x-note ()
  "Insert a HTML markup."
  (interactive)
  (insert "<div class=\"x-note\"></div>\n")
  (backward-char 7)
  )

(defun xah-annotate ()
  "Create a annotation in HTML.
Wrap HTML “span” tag around current word or text selection, then
insert a div tag above the current paragraph."
  (interactive)
  (let (bds inputText)

    (setq bds (get-selection-or-unit 'word))
    (setq inputText (elt bds 0) )

    (wrap-html-tag "span" "xnt")
    (search-backward "<p")
    (insert "\n")
    (backward-char 1)
    (insert-div-x-note)
    (insert (format "<b class=\"x3nt\">%s</b>⇒ " inputText)  )
    )
  )

(defun wrap-html-tag (tagName &optional className ξid)
  "Add a HTML tag to beginning and ending of current word or text selection.

When preceded with `universal-argument',
no arg = prompt for tag, class.
2 = prompt for tag, id.
any = prompt for tag, id, class.

When called interactively,
Default id value is 「id‹random number›」.
Default class value is 「xyz」.

When called in lisp program, if className is nil or empty string, don't add the attribute. Same for ξid."
  (interactive
   (cond
    ((equal current-prefix-arg nil)     ; universal-argument not called
     (list
      (read-string "Tag (span):" nil nil "span") ))
    ((equal current-prefix-arg '(4))    ; C-u
     (list
      (read-string "Tag (span):" nil nil "span")
      (read-string "Class (xyz):" nil nil "xyz") ))
    ((equal current-prefix-arg 2)       ; C-u 2
     (list
      (read-string "Tag (span):" nil nil "span")
      (read-string "id:" nil nil (format "id%d" (random (expt 2 28 ))))
      ))
    (t                                  ; all other cases
     (list
        (read-string "Tag (span):" nil nil "span")
        (read-string "Class (xyz):" nil nil "xyz")
        (read-string "id:" nil nil (format "id%d" (random (expt 2 28 )))) )) ) )
  (let (bds p1 p2 inputText outputText
            (classStr (if (equal className nil) "" (format " class=\"%s\"" className)))
            (idStr (if (equal ξid nil) "" (format " id=\"%s\"" ξid)))
            )
    (setq bds (get-selection-or-unit 'word))
    (setq inputText (elt bds 0) )
    (setq p1 (elt bds 1) )
    (setq p2 (elt bds 2) )

    (setq outputText (format "<%s%s%s>%s</%s>" tagName idStr classStr inputText tagName ) )

    (delete-region p1 p2)
    (goto-char p1)
    (insert outputText) ) )

(defun mark-unicode (p1)
  "Wrap 「<b class=\"u\"></span>」 around current character.

When called in elisp program, wrap the tag at point P1."
  (interactive (list (point)))
  (goto-char p1)
  (insert "<b class=\"u\">")
  (forward-char 1)
  (insert "</b>"))



(defun add-paragraph-tag ()
  "Add <p>…</p> tag to current paragraph or text selection."
  (interactive)
  (let (bds p1 p2 inputText)

    (setq bds (get-selection-or-unit 'block))
    (setq inputText (elt bds 0) )
    (setq p1 (elt bds 1) )
    (setq p2 (elt bds 2) )

    (delete-region p1 p2 )
    (insert "<p>" (replace-regexp-in-string "\n\n+" "</p>\n\n<p>" (trim-string inputText)) "</p>")
    )
  )

(defun make-citation ()
  "Reformat current text block or selection into a canonical citation format.

For example, place cursor somewhere in the following block:

Circus Maximalist
By PAUL GRAY
Monday, Sep. 12, 1994
http://www.time.com/time/magazine/article/0,9171,981408,00.html

After execution, the lines will become

<cite>Circus Maximalist</cite> <time>1994-09-12</time> By Paul Gray. @ <a href=\"http://www.time.com/time/magazine/article/0,9171,981408,00.html\">Source www.time.com</a>

If there's a text selection, use it for input, otherwise the input is a text block between empty lines."
  (interactive)
  (let (bds p1 p2 inputText myList ξtitle ξauthor ξdate ξurl )

    (setq bds (get-selection-or-unit 'block))
    (setq inputText (elt bds 0) )
    (setq p1 (elt bds 1) )
    (setq p2 (elt bds 2) )

    (setq inputText (replace-regexp-in-string "^[[:space:]]*" "" inputText)) ; remove white space in front

    (setq myList (split-string inputText "[[:space:]]*\n[[:space:]]*" t) )

    (setq ξtitle (trim-string (elt myList 0)))
    (setq ξtitle (replace-regexp-in-string "^\"\\(.+\\)\"$" "\\1" ξtitle))
    (setq ξtitle (replace-pairs-in-string ξtitle '(["’" "'"] ["&" "＆"] )))

    (setq ξauthor (trim-string (elt myList 1)))
    (setq ξdate (trim-string (elt myList 2)))
    (setq ξurl (trim-string (elt myList 3)))

    (setq ξauthor (replace-regexp-in-string "\\. " " " ξauthor)) ; remove period in Initals
    (setq ξauthor (replace-regexp-in-string "By +" "" ξauthor))
    (setq ξauthor (upcase-initials (downcase ξauthor)))
    (setq ξdate (fix-timestamp ξdate))

    (setq ξurl (with-temp-buffer (insert ξurl) (source-linkify 2) (buffer-string)))

    (delete-region p1 p2 )
    (insert (concat "<cite>" ξtitle "</cite>") " " "<time>" ξdate "</time>"  " By " ξauthor ". @ " ξurl)
    ))

(defun htmlize-keyboard-shortcut-notation ()
  "Wrap a “kbd” tag around keyboard keys on current text inside 【】, or text selection.
e.g.
 【ctrl+w】
becomes
 【<kbd>Ctrl</kbd>+<kbd>w</kbd>】
Same for Alt, Shift, Cmd, Win, Enter, Return, Home… and other strings."
  (interactive)

  (let (p1 p2 inputStr resultStr replaceList)
    (if (region-active-p)
        (progn
          (setq p1 (region-beginning))
          (setq p2 (region-end))
          )
      (save-excursion
        (progn
          (if (search-backward "【" nil t)
              (progn (forward-char)
                     (setq p1 (point) ) )
            (setq p1 (line-beginning-position) )
            )

          (if (search-forward "】" nil t)
              (progn (backward-char)
                     (setq p2 (point) ))
            (setq p2 (line-end-position) ) ) )) )
    (setq inputStr (buffer-substring-no-properties p1 p2))

    (setq replaceList [
 ;; case in find string shouldn't matter.
["Ctrl" "<kbd>Ctrl</kbd>"]
["AltGr" "<kbd>AltGr</kbd>"]
["Alt" "<kbd>Alt</kbd>"]
["Shift" "<kbd>⇧ Shift</kbd>"]
["Cmd" "<kbd>⌘ Cmd</kbd>"]
["Option" "<kbd>⌥ Opt</kbd>"]
["Opt" "<kbd>⌥ Opt</kbd>"]
["Win" "<kbd>❖ Win</kbd>"]
["App" "<kbd>▤ Menu</kbd>"]
["Menu" "<kbd>▤ Menu</kbd>"]
["Meta" "<kbd>Meta</kbd>"]
["super" "<kbd>Super</kbd>"]

["Return" "<kbd>Return ↩</kbd>"]
["Enter" "<kbd>Enter ↵</kbd>"]
["Backspace" "<kbd>⌫ Backspace</kbd>"]
["Delete" "<kbd>⌦ Delete</kbd>"]
["Del" "<kbd>⌦ Delete</kbd>"]
["Space" "<kbd>Space</kbd>"]
["Caps Lock" "<kbd>Caps Lock</kbd>"]
["CapsLock" "<kbd>Caps Lock</kbd>"]
["F Lock" "<kbd>F Lock</kbd>"]
["Num Lock" "<kbd>Num Lock</kbd>"]
["Tab" "<kbd>Tab ↹</kbd>"]
["Esc" "<kbd>Esc</kbd>"]

["F10" "<kbd>F10</kbd>"]
["F11" "<kbd>F11</kbd>"]
["F12" "<kbd>F12</kbd>"]
["F1" "<kbd>F1</kbd>"]
["F2" "<kbd>F2</kbd>"]
["F3" "<kbd>F3</kbd>"]
["F4" "<kbd>F4</kbd>"]
["F5" "<kbd>F5</kbd>"]
["F6" "<kbd>F6</kbd>"]
["F7" "<kbd>F7</kbd>"]
["F8" "<kbd>F8</kbd>"]
["F9" "<kbd>F9</kbd>"]
["Fn" "<kbd>Fn</kbd>"]

["←" "<kbd>←</kbd>"]
["→" "<kbd>→</kbd>"]
["↑" "<kbd>↑</kbd>"]
["↓" "<kbd>↓</kbd>"]
["Home" "<kbd>↖ Home</kbd>"]
["End" "<kbd>↘ End</kbd>"]
["PageUp" "<kbd>⇞ Page △</kbd>"]
["Page Up" "<kbd>⇞ Page △</kbd>"]
["PgUp" "<kbd>⇞ Page △</kbd>"]
["PageDown" "<kbd>⇟ Page ▽</kbd>"]
["Page Down" "<kbd>⇟ Page ▽</kbd>"]
["PgDn" "<kbd>⇟ Page ▽</kbd>"]
["insert" "<kbd>Insert</kbd>"]
["ins" "<kbd>Insert</kbd>"]

["‹key›" "<kbd>‹key›</kbd>"]
                       ])

    (let ((case-fold-search t) (case-replace nil)
          )
      (setq resultStr (replace-pairs-in-string inputStr replaceList))
      )

    (setq resultStr (replace-regexp-pairs-in-string resultStr
 [
 ["\+\\([^<]\\) \\(.\\) \\(.\\)\\'" "+<kbd>\\1</kbd> <kbd>\\2</kbd> <kbd>\\3</kbd>"]
 ["\+\\([^<]\\) \\([A-Za-z0-0]\\)\\'" "+<kbd>\\1</kbd> <kbd>\\2</kbd>"]
 ["\+\\([^<]\\)" "+<kbd>\\1</kbd>"]
 ]))

    (delete-region p1 p2)
    (insert resultStr)
    )

  ;; test cases
  ;; 【Ctrl+x a】
  ;; 【Ctrl+x a b】
  ;; 【Ctrl+x Ctrl+j】
  )

(defun emacs-to-windows-kbd-notation (p1 p2)
  "Change emacs keyboard-shortcut notation to Windows's notation.

When called interactively, work on text enclosed in 【…】, or text selection.

For example:
 「【C-h f】」⇒ 「【Ctrl+h f】」
 「【M-a】」⇒ 「【Meta+a】」

This command is just for convenient, not 100% correct translation.

Partly because the Windows key notation isn't exactly standardized. e.g. up arrow key may be ↑ or UpArrow.
"
  (interactive
   (let ((bds (get-selection-or-unit ["^【" "^】"])) )
     (list (elt bds 1) (elt bds 2)) ) )

  (let (  (case-fold-search nil))
    (replace-pairs-region p1 p2
                          '(
                            ["C-" "Ctrl+"]
                            ["M-" "Meta+"]
                            ["S-" "Shift+"]

                            ["s-" "Super+"]
                            ["H-" "Hyper+"]

                            ["<prior>" "PageUp"]
                            ["<next>" "PageDown"]
                            ["<home>" "Home"]
                            ["<end>" "End"]

                            ["RET" "Enter"]
                            ["<return>" "Enter"]
                            ["TAB" "Tab"]
                            ["<tab>" "Tab"]

                            ["<right>" "→"]
                            ["<left>" "←"]
                            ["<up>" "↑"]
                            ["<down>" "↓"]

                            ["<insert>" "Insert"]
                            ["<delete>" "Delete"]

                            ["<backspace>" "Backspace"]
                            ["DEL" "Delete"]
                            ))
    )
  )

(defun xah-update-article-timestamp ()
  "Update article's timestamp.
Add today's date to the form
 <p class=\"author_0\">Xah Lee, <time>2005-01-17</time>, <time>2011-07-25</time></p>
 of current file."
  (interactive)
  (let (p1 p2)
    (save-excursion
      (goto-char 1)
      (split-window-vertically)
      (when (search-forward "<p class=\"author_0\">Xah Lee" nil)
        (beginning-of-line)
        (setq p1 (point) )
        (end-of-line)
        (setq p2 (point) )
        (search-backward "</p>")
        (insert (format ", <time>%s</time>" (format-time-string "%Y-%m-%d"))
                ) ) ) ))

(defun xah-update-page-tag-old (p1 p2)
  "Update html page navigation tags.

The input is a text selection.
Each line should a file name
Update each file's page navigation tag.

Each file name is a file path without dir, and relative to current dir.
Sample text selection for input:
“combowords.html
combowords-2.html
combowords-3.html
combowords-4.html”"
  (interactive "r")
  (let (filez pageNavStr (i 1))
    (setq filez
          (split-string (buffer-substring-no-properties p1 p2) "\n" t)
          )

    (delete-region p1 p2)

    ;; generate the page nav string
    (setq pageNavStr "<div class=\"pgs\">")

    (while (<= i (length filez))
      (setq pageNavStr
            (concat pageNavStr
                    "<a href=\""
                    (nth (- i 1) filez)
                    "\">"
                    (number-to-string i)
                    "</a>, ")
            )
      (setq i (1+ i))
      )

    (setq pageNavStr (substring pageNavStr 0 -2) ) ; remove the last ", "
    (setq pageNavStr (concat pageNavStr "</div>"))

    ;; open each file, insert the page nav string, remove link in the
    ;; nav string that's the current page
    (mapc
     (lambda (thisFile)
       (message "%s" thisFile)
       (find-file thisFile)
       (goto-char (point-min))
       (search-forward "<div class=\"pgs\">")
       (beginning-of-line)
       (kill-line 1)
       (insert pageNavStr "\n")
       (search-backward (file-name-nondirectory buffer-file-name))

       (require 'sgml-mode)
       (sgml-delete-tag 1)
       ;;        (save-buffer)
       ;;        (kill-buffer)
       )
     filez)
    ))

(defun xah-update-page-tag ()
  "Update html page navigation tags.

The input is a text block or text selection.
Each line should a file name/path (can be relative path)
Update each file's page navigation tag.

Each file name is a file path without dir, and relative to current dir.
Sample text selection for input:

words.html
words-2.html
words-3.html
words-4.html
"
  (interactive)
  (require 'sgml-mode)
  (let (bds p1 p2 inputStr fileList pageNavStr )
    (setq bds (get-selection-or-unit 'block))
    (setq inputStr (elt bds 0) p1 (elt bds 1) p2 (elt bds 2)  )
    (setq fileList (split-string (buffer-substring-no-properties p1 p2) "\n" t) )

    (delete-region p1 p2)

    ;; generate the page nav string
    (setq pageNavStr (format "<nav class=\"page\">\n%s</nav>"
                             (let (ξresult linkPath fTitle (ξi 0) )
                               (while (< ξi (length fileList))
                                 (setq linkPath (elt fileList ξi) )
                                 (setq fTitle (get-html-file-title linkPath) )
                                 (setq ξresult (concat ξresult "<a href=\"" linkPath "\" title=\"" fTitle "\">" (number-to-string (1+ ξi)) "</a>\n") )
                                 (setq ξi (1+ ξi))
                                 )
                               ξresult
                               )))

    ;; open each file, insert the page nav string
    (mapc
     (lambda (thisFile)
       (message "%s" thisFile)
       (find-file thisFile)
       (goto-char 1)

       (if
           (search-forward "<nav class=\"page\">" nil t)
           (let (p3 p4 )
             (search-backward "<")
             (setq p3 (point))
             (sgml-skip-tag-forward 1)
             (setq p4 (point))
             (delete-region p3 p4)
             (insert pageNavStr)
             )
         (progn
           (search-forward "<script><!--
google_ad_client")
           (progn
             (search-backward "<script>")
             (insert pageNavStr "\n\n")
             ) ) )

       )
     fileList)
    ))

(defun extract-url (htmlText)
  "Returns a list of URLs in the HTML text string htmlText.

When called interactively, use text selection as input, or current text block between empty lines. Output URLs in a buffer named 「*extract URL output*」.

When called in a program, the first URL is the last list element.

WARNING: this function extract all text of the form 「<a … href=\"…\" …>」 by a simple regex. It does not extract single quote form 「href='…'」 nor 「src=\"…\"」 , nor other considerations."
  (interactive (list (elt (get-selection-or-unit 'block) 0) ) )
  (let ((urlList (list)))
    (with-temp-buffer
      (insert htmlText)
      (goto-char 1)
      (while (re-search-forward "<a.+?href=\"\\([^\"]+?\\)\".+?>" nil t)
        (setq urlList (cons (match-string 1) urlList))
        ))

    (when (called-interactively-p 'any)
        (with-output-to-temp-buffer "*extract URL output*"
          (mapc (lambda (ξx) (princ ξx) (terpri) ) (reverse urlList))
          )
      )
    urlList
    ))

