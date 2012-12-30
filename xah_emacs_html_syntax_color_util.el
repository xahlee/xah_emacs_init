;; -*- coding: utf-8 -*-
;; some custome functions to syntax color source code for publishing in HTML. i.e. htmlize
;; 2011-05-29
;;   Xah Lee
;; ∑ http://xahlee.org/



(require 'htmlize)

(defvar ξ-language-name-map nil "a alist that maps lang name. Each element has this form 「(‹lang code› . [‹emacs major mode name› ‹file_extension›])」")
(setq ξ-language-name-map
'(
           ("ahk" . ["ahk-mode" "ahk"])

           ("code" . ["fundamental-mode" "txt"])
           ("output" . ["fundamental-mode" "txt"])

           ("bash" . ["sh-mode" "sh"])
           ("cmd" . ["dos-mode" "bat"])

           ("bbcode" . ["xbbcode-mode" "bbcode"])
           ("c" . ["c-mode" "c"])
           ("cpp" . ["c++-mode" "cpp"])
           ("cl" . ["lisp-mode" "lisp"])
           ("clojure" . ["clojure-mode" "clj"])
           ("css" . ["css-mode" "css"])
           ("elisp" . ["emacs-lisp-mode" "el"])
           ("haskell" . ["haskell-mode" "hs"])
           ("html" . ["html-mode" "html"])
           ("mysql" . ["sql-mode" "sql"])
           ("xml" . ["sgml-mode"])
           ("html6" . ["html6-mode" "html6"])
           ("java" . ["java-mode" "java"])
           ("js" . ["js-mode" "js"])
           ("lsl" . ["xlsl-mode" "lsl"])
           ("ocaml" . ["tuareg-mode" "ocaml"])
           ("org" . ["org-mode" "org"])
           ("perl" . ["cperl-mode" "pl"])
           ("php" . ["php-mode" "php"])
           ("povray" . ["pov-mode" "pov"])
           ("powershell" . ["powershell-mode" "ps1"])
           ("python" . ["python-mode" "py"])
           ("python3" . ["python-mode" "py3"])
           ("qi" . ["shen-mode" "qi"])
           ("ruby" . ["ruby-mode" "rb"])
           ("scala" . ["scala-mode" "scala"])
           ("scheme" . ["scheme-mode" "scm"])
           ("yasnippet" . ["snippet-mode" "yasnippet"])
           ("vbs" . ["visual-basic-mode" "vbs"])
           ("visualbasic" . ["visual-basic-mode" "vbs"])
           ("mma" . ["fundamental-mode" "m"])
           ) )

(defun get-pre-block-langCode ()
  "Get the langCode and boundary of current HTML pre block.
A pre block is text of this form
<pre class=\"‹langCode›\">…▮…</pre>.

Returns a vector [langCode pos1 pos2], where pos1 pos2 are the boundary of the text content."
  (interactive)
  (let (langCode p1 p2)
    (if (region-active-p)
        (progn
          (setq p1 (region-beginning) )
          (setq p2 (region-end) )
          (setq langCode (read-string "langcode:"))
 (message "%s %d %d" langCode p1 p2)
          (vector langCode p1 p2)
          )
      (save-excursion
        (re-search-backward "<pre class=\"\\([-A-Za-z0-9]+\\)\"") ; tag begin position
        (setq langCode (match-string 1))
        (setq p1 (search-forward ">"))    ; text content begin
        (search-forward "</pre>")
        (setq p2 (search-backward "<"))   ; text content end
(message "%s %d %d" langCode p1 p2)
        (vector langCode p1 p2)
 ) ) ))

(defun get-pre-block-make-new-file (ξlangNameMap)
  "Create a new file on current dir with text inside pre code block.
For example, if the cursor is somewhere between the tags:
<pre class=\"…\">…▮…</pre>

after calling, all a new file of name 「xx-‹random›.‹suffix›」 is created in current dir, with content from the block.

If there's a text selection, use that region as content."
  (interactive (list ξ-language-name-map))
  (let* (
        (ξxx (get-pre-block-langCode))
        (ξlangCode (elt ξxx 0))
        (p1 (elt ξxx 1))
        (p2 (elt ξxx 2))
        (ξyy (cdr (assoc ξlangCode ξlangNameMap)))
        (ξfileSuffix (elt ξyy 1))
        (ξtextContent (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" (buffer-substring-no-properties p1 p2))) )
        )
    (delete-region p1 p2 )
    (split-window-vertically)
    (find-file (format "xx-testscript-%d.%s" (random 9008000 ) ξfileSuffix) )
    (insert ξtextContent)
    ;; (save-buffer )
    )
  )

(defun ξhtmlize-string (ξsourceCodeStr ξmajorModeName)
  "Take ξsourceCodeStr and return a htmlized version using major mode ξmajorModeName.
The purpose is to syntax color source code in HTML.
This function requires the `htmlize-buffer' from 〔htmlize.el〕 by Hrvoje Niksic."
  (interactive)
  (let (htmlizeOutputBuffer resultStr)
    ;; put code in a temp buffer, set the mode, fontify
    (with-temp-buffer
      (insert ξsourceCodeStr)
      (funcall (intern ξmajorModeName))
      (font-lock-fontify-buffer)
      (setq htmlizeOutputBuffer (htmlize-buffer))
      )
    ;; extract the fontified source code in htmlize output
    (with-current-buffer htmlizeOutputBuffer
      (let (p1 p2 )
        (setq p1 (search-forward "<pre>"))
        (setq p2 (search-forward "</pre>"))
        (setq resultStr (buffer-substring-no-properties (+ p1 1) (- p2 6))) ) )
    (kill-buffer htmlizeOutputBuffer)
    resultStr ) )

(defun htmlize-pre-block (ξlangCodeMap)
  "Replace text enclosed by “pre” tag to htmlized code.
For example, if the cursor is somewhere between the pre tags <pre class=\"‹langCode›\">…▮…</pre>, then after calling, the text inside the pre tag will be htmlized.  That is, wrapped with many span tags.

The opening tag must be of the form <pre class=\"‹langCode›\">.  The ‹langCode› determines what emacs mode is used to colorize the text. See `ξ-language-name-map' for possible ‹langCode›.

See also: `dehtmlize-pre-block', `htmlize-or-dehtmlize-pre-block'.
This function requires the `htmlize-buffer' from 〔htmlize.el〕 by Hrvoje Niksic."
  (interactive (list ξ-language-name-map))
  (let (ξlangCode p1 p2 inputStr ξmodeName )

    (save-excursion
      (let (( ξxx (get-pre-block-langCode)))
        (setq ξlangCode (elt ξxx 0))
        (setq p1 (elt ξxx 1))
        (setq p2 (elt ξxx 2))
        (setq inputStr (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]*\\'" "" (buffer-substring-no-properties p1 p2))) )
        (setq ξmodeName (elt (cdr (assoc ξlangCode ξlangCodeMap)) 0))
        )
      (delete-region p1 p2 )
      (goto-char p1)
      (insert (ξhtmlize-string inputStr ξmodeName))
      )
    ) )



(defun dehtmlize-pre-block ()
  "Delete span tags between pre tags.

Note: only span tags of the form 「<span class=\"…\">…</span>」 are deleted.

This command does the reverse of `htmlize-pre-block'."
  (interactive)
  (let (( ξxx (get-pre-block-langCode)))
    (dehtmlize-span-region (elt ξxx 1) (elt ξxx 2))
    )
  )

(defun htmlize-or-dehtmlize-pre-block (langCodeMap)
  "Call `htmlize-pre-block' or `dehtmlize-pre-block'."
  (interactive (list ξ-language-name-map))
  (let* (
         (ξxx (get-pre-block-langCode))
         (langCode (elt ξxx 0))
         (p1 (elt ξxx 1))
         (p2 (elt ξxx 2))
         (inputStr (buffer-substring-no-properties p1 p2) )
         )

    (if (string-match "<span class=" inputStr)
        (dehtmlize-span-region p1 p2)
      (progn
        (delete-region p1 p2)
        (insert (ξhtmlize-string (replace-regexp-in-string "\\`[ \t\n]*" "" (replace-regexp-in-string "[ \t\n]+\\'" "\n" inputStr)) (elt (cdr (assoc langCode langCodeMap)) 0)))
          )
      ) ) )

(defun dehtmlize-span-region (p1 p2)
  "Delete HTML “span” tags in region.
Note: only span tags of the form 「<span class=\"…\">…</span>」 are deleted.
And 3 html entities &amp; &lt; &gt; are changed to & < >."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region p1 p2)
      (replace-regexp-pairs-region (point-min) (point-max) '(["<span class=\"[^\"]+\">" ""]))
      (replace-pairs-region (point-min) (point-max) '( ["</span>" ""] ["&amp;" "&"] ["&lt;" "<"] ["&gt;" ">"] ) ) ) ) )

(defun dehtmlize-text (ξstring &optional ξfrom ξto)
"Delete HTML tags in string or region.
Work on current text block or text selection. (a “text block” is text between empty lines)

When called in lisp code, if ξstring is non-nil, returns a changed string.  If ξstring nil, change the text in the region between positions ξfrom ξto.

WARNING: this implementation is for my personal use.
Is does not cover all html tags or convert all html entities.
For robust solution you might use: 「lynx -dump -display_charset=utf-8 URL」."
  (interactive
   (if (region-active-p)
       (list nil (region-beginning) (region-end))
     (let ((bds (get-selection-or-unit 'block)) )
       (list nil (elt bds 1) (elt bds 2))) ) )

  (let (workOnStringP inputStr outputStr)
    (setq workOnStringP (if ξstring t nil))
    (setq inputStr (if workOnStringP ξstring (buffer-substring-no-properties ξfrom ξto)))
    (setq outputStr
          (let ((case-fold-search t) (tempStr inputStr))
(setq tempStr (replace-regexp-pairs-in-string tempStr '(
["<a href=\"\\([^\"]+?\\)\">\\([^<]+?\\)</a>" "\\2 〔 \\1 〕"]
["<img src=\"\\([^\"]+?\\)\" alt=\"\\([^\"]+?\\)\" width=\"[0-9]+\" height=\"[0-9]+\" */?>" "〔IMAGE “\\2” \\1 〕"]
["<[a-z0-9]+ */?>" ""]
["<[a-z0-9]+ class=\"[^\"]+\">" ""]
["</[a-z0-9]+>" ""]

["&amp;" "&"]
["&lt;" "<"]
["&gt;" ">"]

)))

tempStr
             )  )

    (if workOnStringP
        outputStr
      (save-excursion
        (delete-region ξfrom ξto)
        (goto-char ξfrom)
        (insert outputStr) )) ) )
