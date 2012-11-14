;; -*- coding: utf-8 -*-
;; some custome functions to syntax color source code for publishing in HTML. i.e. htmlize
;; 2011-05-29
;;   Xah Lee
;; ∑ http://xahlee.org/



(require 'htmlize)

(defun htmlize-text (ξstring major-mode-name &optional from-to-positions-pair)
  "HTMLize text. That is, syntax color by adding span tags.

When called interactively, work on current pre tag text block or text selection.
A “pre tag text block” is text enclosed by the tags
 <pre class=\"lang-code\"> …▮…</pre>

after calling, the text inside the pre tag will be htmlized.
The “lang-code” can be any of {c, bash, cl, clojure, elisp, haskell, html, xml, js, …}.
See source code for exact list.

If there's a text selection, ask for a major-mode name and htmlize that region.

When called in lisp code:
First argument ΞSTRING is the input string.
Second argument MAJOR-MODE-NAME should be a possible value of the variable `major-mode'.
If third argument FROM-TO-POSITIONS-PAIR is nil, the function returns a string, else, work on that region.
It should be a vector or list for positions of the region,
 e.g. [pos1 pos2]

This function requries the `htmlize-buffer' from 〔htmlize.el〕 by Hrvoje Niksic."
  (interactive
   (let (ξlangCode ξmajorMode p1 p2
                   (langNameMap
                    '(("ahk" . "ahk-mode")

                      ("code" . "fundamental-mode")
                      ("output" . "fundamental-mode")

                      ("bash" . "sh-mode") ; unix script/lines to be run by bash. sh-mode = bash script. shell-mode  = inferor shell
                      ("cmd" . "dos-mode") ; Windows script/lines to be run by cmd.exe

                      ("bbcode" . "xbbcode-mode")
                      ("c" . "c-mode")
                      ("cpp" . "c++-mode")
                      ("cl" . "lisp-mode")
                      ("clojure" . "clojure-mode")
                      ("css" . "css-mode")
                      ("elisp" . "emacs-lisp-mode")
                      ("haskell" . "haskell-mode")
                      ("html" . "html-mode")
                      ("mysql" . "sql-mode")
                      ("xml" . "sgml-mode")
                      ("html6" . "html6-mode")
                      ("java" . "java-mode")
                      ("javascript" . "js-mode")
                      ("js" . "js-mode")
                      ("lsl" . "xlsl-mode")
                      ("ocaml" . "tuareg-mode")
                      ("org" . "org-mode")
                      ("perl" . "cperl-mode")
                      ("php" . "php-mode")
                      ("povray" . "pov-mode")
                      ("powershell" . "powershell-mode")
                      ("python" . "python-mode")
                      ("qi" . "shen-mode")
                      ("ruby" . "ruby-mode")
                      ("scala" . "scala-mode")
                      ("scheme" . "scheme-mode")
                      ("yasnippet" . "snippet-mode")
                      ("vbs" . "visual-basic-mode")
                      ("visualbasic" . "visual-basic-mode")
                      ("mma" . "fundamental-mode")
                      )))

     (if (region-active-p)
         (progn
           (setq ξmajorMode (read-from-minibuffer "Major mode name:" nil nil nil nil "text-mode" nil))
           (list nil ξmajorMode (vector (region-beginning) (region-end)))
           )
       (progn
         (save-excursion
           (re-search-backward "<pre class=\"\\([-A-Za-z0-9]+\\)\">")
           (setq ξlangCode (match-string 1))
           (setq ξmajorMode (cdr (assoc ξlangCode langNameMap)))
           (setq p1 (search-forward ">"))
           (setq p2 (- (search-forward "</pre>") 6) )
           (list nil ξmajorMode (vector p1 p2) ) ) ) ) ) )

;; (when (not ξmajorMode) (error "Your language string 「class=\"%s\"」 doesn't match any supported language string. Supported language string are %S" ξmajorMode langNameMap ))

  (let (workOnStringP inputStr outputStr
                      (ξfrom (elt from-to-positions-pair 0))
                      (ξto (elt from-to-positions-pair 1))
                      )
    (setq workOnStringP (if from-to-positions-pair nil t))
    ;; (insert-buffer-substring BUFFER &optional START END)

    (setq inputStr (if workOnStringP ξstring (buffer-substring-no-properties ξfrom ξto)))
    (setq outputStr
          (let (htmlizeOutputBuffer resultStr)
            ;; put code in a temp buffer, set the mode, fontify
            (with-temp-buffer
              (insert inputStr)
              (funcall (intern major-mode-name))
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

    (if workOnStringP
        outputStr
      (save-excursion
        (delete-region ξfrom ξto)
        (goto-char ξfrom)
        (insert outputStr) )) ) )

;; (defun htmlize-string (sourceCodeStr langModeName)
;;   "Take SOURCECODESTR and return a htmlized version using LANGMODENAME.
;; This function requries the htmlize.el by Hrvoje Niksic."
;;   (require 'htmlize)
;;   (let (htmlizeOutputBuf p1 p2 resultStr)

;;     ;; put code in a temp buffer, set the mode, fontify
;;     (with-temp-buffer
;;       (insert sourceCodeStr)
;;       (funcall (intern langModeName))
;;       (font-lock-fontify-buffer)
;;       (setq htmlizeOutputBuf (htmlize-buffer))
;;       )

;;     ;; extract the fontified source code in htmlize output
;;     (with-current-buffer htmlizeOutputBuf
;;       (setq p1 (search-forward "<pre>"))
;;       (setq p2 (search-forward "</pre>"))
;;       (setq resultStr (buffer-substring-no-properties (+ p1 1) (- p2 6))))

;;     (kill-buffer htmlizeOutputBuf)
;;     resultStr
;;     ))

;; (defun htmlize-pre-block ()
;;   "Replace text enclosed by <pre> tag to htmlized code.
;; For example, if the cursor is somewhere between the pre tags:
;;  <pre class=\"lang-code\">…▮…</pre>

;; after calling, the text inside the pre tag will be htmlized.
;; That is, wrapped with many span tags.

;; The opening tag must be of the form <pre class=\"lang-code\">.
;; The “lang-code” determines what emacs mode is used to colorize the
;; text.

;;  “lang-code” can be any of {c, elisp, java, javascript, html, xml, css, …}.
;;  (See source code for a full list)

;; See also: `dehtmlize-pre-block'.

;; This function requires htmlize.el by Hrvoje Niksic."
;;   (interactive)
;;   (let (inputStr langCode p1 p2 modeName
;;     (langModeMap
;;      '(
;;        ("ahk" . "ahk-mode")
;;        ("bash" . "sh-mode")
;;        ("bbcode" . "xbbcode-mode")
;;        ("c" . "c-mode")
;;        ("cl" . "lisp-mode")
;;        ("clojure" . "clojure-mode")
;;        ("cmd" . "dos-mode")
;;        ("css" . "css-mode")
;;        ("elisp" . "emacs-lisp-mode")
;;        ("haskell" . "haskell-mode")
;;        ("html" . "html-mode")
;;        ("xml" . "sgml-mode")
;;        ("html6" . "html6-mode")
;;        ("java" . "java-mode")
;;        ("javascript" . "js-mode")
;;        ("js" . "js-mode")
;;        ("lsl" . "xlsl-mode")
;;        ("ocaml" . "tuareg-mode")
;;        ("org" . "org-mode")
;;        ("perl" . "cperl-mode")
;;        ("php" . "php-mode")
;;        ("povray" . "pov-mode")
;;        ("powershell" . "powershell-mode")
;;        ("python" . "python-mode")
;;        ("ruby" . "ruby-mode")
;;        ("scala" . "scala-mode")
;;        ("scheme" . "scheme-mode")
;;        ("vbs" . "visual-basic-mode")
;;        ("visualbasic" . "visual-basic-mode")
;;        ) ))

;;     (save-excursion
;;       (re-search-backward "<pre class=\"\\([-A-Za-z0-9]+\\)\"") ; tag begin position
;;       (setq langCode (match-string 1))
;;       (setq p1 (search-forward ">")) ; lang source code string begin 
;;       (search-forward "</pre>")
;;       (setq p2 (search-backward "<")) ; lang source code string end
;;       (search-forward "</pre>") ; tag end position
;;       (setq inputStr (buffer-substring-no-properties p1 p2))

;;       (setq modeName
;;             (let ((tempVar (assoc langCode langModeMap) ))
;;               (if tempVar (cdr tempVar) "text-mode" ) ) )

;;       (delete-region p1 p2)
;;       (goto-char p1)
;;       (insert (htmlize-string inputStr modeName)) ) ) )

(defun dehtmlize-pre-block (p1 p2)
  "Delete span tags between pre tags.
For example, if the cursor is somewhere between the tags:
<pre class=\"…\">…▮…</pre>

after calling, all span tags inside the block will be removed.
If there's a text selection, dehtmlize that region.

Note: only span tags of the form 「<span class=\"…\">…</span>」 are deleted.

This command does the reverse of `htmlize-pre-block'."
  (interactive
   (if (region-active-p)
       (list (region-beginning) (region-end))
     (let (p3 p4)
       (save-excursion
         (search-backward "<pre class")
         (re-search-forward ">")
         (setq p3 (point)) ; code begin position
         (re-search-forward "</pre>")
         (setq p4 (- (point) 6)) ; code end position
         (list p3 p4 )) ) ) )
  (dehtmlize-span-region p1 p2)
   )

(defun dehtmlize-span-region (p1 p2)
  "Delete HTML “span” tags in region.
Note: only span tags of the form 「<span class=\"…\">…</span>」 are deleted."
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
"
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
(setq tempStr (replace-pairs-in-string tempStr
 '(
["<html>" ""] ["</html>" ""]
["<head>" ""] ["</head>" ""]
["<title>" ""] ["</title>" ""]
["<body>" ""] ["</body>" ""]
["<pre>" ""] ["</pre>" ""]

["<h1>" ""] ["</h1>" ""]
["<h2>" ""] ["</h2>" ""]
["<h3>" ""] ["</h3>" ""]
["<h4>" ""] ["</h4>" ""]
["<h5>" ""] ["</h5>" ""]
["<p>" ""] ["</p>" ""]

["<i>" ""] ["</i>" ""]
["<b>" ""] ["</b>" ""]
["<kbd>" ""] ["</kbd>" ""]

["<ul>" ""] ["</ul>" ""]
["<ol>" ""] ["</ol>" ""]
["<li>" ""] ["</li>" ""]
["<blockquote>" ""] ["</blockquote>" ""]

["<table>" ""] ["</table>" ""]
["<table class=\"nrm\">" ""] ["</table>" ""]
["<caption>" ""] ["</caption>" ""]
["<tr>" ""] ["</tr>" ""]
["<td>" ""] ["</td>" ""]
["<th>" ""] ["</th>" ""]

["<br>" ""]
["<hr>" ""]
)))

(setq tempStr (replace-regexp-pairs-in-string tempStr '(
["<span class=\"[^\"]+\">" ""] ["</span>" ""]
["<pre class=\"[^\"]+\">" ""] ["</pre>" ""]
["<div class=\"[^\"]+\">" ""] ["</div>" ""]
["<a href=\"[^\"]+\">" ""] ["</a>" ""]
)))

(setq tempStr (replace-pairs-in-string tempStr
 '(
["&amp;" "&"] ["&lt;" "<"] ["&gt;" ">"]
)))

tempStr
             )  )

    (if workOnStringP
        outputStr
      (save-excursion
        (delete-region ξfrom ξto)
        (goto-char ξfrom)
        (insert outputStr) )) ) )