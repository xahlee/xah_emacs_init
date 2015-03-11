;; -*- coding: utf-8 -*-
;; stuff related to HTML
;; most things moved to xah-html-mode
;; ∑ http://xahlee.org/

(defun forward-html-end-tag ()
  "Move cursor to the next HTML tag's content."
  (interactive)
  (forward-char 1)
  (search-forward "</")
  (backward-char 2)
  )

(defun backward-html-end-tag ()
  "Move cursor to the previous HTML tag's content."
  (interactive)
  (search-backward "</")
  ;; (forward-char-char 2)
  )

(defun xah-add-reference-span-tag ()
  "Add <span class=\"ref\">…</span> tag to current line or text selection."
  (interactive)
  (require 'xah-html-mode)
  (let ( ξp1 ξp2 )
    (if (use-region-p)
        (progn (setq ξp1 (region-beginning))
               (setq ξp2 (region-end)))
      (progn (setq ξp1 (line-beginning-position))
             (setq ξp2 (line-end-position))))
    (set-mark ξp1)
    (goto-char ξp2)
    (xhm-wrap-html-tag "span" "ref")))

(defun xahsite-update-article-timestamp ()
  "Update article's timestamp.
Add today's date to the byline tag of current file, also delete the last one if there are more than one.
WARNING: This command saves buffer if it's a file."
  (interactive)
  (require 'sgml-mode)
  (let (ξp1 ξp2 ξnum bufferTextOrig)
    (save-excursion
      (goto-char 1)
      (when (search-forward "<div class=\"byline\">" nil)

        (progn
          (setq ξp1 (point))
          (backward-char 1)
          (sgml-skip-tag-forward 1)
          ;; (search-forward "</time></div>")
          (setq ξp2 (point)))

        (save-restriction
          (narrow-to-region ξp1 ξp2)

          (setq bufferTextOrig (buffer-string ))
          (setq ξnum (count-matches "<time>" (point-min) (point-max)))

          ;; if there are more than 1 “time” tag, delete the last one
          (when (> ξnum 1)
            (let (ξp3 ξp4)
              (goto-char (point-max))
              (search-backward "</time>")
              (setq ξp4 (+ (point) 7))
              (search-backward "<time>")
              (setq ξp3 (point))
              (delete-region ξp3 ξp4 )))

          ;; insert new time
          (goto-char (point-max))
          (search-backward "</div>")
          (insert (format ", <time>%s</time>" (format-time-string "%Y-%m-%d")))

          ;; remove repeated comma separator
          (goto-char (point-min))
          (when (search-forward ", , " (point-max) "NOERROR")
            (replace-match ", "))

          (goto-char (point-min))
          (when (search-forward "</time>, <time>" (point-max) "NOERROR")
            (replace-match "</time>, …, <time>"))

          (when (buffer-file-name)
            (save-buffer))

          (message "%s\nchanged to\n%s" bufferTextOrig (buffer-string )))))))

(defun xahsite-update-page-tag ()
  "Update HTML page navigation tags.

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
  (let* (
         ξp1 ξp2
         (_setBoundary
          (save-excursion
            (if (re-search-backward "\n[ \t]*\n" nil "move")
                (progn (re-search-forward "\n[ \t]*\n")
                       (setq ξp1 (point)))
              (setq ξp1 (point)))
            (if (re-search-forward "\n[ \t]*\n" nil "move")
                (progn (re-search-backward "\n[ \t]*\n")
                       (setq ξp2 (point)))
              (setq ξp2 (point)))))
         (ξfileList (split-string (buffer-substring-no-properties ξp1 ξp2) "\n" t))
         ξpageNavStr )

    (delete-region ξp1 ξp2)

    ;; generate the page nav string
    (setq ξpageNavStr
          (format "<nav class=\"page\">\n%s</nav>"
                  (let (ξresult ξlinkPath ξfTitle (ξi 0))
                    (while (< ξi (length ξfileList))
                      (setq ξlinkPath (elt ξfileList ξi))
                      (setq ξfTitle (xhm-get-html-file-title ξlinkPath))
                      (setq ξresult (concat ξresult "<a href=\"" ξlinkPath "\" title=\"" ξfTitle "\">" (number-to-string (1+ ξi)) "</a>\n"))
                      (setq ξi (1+ ξi)))
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
           (let (ξp3 ξp4 )
             (search-backward "<")
             (setq ξp3 (point))
             (sgml-skip-tag-forward 1)
             (setq ξp4 (point))
             (delete-region ξp3 ξp4)
             (insert ξpageNavStr))
         (progn
           (search-forward "<script><!--
google_ad_client")
           (progn
             (search-backward "<script>")
             (insert ξpageNavStr "\n\n")))))
     ξfileList)))

(defun xah-syntax-color-hex ()
"Syntax color hex color spec ⁖ 「#ff1100」 in current buffer."
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[abcdef[:digit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (match-string-no-properties 0)))))))
  (font-lock-fontify-buffer)
  )

(defun xah-syntax-color-hsl ()
  "Syntax color CSS's HSL color spec ⁖ 「hsl(0,90%,41%)」 in current buffer."
  (interactive)
  (font-lock-add-keywords
   nil
  '(("hsl( *\\([0-9]\\{1,3\\}\\) *, *\\([0-9]\\{1,3\\}\\)% *, *\\([0-9]\\{1,3\\}\\)% *)"
     (0 (put-text-property
         (+ (match-beginning 0) 3)
         (match-end 0)
         'face (list :background
 (concat "#" (mapconcat 'identity
                        (mapcar
                         (lambda (x) (format "%02x" (round (* x 255))))
                         (color-hsl-to-rgb
                          (/ (string-to-number (match-string-no-properties 1)) 360.0)
                          (/ (string-to-number (match-string-no-properties 2)) 100.0)
                          (/ (string-to-number (match-string-no-properties 3)) 100.0)
                          ) )
                        "" )) ;  "#00aa00"
                      ))))) )
  (font-lock-fontify-buffer)
  )

;; (concat "#" (mapconcat 'identity
;;                         (mapcar
;;                          (lambda (x) (format "%x" (round (* x 255))))
;;                          (color-hsl-to-rgb
;;                           (/ (string-to-number "0") 360.0)
;;                           (/ (string-to-number "90") 100.0)
;;                           (/ (string-to-number "50") 100.0)
;;                           ) )
;;                         "" ))

;; (format "%2x" (round (* (/ (string-to-number "49") 100.0) 255)))
;; (format "%02x" 10)

(defun xah-python-ref-linkify ()
  "Transform current line (a file path) into a link.
For example, this line:

~/web/xahlee_info/python_doc_2.7.6/library/stdtypes.html#mapping-types-dict

becomes

<span class=\"ref\"><a href=\"../python_doc_2.7.6/library/stdtypes.html#mapping-types-dict\">5. Built-in Types — Python v2.7.6 documentation #mapping-types-dict</a></span>

The path is relative to current file. The link text is the linked file's title, plus fragment url part, if any.

Requires a python script. See code."
  (interactive)
  (let (scriptName bds)
    (setq bds (bounds-of-thing-at-point 'filename))
    (save-excursion
      (setq scriptName (format "/usr/bin/python ~/git/xahscripts/emacs_pydoc_ref_linkify.py %s" (buffer-file-name)))
      (shell-command-on-region (car bds) (cdr bds) scriptName nil "REPLACE" nil t))))
