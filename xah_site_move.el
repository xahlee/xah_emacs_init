;; -*- coding: utf-8; lexical-binding: t; -*-
;; 2012-02-11

;; fix all links of a given dir of html files.

;; for a web root dir, some subdir or files in it are moved.  This script takes the moved files/dirs as input, go thru all html files in web root dir, and correct all links.

;; • if the link is relative, the new path should also be relative
;; • if the link contain fragment id (e.g. hash char 「some.html#xyz」), the new path should also contain it

;; inputs:
;; • a root dir. e.g. /Users/xah/web/
;; • a list of (from, to) dirs.
;; for each pair, the 1st element is the “from” node, the second is the destination node (the node itself, not the parent to move to under)

;; • the destination node must not exist.
;; • each “from” node must be unique, and must exist

;; algorithm
;; ① open each html files in a web root dir.
;; ② for each link 「<… href=…>」 and 「… src=…」, call this “xlink”
;; if xlink is a xah site (e.g.  http://ergoemacs.org/ , http://xahlee.org/ , may be relative link), then, generate the full path fo xlink (call this xfpath), assuming the dir structure BEFORE the dir move.
;; check if xfpath points to the moved dir/file, generate xfpath-new.
;; change the link in the file to use xfpath-new.

;; issues of link string:
;; it can be a url 〔http://ergoemacs.org/index.html〕
;; relative file path e.g.  〔../i/qi_logo.html〕
;; when url, it may not contain file name. e.g. 〔http://ergoemacs.org/〕
;; may contain fragment id 〔http://ergoemacs.org/some.html#section〕
;; may be relative link. e.g. 〔qi_logo.html〕, 〔../i/qi_logo.html〕


;; (require 'find-lisp) ; in emacs
(require 'hi-lock) ; in emacs

(require 'seq)


(defvar ε-inputPath nil "Input dir. Must end with a slash")
(setq ε-inputPath "/Users/xah/web/xahlee_info/" )

(defvar ε-writeToFile-p nil "whether to write to file.")
(setq ε-writeToFile-p nil)

(defvar ε-debug-p nil "Boolean. Print debug info.")
(setq ε-debug-p nil )

(defun ε-check-this-link-p (linkString hostFilePath)
  "Return true or false.
This function can change arbitrarily. Its meant to be modified on-the-fly according to requirement.

linkString is the string of “href=…” value or “src=…” value.
hostFilePath is the file full path that contains the link."
t
  )

(defvar ε-skip-list nil "list of dirs to skip")
(setq ε-skip-list
      (mapcar
       (lambda (x) (concat (xahsite-server-root-path) "xahlee_info/" x))
       xahsite-external-docs))

(defvar ε-move-from-to-list nil "alist of dirs that are to be moved.
Each entry is of the form (‹from› . ‹to›).
• ‹from› and ‹to› must be full path.
• The ‹from› can be a file or dir.
• All dir paths must end with slash.
• If ‹from› is a dir, then ‹to› must also be a dir. Same for file.
• No ‹from› should be a subdir of each other.
• No ‹from› should be a identical to a ‹to› dir.
")

(setq ε-move-from-to-list
 '(

;; remove or regenerate ../wikipedia_links.html

("/Users/xah/web/xahlee_info/js/css2.html" . "/Users/xah/web/xahlee_info/css/css2.html")

;; ("c:/Users/h3/web/xahlee_org/sex/is_YouTube_porn_fodder.html" . "c:/Users/h3/web/xahlee_org/Periodic_dosage_dir/is_YouTube_porn_fodder.html")

;; ("c:/Users/h3/web/xaharts_org/funny/Microsoft_linux_ad.html" .
;; "c:/Users/h3/web/xahlee_info/funny/Microsoft_linux_ad.html"
;; )

;; http://xahlee.org/diklo/the_beauty_song.txt

;; http://xahlee.org/Periodic_dosage_dir/skina/apocalypse_now.html

;; ("c:/Users/h3/web/xahlee_org/Periodic_dosage_dir/lacru/index_batgirl_thumbnail.html" . "c:/Users/h3/web/xaharts_org/batgirl/index_batgirl_thumbnail.html")

;; ("c:/Users/h3/web/xahlee_org/Periodic_dosage_dir/lacru/_p/bg/" . "c:/Users/h3/web/xaharts_org/batgirl/i/")

;; ("c:/Users/h3/web/xahlee_org/Periodic_dosage_dir/lanci/i/na_tinbe/" . "c:/Users/h3/web/xahlee_org/Periodic_dosage_dir/lanci/i/sw/")

;; ("c:/Users/h3/web/xahlee_org/Periodic_dosage_dir/pirate_bay.html" . "c:/Users/h3/web/xahlee_info/comp/pirate_bay.html")

;("c:/Users/h3/web/xahlee_org/funny/condom_ad.html" . "c:/Users/h3/web/xahlee_org/sex/condom_ad.html")
;("c:/Users/h3/web/xaharts_org/funny/condom_ad.html" . "c:/Users/h3/web/xahlee_org/sex/condom_ad.html")

;("c:/Users/h3/web/xaharts_org/funny/4chan.html" . "c:/Users/h3/web/xahlee_org/funny/4chan.html")

;("c:/Users/h3/web/xahlee_org/Periodic_dosage_dir/american_socialism.html" . "c:/Users/h3/web/xaharts_org/funny/american_socialism.html")

;; no ad
;; c:/Users/h3/web/xaharts_org/funny/fredryk_phox.html
;; c:/Users/h3/web/xaharts_org/funny/breast_mouspad.html
;; c:/Users/h3/web/xaharts_org/funny/iMac_girl.html

;; i/mp/breast_mouse_pad3.jpg

   ))



(defvar ε-moved-from-paths nil "The first elements of ε-move-from-to-list.")
(setq ε-moved-from-paths (vconcat (mapcar (lambda ($x) (car $x) ) ε-move-from-to-list )) )

(defvar ε-backup-filename-suffix nil "")
(setq ε-backup-filename-suffix (concat "~s" (format-time-string "%Y%m%d_%H%M%S") "~"))



(defun get-new-fpath (@fPath @moveFromToList)
  "Return a new file full path for @fPath.
@moveFromToList is a alist."
  (let (($foundResult nil) ($i 0) ($len (length @moveFromToList)))
    ;; compare to each moved dir.
    (while (and (not $foundResult) (< $i $len))
      (when (string-match (concat "\\`" (regexp-quote (car (elt @moveFromToList $i)))) @fPath )
        (let (
              (fromDir (car (elt @moveFromToList $i)))
              (toDir (cdr (elt @moveFromToList $i))))
          (setq $foundResult (concat toDir (xah-substract-path @fPath fromDir)))))
      (setq $i (1+ $i)))
    (if $foundResult $foundResult @fPath )))
;; (get-new-fpath "c:/Users/h3/web/xahlee_org/emacs/th" ε-move-from-to-list)
;; (get-new-fpath "c:/Users/h3/web/xahlee_org/emacs_manual/elisp/tt" ε-move-from-to-list)
;; (get-new-fpath "c:/Users/h3/web/xahlee_org/emacs" ε-move-from-to-list)



(defun xahsite-fix-html-links (@host-file-path @move-from-to-list @moved-from-paths &optional @write-to-file-p @debug-p )
  "Process the file at xxxxxxx"
  (let (

        $p-match-b $p-match-e $p-hrefValue-b $p-hrefValue-e

        $p-beginTag-b ; begin tag begin. <
        $p-beginTag-e ; begin tag end. >

        $hrefValue
        $linkFileFullPath
        ($hostFileMoved-p nil)
        ($linkedFileMoved-p nil)
        $linkFragmentHead ;the part before “#”
        $linkFragmentTail ;the part after “#” including #
        $newHrefValue
        ($need-to-update-link-p nil)
        ($changeNecessary-p nil))

    ;; open file, search for a “href=”
    (when
        ;; (not (string-match-p "/xx" @host-file-path)) ; skip file whose name starts with “xx”
        t
      (when @debug-p (princ (format "\n▸@host-file-path 「%s」\n" @host-file-path )))
      (with-temp-buffer
        (insert-file-contents @host-file-path)
        (while
            (search-forward-regexp "\\(href\\|src\\)=\"\\(?3:[^\"]+?\\)\"" nil t)
          (setq $p-match-b (match-beginning 0))
          (setq $p-match-e (match-end 0))
          (setq $p-hrefValue-b (match-beginning 3))
          (setq $p-hrefValue-e (match-end 3))
          (setq $hrefValue (match-string 3))
          (save-excursion
            (search-backward "<" nil t)
            (setq $p-beginTag-b (point))
            (search-forward ">" nil t)
            (setq $p-beginTag-e (point)))

          ;; check if 「href="…"」 is inside <…>, and <…> doesn't contain any < or > character. If so, consider it's a link.
          (when (and (< $p-beginTag-b $p-match-b) (< $p-match-e $p-beginTag-e)
                     (not (string-match "<\\|>" (buffer-substring-no-properties (+ $p-beginTag-b 1) (- $p-beginTag-e 1))))
                     (not (string-match-p "\\`#" $hrefValue )) ; skip links that's only http://en.wikipedia.org/wiki/Fragment_identifier
                     )

            (when @debug-p (princ (format "▸$hrefValue 「%s」\n" $hrefValue )))

            (when  (xahsite-is-link-to-xahsite-p $hrefValue)
              (progn
                (let ((x (xah-html-split-uri-hashmark $hrefValue)))
                  (setq $linkFragmentHead (elt x 0))
                  (setq $linkFragmentTail (elt x 1)))

                (setq $linkFileFullPath
                      (if (xahsite-local-link-p $hrefValue)
                          (expand-file-name $linkFragmentHead (file-name-directory @host-file-path))
                        (xahsite-url-to-filepath $linkFragmentHead "addFileName")))
                (when @debug-p (princ (format "▸$linkFileFullPath 「%s」\n" $linkFileFullPath )))

                (setq $hostFileMoved-p (file-moved-p @host-file-path @moved-from-paths ))
                (setq $linkedFileMoved-p (file-moved-p $linkFileFullPath @moved-from-paths ))
                (setq $need-to-update-link-p (or $hostFileMoved-p $linkedFileMoved-p))

                (when @debug-p (princ (format "▸$hostFileMoved-p: 「%s」\n" $hostFileMoved-p )))
                (when @debug-p (princ (format "▸$linkedFileMoved-p: 「%s」\n" $linkedFileMoved-p )))
                (when @debug-p (princ (format "▸$need-to-update-link-p: 「%s」\n" $need-to-update-link-p )))

                (when t ; $need-to-update-link-p
                  (setq $newHrefValue
                        (concat (xahsite-filepath-to-href-value
                                 (if $linkedFileMoved-p
                                     (get-new-fpath $linkFileFullPath @move-from-to-list)
                                   $linkFileFullPath
                                   )
                                 (if $hostFileMoved-p
                                     (get-new-fpath @host-file-path @move-from-to-list)
                                   @host-file-path
                                   )) $linkFragmentTail))
                  (when @debug-p (princ (format "▸$newHrefValue 「%s」\n" $newHrefValue )))

                  (when (not (string= $hrefValue $newHrefValue))
                    (setq $changeNecessary-p t )
                    (progn
                      (princ (format "  「%s」\n" $hrefValue ))
                      (princ (format "  『%s』\n" (replace-regexp-in-string "^c:/Users/h3/" "~/" $newHrefValue))))
                    (when @write-to-file-p
                      (delete-region $p-hrefValue-b $p-hrefValue-e )
                      (goto-char $p-hrefValue-b)
                      (insert $newHrefValue))))))))

        (when $changeNecessary-p
          (princ (format "• %s\n" (replace-regexp-in-string "^c:/Users/h3/" "~/" @host-file-path)))
          (when @write-to-file-p
            (copy-file @host-file-path (concat @host-file-path ε-backup-filename-suffix) t) ; backup
            (write-region (point-min) (point-max) @host-file-path)))))))



(let ((outputBuffer "*xah sitemove output*" ))
  (with-output-to-temp-buffer outputBuffer

    (princ (format "-*- coding: utf-8 -*-
%s, xah site move link change results. Input path: 〔%s〕 \n\n" (xah-current-date-time-string) ε-inputPath))
    (if (file-regular-p ε-inputPath)
        (xahsite-fix-html-links ε-inputPath ε-move-from-to-list ε-moved-from-paths ε-writeToFile-p ε-debug-p)

      (progn
        (if (file-directory-p ε-inputPath)
            (mapc
             (lambda ($f)
               (xahsite-fix-html-links $f ε-move-from-to-list ε-moved-from-paths ε-writeToFile-p ε-debug-p))

             (seq-filter
              (lambda ($h) (not (xah-string-match-in-list-p $h ε-skip-list "match case" t)))

              (directory-files-recursively ε-inputPath "\\.html\\'\\|\\.xml\\'")

              ;;

              ))
          (error "Input path 「%s」 isn't a regular file nor dir." ε-inputPath))))
    (princ "Done")
    (switch-to-buffer outputBuffer)
    (html-mode)
    (highlight-lines-matching-regexp "\\`• " (quote hi-pink))))
