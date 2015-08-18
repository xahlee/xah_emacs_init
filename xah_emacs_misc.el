;; -*- coding: utf-8 -*-
;; part of Xah Lee's emacs init file.
;; 2007-06, 2011-01-24
;; Xah Lee,
;; ∑ http://xahlee.org/

; some functions personal to working on XahLee.org's website
; many of these opens a particular file and insert a string



(defun xah-set-input-method-to-chinese (φn)
  "Set input method to Chinese.

Normally, set to 'chinese-py.
C-u → set to 'chinese-tonepy-punct.
C-u 2 → set to 'chinese-py-b5."
(interactive "P")
  (cond
    ((equal φn nil)     ; universal-argument not called
     (set-input-method 'chinese-py))
    ((equal φn '(4))    ; C-u
     (set-input-method 'chinese-tonepy-punct))
    ((equal φn 2)       ; C-u 2
     (set-input-method 'chinese-py-b5))
    (t                                  ; all other cases
     (set-input-method 'chinese-py))))



(defun xah-list-matching-lines-no-regex ()
  "Show lines in the current buffer matching current word or text selection.
This command is the similar to `list-matching-lines'.
The differences are:
• The input of this command is the current word.
• If there is a text selection, that is used as input.
• The input is plain text, not regex."
  (interactive)
  (let (ξp1 ξp2 ξsearchStr )
    (if (use-region-p)
        (progn
          (setq ξp1 (region-beginning))
          (setq ξp2 (region-end))
          (setq ξsearchStr (buffer-substring-no-properties ξp1 ξp2)))
      (progn
        (setq ξsearchStr (word-at-point))))
    (list-matching-lines (regexp-quote ξsearchStr))))

(defun xah-make-lojban-entry ()
  "Insert a blank a-lojban-a-day HTML template in a paritcular file."
  (interactive)
(find-file "~/web/lojban/valsi_dikni/valsi_dikni.html")
(goto-char (point-min))
(re-search-forward "<!--t-->\n" nil t)
(insert (concat
"<div class=\"date\">"
(format-time-string "%Y-%m-%d")
"</div>
<p><b>renro</b> = throw = 丢 diu1</p>
<div class=\"def\">
renro:=x1 throws/launches/casts/hurls x2 to/at/in direction x3 (propulsion derives internally to x1)
</div>
<div class=\"ex\">
mi renro (le bolci ku) do = i throw ball to you = 我 丢 球qiu2 给gei3 你
</div>
<p>bolci = ball = 球. 给 = give.</p>
<pre class=\"linsi\">
• <a href=\"http://en.wiktionary.org/wiki/丢\">http://en.wiktionary.org/wiki/丢</a>
• <a href=\"http://en.wiktionary.org/wiki/给\">http://en.wiktionary.org/wiki/给</a>
</pre>
"))
(re-search-backward "<p><b>" nil t)
(re-search-forward "<p><b>" nil t))



(defvar xah-filelist 
  '(
    ("3emacs blog" . "~/web/ergoemacs_org/emacs/blog.html" )
    ("xah fly keys xs" . "~/web/ergoemacs_org/misc/ergoemacs_vi_mode.html")
    ("xahmodes xs" . "~/web/ergoemacs_org/emacs/xah_emacs_modes.html" )
    ("emacs xs" . "~/web/ergoemacs_org/emacs/emacs.html"))
  "alist for files i need to open frequently. Key is a short abbrev string, Value is file path string.")

(defun xah-open-file-fast ()
  "Prompt to open a file from `xah-filelist'.
URL `http://ergoemacs.org/emacs/emacs_hotkey_open_file_fast.html'
Version 2015-04-23"
  (interactive)
  (let ((ξabbrevCode
         (ido-completing-read "Open:" (mapcar (lambda (ξx) (car ξx)) xah-filelist))))
    (find-file (cdr (assoc ξabbrevCode xah-filelist)))))



(defun xah-sync-css ()
  "Save current file and copy to all other xahsite dirs."
  (interactive)
  (let* (
         (ξfromPath (buffer-file-name))
         (ξfromFileName (file-name-nondirectory ξfromPath ))
         ξtoPath
         )
    (save-buffer)
    (mapc
     (lambda (ξx)
       (progn
         (setq ξtoPath (concat (xahsite-url-to-filepath (format "http://%s/" ξx)) ξfromFileName))
         (when (not (string= ξfromPath ξtoPath ))
           (if (file-exists-p ξtoPath)
               (progn
                 (copy-file ξtoPath (concat ξtoPath "~" (format-time-string "%Y%m%d_%H%M%S") "~") "OK-IF-ALREADY-EXISTS") ;backup
                 (copy-file ξfromPath ξtoPath "OK-IF-ALREADY-EXISTS")
                 (message "wrote to 「%s」." ξtoPath))
             (progn (error "logic error. The file 「%s」 doesn't exist, it should already." ξtoPath)))))) [
       "ergoemacs.org"
       "wordyenglish.com"
       "xaharts.org"
       "xahlee.info"
       "xahlee.org"
       "xahmusic.org"
       "xahsl.org"
       ])))

(defun xah-cite ()
  "Change the file path under cursor into title and URL.

For example, this line
 /Users/xah/web/ergoemacs_org/emacs/emacs.html
becomes
 〈Xah's Emacs Tutorial〉 @ http://ergoemacs.org/emacs/emacs.html

The title came from HTML file's title tag.
File path must be a URL scheme, full path, or relative path. See: `xahsite-web-path-to-filepath'.

This is Xah Lee's personal command assuming a particular dir structure."
  (interactive)
  (let (
        ξp1 ξp2
        ξinputStr
        ξfile
        ξtitle
        (ξpathDelimitors "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·，。\\`") ; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
        )

    (if (use-region-p)
        (setq ξp1 (region-beginning) ξp2 (region-end))
      (let (ξp0)
        (setq ξp0 (point))
        (skip-chars-backward ξpathDelimitors)
        (setq ξp1 (point))
        (goto-char ξp0)
        (skip-chars-forward ξpathDelimitors)
        (setq ξp2 (point))))

    (setq ξinputStr (buffer-substring-no-properties ξp1 ξp2))
    (setq ξfile (xahsite-web-path-to-filepath ξinputStr))

    (if (file-exists-p ξfile)
        (progn
          (setq ξtitle
                (if (string-match-p ".+html\\'" ξfile)
                    (xah-html-get-html-file-title ξfile)
                  (file-name-nondirectory ξfile)))
          (setq ξtitle (xah-replace-pairs-in-string ξtitle [["&amp;" "&"] ["&lt;" "<"] ["&gt;" ">" ]]))

          (delete-region ξp1 ξp2)
          (insert ξtitle "\n" (xahsite-filepath-to-url ξfile)))
      (progn (user-error "file doesn't exist.")))))

(defun xah-copy-url-current-file ()
  "Put the current file's URL into the kill-ring."
  (interactive)
  (let (ξurl)
    (setq ξurl (xahsite-filepath-to-url (buffer-file-name)))
    (kill-new ξurl)
    (message "URL copied %s" ξurl)))



(defun compact-uncompact-block-chinese ()
  "Remove or add line ending chars on current text block.
 (text block is delimited by blank lines)
This command is similar to a toggle of `fill-paragraph'.
When there is a text selection, act on the region."
  (interactive)

  ;; This command symbol has a property “'stateIsCompact-p”.
  (let (currentStateIsCompact (bigFillColumnVal 4333999) (deactivate-mark nil))

    (save-excursion
      ;; Determine whether the text is currently compact.
      (setq currentStateIsCompact
            (if (eq last-command this-command)
                (get this-command 'stateIsCompact-p)
              (if (> (- (line-end-position) (line-beginning-position)) fill-column) t nil)))

      (if (use-region-p)
          (if currentStateIsCompact
              (fill-region (region-beginning) (region-end))
            (save-restriction
              (narrow-to-region (region-beginning) (region-end))
              (goto-char (point-min))
              (while (search-forward "\n" nil t) (replace-match "" nil t))))
        (if currentStateIsCompact
            (fill-paragraph nil)
          (let (ξp1 ξp2) ; ξp1 and ξp2 are beginning/end of text block
            (progn
              (if (re-search-backward "\n[ \t]*\n" nil "move")
                  (progn (re-search-forward "\n[ \t]*\n")
                         (setq ξp1 (point)))
                (setq ξp1 (point)))
              (if (re-search-forward "\n[ \t]*\n" nil "move")
                  (progn (re-search-backward "\n[ \t]*\n")
                         (setq ξp2 (point)))
                (setq ξp2 (point))))
            (save-restriction
              (narrow-to-region ξp1 ξp2)
              (goto-char (point-min))
              (while (search-forward "\n" nil t) (replace-match "" nil t))))))

      (put this-command 'stateIsCompact-p (if currentStateIsCompact nil t)))))

(defcustom xah-shell-abbrev-alist nil "alist of xah's shell abbrevs" :group 'xah)
(setq xah-shell-abbrev-alist
      '(
        ("rsync1" . "rsync -z -r -v -t --delete --chmod=Dugo+x --chmod=ugo+r --exclude='*~' --exclude='.bash_history' --exclude='logs/' --exclude='xahbackup/' --exclude='.git/*' --rsh='ssh -l u40651120' ~/web/ u40651120@s168753655.onlinehome.us:~/")
        ("ssh" . "ssh -l u40651120 xahlee.org ")
        ("img1" . "convert -quality 85% ")
        ("imgScale" . "convert -scale 50% -quality 85% ")
        ("img256" . "convert +dither -colors 256 ")
        ("imgBatch" . "find . -name \"*png\" | xargs -l -i basename \"{}\" \".png\" | xargs -l -i  convert -quality 85% \"{}.png\" \"{}.jpg\"")
        ("img-bmp2png" . "find . -name \"*bmp\" | xargs -l -i basename \"{}\" \".bmp\" | xargs -l -i  convert \"{}.bmp\" \"{}.png\"")

        ("grep" . "grep -r -F 'xxx' --include='*html' ~/web")
        ("firefox" . "setsid firefox &")

        ("delete empty file" . "find . -type f -empty")
        ("chmod file" . "find . -type f -exec chmod 644 {} ';'")
        ("delete emacs backup~" . "find . -name \"*~\" -delete")
        ("find empty dir" . "find . -depth -empty -type d")
        ("delete empty dir" . "find . -depth -empty -type d -delete")
        ("chmod2" . "find . -type d -exec chmod 755 {} ';'")
        ("lynx" . "lynx -dump -assume_local_charset=utf-8 -display_charset=utf-8 -width=100")
        ("viewp" . "setsid feh --randomize --recursive --auto-zoom --action \"gvfs-trash '%f'\" --geometry 1600x980+10+10 .")

        ("clojure" . "java -cp /home/xah/apps/clojure-1.6.0/clojure-1.6.0.jar clojure.main")
        ("multimedia keys" . "<kbd>◼</kbd>, <kbd>⏯</kbd>, <kbd>⏮</kbd>, <kbd>⏭</kbd>, <kbd>🔇</kbd>")))

(defun xah-shell-commands (φcmd-abbrev)
  "insert shell command from a list of abbrevs.

URL `http://ergoemacs.org/misc/emacs_abbrev_shell_elisp.html'
version 2015-02-05"
  (interactive
   (list
    (ido-completing-read "shell abbrevs:" (mapcar (lambda (x) (car x)) xah-shell-abbrev-alist) "PREDICATE" "REQUIRE-MATCH")))
  (progn
    (insert (cdr (assoc φcmd-abbrev xah-shell-abbrev-alist)))))

(defun xah-to-xah-elisp-mode  ()
  "redo my tutorial's code elisp markup"
  (interactive)
  (xah-make-backup)
  (goto-char 1)
  (while
      (search-forward "<pre class=\"elisp\">" nil "NOERROR")
    (replace-match "<pre class=\"emacs-lisp\">" "FIXEDCASE" "LITERAL" )

    (let* (
           ( ξxx (xah-html-get-precode-langCode))
           (langCode (elt ξxx 0))
           (ξp1 (elt ξxx 1))
           (ξp2 (elt ξxx 2)))

      (xah-html-remove-span-tag-region ξp1 ξp2)
      (goto-char ξp1)
      (xah-html-htmlize-precode xah-html-lang-name-map))

    ;; (call-interactively 'xah-html-htmlize-or-de-precode)
    ))

(defun xah-slide-show ()
  "start external program to do slideshow of current dir.
Linux only. Requires 「feh」 image viewer.
"
  (interactive)
  (let ()
(shell-command (format "setsid feh --randomize --recursive --auto-zoom --action \"gvfs-trash '%%f'\" --geometry 1600x1000 '%s'" (expand-file-name default-directory)) )
  ))

(defun xah-remove-paypal-unimportant-text ()
  "..."
  (interactive)
  (let ()
(xah-replace-pairs-region 1 (point-max)
 [
["Dear Xah Lee," ""]
["Hello Xah Lee," ""]
["To see all the transaction details, please log into your PayPal account." ""]
["Thanks for using PayPal. You can now ship any items. To see all the transaction details, log in to your PayPal account." ""]
["It may take a few moments for this transaction to appear in your account." ""]
["Seller Protection - Not Eligible" ""]
["It may take a few moments for this transaction to appear in your account." ""]
["Questions? Go to the Help Center at: www.paypal.com/help." ""]
["Questions? Visit the Help Center at: www.paypal.com/help." ""]
["Thanks for using PayPal – the safer, easier way to pay and get paid online." ""]
["Please do not reply to this email. This mailbox is not monitored and you will not receive a response. For assistance, log in to your PayPal account and click Help in the top right corner of any PayPal page." ""]
["You can receive plain text emails instead of HTML emails. To change your Notifications preferences, log in to your account, go to your Profile, and click My settings." ""]
["Please keep this number for future reference, as your customer doesn't have a PayPal Transaction ID for this payment." ""]
["Lift your withdrawal and receiving limits. Log in to your PayPal account and click View limits on your Account Overview page." ""]
["Once the money's there you can:
Spend the money online at thousands of stores that accept PayPal.
Transfer it to your bank account (takes 2-3 days).
Get a PayPal Debit MasterCard." ""]
["Don't see the money in your account?" ""]
["Don’t worry - sometimes it just takes a few minutes for it to show up." ""]
["Don't worry - sometimes it just takes a few minutes for it to show up." ""]
["Sincerely,
PayPal" ""]
["Help Center:
https://www.paypal.com/us/cgi-bin/helpweb?cmd=_help" ""]
["Resolution Center:
https://www.paypal.com/us/cgi-bin/?cmd=_complaint-view
" ""]
["Security Center:
https://www.paypal.com/us/security" ""]
["Please don't reply to this email. It'll just confuse the computer that sent it and you won't get a response." ""]
["This email was sent by an automated system, so if you reply, nobody will see it. To get in touch with us, log in to your account and click \"Contact Us\" at the bottom of any page." ""]
["Copyright © 2014 PayPal, Inc. All rights reserved. PayPal is located at 2211 N. First St., San Jose, CA 95131." ""]
["Instructions to merchant:
The buyer hasn't entered any instructions." ""]
["Instructions from buyer:
None provided" ""]
["----------------------------------------------------------------" ""]
                           ]
                          )
(xah-replace-regexp-pairs-region 1 (point-max)
 [

["Get the details
https://www.paypal.com/us/cgi-bin/\\?cmd=_view-a-trans&id=\\([0-9a-zA-Z]\\{17\\}\\)"
""]

["Important note: \\([ a-zA-Z,]+?\\) has provided an unconfirmed address. Please check the Transaction Details page for this payment to find out whether you will be covered by PayPal Seller Protection."
 ""]

["PPID PP\\([0-9]\\{3,4\\}\\) - \\([0-9a-fA-F]\\{12,13\\}\\)" ""]

["PayPal Email ID +PP\\([0-9]\\{3,4\\}\\) - \\([0-9a-fA-F]\\{12,13\\}\\)" ""]

]
"FIXEDCASE" "LITERAL")

(let* (
         (ξp1 (point-min) )
         (ξp2 (point-max) )
         )
    (save-excursion
      (save-restriction
        (narrow-to-region ξp1 ξp2)
        (progn
          (goto-char (point-min))
          (while (search-forward-regexp "[ \t]+\n" nil "noerror")
            (replace-match "\n") ))
        (progn
          (goto-char (point-min))
          (while (search-forward-regexp "\n\n\n+" nil "noerror")
            (replace-match "\n\n") )) )) )
))

(defun xah-replace-BOM-mark-etc ()
  "Query replace Unicode some invisible Unicode chars.
The chars to be searched are:
 RIGHT-TO-LEFT MARK 8207 x200f
 ZERO WIDTH NO-BREAK SPACE 65279 xfeff

start on cursor position to end.
    "
  (interactive)
  (let ()
    (query-replace-regexp "\u200f\\|\ufeff" "")
    ))

(defun xah-show-hexadecimal-value ()
  "Prints the decimal value of a hexadecimal string under cursor.
TODO: 2014-05-23 doesn't work. something's broken.

Samples of valid input:

  ffaf
  ffff
  0xffff
  #xffff
  FFFF
  0xFFFF
  #xFFFF

Test cases
  64*0xc8+#x12c 190*0x1f4+#x258
  100 200 300   400 500 600"
  (interactive )

  (let (ξinputStr ξtempStr ξp1 ξp2
                 (case-fold-search t) )
    (save-excursion
      ;; (skip-chars-backward "0123456789abcdef")
      ;; (search-backward-regexp "[[:xdigit:]]+" nil t)
      (search-backward-regexp "[0123456789abcdef]+" nil t)
      (setq ξp1 (point) )
      (search-forward-regexp "[0123456789abcdef]+" nil t)
      (setq ξp2 (point) )

      (setq ξinputStr (buffer-substring-no-properties ξp1 ξp2) )

      (let ((case-fold-search nil) )
        (setq ξtempStr (replace-regexp-in-string "\\`0x" "" ξinputStr )) ; C, Perl, …
        (setq ξtempStr (replace-regexp-in-string "\\`#x" "" ξtempStr )) ; elisp …
        (setq ξtempStr (replace-regexp-in-string "\\`#" "" ξtempStr ))  ; CSS …
        )

      ;; (message "Hex 「%s」 is 「%d」" ξtempStr (string-to-number ξtempStr 16))
      (message "input 「%s」 Hex 「%s」 is 「%d」" ξinputStr ξtempStr (string-to-number ξtempStr 16)))))



;; (defun xah-dired-sort-time-accessed ()
;;   "DOCSTRING"
;;   (interactive)
;;   (let ()
;;     (setq dired-listing-switches "-Al --si --time-style long-iso")
;;     (dired-sort-other "-Al --si --time-style long-iso -t") ;;by mod time
;;     (dired-sort-other "-Al --si --time-style long-iso -tc") ;;by file metadata mod time
;;     (dired-sort-other "-Al --si --time-style long-iso -tu") ;;by access time
;;     (dired-sort-other "-Al --si --time-style long-iso -S") ;;by file size
;;     (dired-sort-other "-Al --si --time-style long-iso -X") ;; by by extension
;;     ))

(defun xah-unfontify-region-or-buffer ()
  "Unfontify text selection or buffer."
  (interactive)
  (if (use-region-p)
      (font-lock-unfontify-region (region-beginning) (region-end))
    (font-lock-unfontify-buffer)))

;; (defun overlay-key-binding (key)
;; 2014-10-11 from http://stackoverflow.com/questions/18801018/how-to-find-in-which-map-a-key-binding-is-from-programatically-in-emacs
;;   (mapcar (lambda (keymap) (lookup-key keymap key))
;;           (cl-remove-if-not
;;            #'keymapp
;;            (mapcar (lambda (overlay)
;;                      (overlay-get overlay 'keymap))
;;                    (overlays-at (point))))))

;; (defun xah-find-keybinding-source (φkey)
;; " 2014-10-11 from http://stackoverflow.com/questions/18801018/how-to-find-in-which-map-a-key-binding-is-from-programatically-in-emacs"
;;   (list
;;    (minor-mode-key-binding φkey)
;;    (local-key-binding φkey)
;;    (global-key-binding φkey)
;;    ;; (overlay-key-binding φkey)
;;    ))

(defun xxlocate-key-binding (key)
  "Determine in which keymap KEY is defined.
2014-10-11 http://emacs.stackexchange.com/questions/653/how-can-i-find-out-in-which-keymap-a-key-is-bound"
  (interactive "kPress key: ")
  (let ((ret
         (list
          (key-binding-at-point key)
          (minor-mode-key-binding key)
          (local-key-binding key)
          (global-key-binding key))))
    (when (called-interactively-p 'any)
      (message "At Point: %s\nMinor-mode: %s\nLocal: %s\nGlobal: %s"
               (or (nth 0 ret) "")
               (or (mapconcat (lambda (x) (format "%s: %s" (car x) (cdr x)))
                              (nth 1 ret) "\n             ")
                   "")
               (or (nth 2 ret) "")
               (or (nth 3 ret) "")))
    ret))

(defun xxkey-binding-at-point (key)
"2014-10-11
http://emacs.stackexchange.com/questions/653/how-can-i-find-out-in-which-keymap-a-key-is-bound"
  (mapcar (lambda (keymap) (lookup-key keymap key))
          (cl-remove-if-not
           #'keymapp
           (append
            (mapcar (lambda (overlay)
                      (overlay-get overlay 'keymap))
                    (overlays-at (point)))
            (get-text-property (point) 'keymap)
            (get-text-property (point) 'local-map)))))

(defvar gitgrep-history nil)

(defun gitgrep (φsearch-string)
"call git grep to search symbols in a project.

2014-11-19 by “Left Right” https://plus.google.com/113859563190964307534/posts/CyEsoyhkTVe
"
  (interactive
   (let ((ξsym (thing-at-point 'symbol)))
     (list
      (completing-read
       "String to search for: "
       (list ξsym
             (buffer-name)
             (buffer-file-name))
       'identity nil ξsym gitgrep-history ξsym))))
  (grep (format "git --no-pager grep -P -n '%s'" φsearch-string)))
