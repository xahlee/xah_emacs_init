;; -*- coding: utf-8 -*-
;; part of Xah Lee's emacs init file.
;; 2007-06
;; Xah Lee,
;; ∑ http://xahlee.org/



(defun xah-select-text-in-html-bracket ()
  "Select text between <…> or >…<."
  (interactive)
  (let (-p0 -p1< -p1> -p2< -p2>
           distance-p1<
           distance-p1>
           )
    (setq -p0 (point))
    (search-backward "<" nil "NOERROR" )
    (setq -p1< (point))
    (goto-char -p0)
    (search-backward ">" nil "NOERROR" )
    (setq -p1> (point))
    (setq distance-p1< (abs (- -p0 -p1<)))
    (setq distance-p1> (abs (- -p1> -p0)))
    (if (< distance-p1< distance-p1>)
        (progn
          (goto-char -p0)
          (search-forward ">" nil "NOERROR" )
          (setq -p2> (point))
          (goto-char (1+ -p1<))
          (set-mark (1- -p2>)))
      (progn
        (goto-char -p0)
        (search-forward "<" nil "NOERROR" )
        (setq -p2< (point))
        (goto-char (1+ -p1>))
        (set-mark (1- -p2<))))))

(defun xah-open-file-from-clipboard ()
  "Open the file path from OS's clipboard.
The clipboard should contain a file path or url to xah site. Open that file in emacs."
  (interactive)
  (let (
        (-inputStr
         (with-temp-buffer
           (yank)
           (buffer-string)))
        -fpath
        )

    (if (string-match-p "\\`http://" -inputStr)
        (progn
          (setq -fpath (xahsite-url-to-filepath -inputStr "addFileName"))
          (if (file-exists-p -fpath)
              (progn (find-file -fpath))
            (progn (error "file doesn't exist 「%s」" -fpath))))
      (progn ; not starting “http://”
        (setq -inputStr (xah-remove-uri-fragment -inputStr))
        (setq -fpath (xahsite-web-path-to-filepath -inputStr default-directory))
        (if (file-exists-p -fpath)
            (progn (find-file -fpath))
          (progn (user-error "file doesn't exist 「%s」" -fpath)))))))

(defun xah-open-file-at-cursor ()
  "Open the file path under cursor.
If there is text selection, uses the text selection for path.
If the path starts with “http://”, open the URL in browser.
Input path can be {relative, full path, URL}.
Path may have a trailing “:‹n›” that indicates line number. If so, jump to that line number.
If path does not have a file extention, automatically try with “.el” for elisp files.
This command is similar to `find-file-at-point' but without prompting for confirmation.

URL `http://ergoemacs.org/emacs/emacs_open_file_path_fast.html'
Version 2016-06-14"
  (interactive)
  (let* ((-inputStr (if (use-region-p)
                 (buffer-substring-no-properties (region-beginning) (region-end))
               (let (-p0 -p1 -p2
                         (-charSkipRegex "^  \"\t\n`'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›❮❯·。\\`"))
                 (setq -p0 (point))
                 ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
                 (skip-chars-backward -charSkipRegex)
                 (setq -p1 (point))
                 (goto-char -p0)
                 (skip-chars-forward -charSkipRegex)
                 (setq -p2 (point))
                 (goto-char -p0)
                 (buffer-substring-no-properties -p1 -p2))))
         (-path (replace-regexp-in-string ":\\'" "" -inputStr)))
    (if (string-match-p "\\`https?://" -path)
        (browse-url -path)
      (progn ; not starting “http://”
        (if (string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\'" -path)
            (progn
              (let (
                    (-fpath (match-string 1 -path))
                    (-line-num (string-to-number (match-string 2 -path))))
                (if (file-exists-p -fpath)
                    (progn
                      (find-file -fpath)
                      (goto-char 1)
                      (forward-line (1- -line-num)))
                  (progn
                    (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" -fpath))
                      (find-file -fpath))))))
          (progn
            (if (file-exists-p -path)
                (find-file -path)
              (if (file-exists-p (concat -path ".el"))
                  (find-file (concat -path ".el"))
                (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" -path))
                  (find-file -path ))))))))))

(defun xah-open-file-path-under-cursor ()
  "Open the file path under cursor.
If there is text selection, use the text selection for path.
If path starts with “http://”, launch browser vistiting that URL, or open the corresponding file, if it's xah site.

Input path can be {relative, full path, URL}. See: `xahsite-web-path-to-filepath' for types of paths supported.

Version 2015-06-12"
  (interactive)
  (let* (
         (-inputStr1
          (xah-remove-uri-fragment
           (if (use-region-p)
               (buffer-substring-no-properties (region-beginning) (region-end))
             (let (-p0 -p1 -p2
                       (-charSkipRegex "^  \"\t\n`'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›❮❯·。\\`"))
               (setq -p0 (point))
               ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
               (skip-chars-backward -charSkipRegex)
               (setq -p1 (point))
               (goto-char -p0)
               (skip-chars-forward -charSkipRegex)
               (setq -p2 (point))
               (goto-char -p0)
               (buffer-substring-no-properties -p1 -p2)))))
         (-inputStr2 (replace-regexp-in-string ":\\'" "" -inputStr1))
         -p
         )
    (if (string-equal -inputStr2 "")
        (progn (user-error "No path under cursor" ))
      (progn
        ;; convenience. if the input string start with a xah domain name, make it a url string
        (setq -p
              (cond
               ((string-match "\\`//" -inputStr2 ) (concat "http:" -inputStr2)) ; relative http protocol, used in css
               ((string-match "\\`ergoemacs\\.org" -inputStr2 ) (concat "http://" -inputStr2))
               ((string-match "\\`wordyenglish\\.com" -inputStr2 ) (concat "http://" -inputStr2))
               ((string-match "\\`xaharts\\.org" -inputStr2 ) (concat "http://" -inputStr2))
               ((string-match "\\`xahlee\\.info" -inputStr2 ) (concat "http://" -inputStr2))
               ((string-match "\\`xahlee\\.org" -inputStr2 ) (concat "http://" -inputStr2))
               ((string-match "\\`xahmusic\\.org" -inputStr2 ) (concat "http://" -inputStr2))
               ((string-match "\\`xahporn\\.org" -inputStr2 ) (concat "http://" -inputStr2))
               ((string-match "\\`xahsl\\.org" -inputStr2 ) (concat "http://" -inputStr2))
               (t -inputStr2)))
        (if (string-match-p "\\`https?://" -p)
            (if (xahsite-url-is-xah-website-p -p)
                (let ((-fp (xahsite-url-to-filepath -p )))
                  (if (file-exists-p -fp)
                      (find-file -fp)
                    (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" -fp)) (find-file -fp))))
              (browse-url -p))
          (progn ; not starting “http://”
            (let ((-fff (xahsite-web-path-to-filepath -p default-directory)))
              (if (file-exists-p -fff)
                  (progn (find-file -fff))
                (if (file-exists-p (concat -fff ".el"))
                    (progn (find-file (concat -fff ".el")))
                  (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" -fff)) (find-file -fff )))))))))))



(defun xah-toggle-margin-right ()
  "Toggle the right margin between `fill-column' or window width.
This command is convenient when reading novel, documentation."
  (interactive)
  (if (null (cdr (window-margins)))
      (set-window-margins nil 0 (- (window-body-width) fill-column))
    (set-window-margins nil 0 0)))

(defun xah-toggle-line-spacing ()
  "Toggle line spacing between no extra space to extra half line height.
URL `http://ergoemacs.org/emacs/emacs_toggle_line_spacing.html'
Version 2015-12-17"
  (interactive)
  (if (null line-spacing)
      (setq line-spacing 0.5) ; add 0.5 height between lines
    (setq line-spacing nil)   ; no extra heigh between lines
    )
  (redraw-frame (selected-frame)))

(defun xah-toggle-read-novel-mode ()
  "Setup current buffer to be suitable for reading long novel/article text.

• Line wrap at word boundaries.
• Set a right margin.
• line spacing is increased.
• variable width font is used.

Call again to toggle back.
URL `http://ergoemacs.org/emacs/emacs_novel_reading_mode.html'
Version 2016-01-16"
  (interactive)
  (if (null (get this-command 'state-on-p))
      (progn
        (set-window-margins
         nil 0
         (if (> fill-column (window-body-width))
             0
           (progn
             (- (window-body-width) fill-column))))
        (variable-pitch-mode 1)
        (setq line-spacing 0.4)
        (setq word-wrap t)
        (put this-command 'state-on-p t))
    (progn
      (set-window-margins nil 0 0)
      (variable-pitch-mode 0)
      (setq line-spacing nil)
      (setq word-wrap nil)
      (put this-command 'state-on-p nil)))
  (redraw-frame (selected-frame)))



(defun xah-set-input-method-to-chinese (*n)
  "Set input method to Chinese.

Normally, set to 'chinese-py.
C-u → set to 'chinese-tonepy-punct.
C-u 2 → set to 'chinese-py-b5."
(interactive "P")
  (cond
    ((equal *n nil)     ; universal-argument not called
     (set-input-method 'chinese-py))
    ((equal *n '(4))    ; C-u
     (set-input-method 'chinese-tonepy-punct))
    ((equal *n 2)       ; C-u 2
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
  (let (-p1 -p2 -searchStr )
    (if (use-region-p)
        (progn
          (setq -p1 (region-beginning))
          (setq -p2 (region-end))
          (setq -searchStr (buffer-substring-no-properties -p1 -p2)))
      (progn
        (setq -searchStr (word-at-point))))
    (list-matching-lines (regexp-quote -searchStr))))

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
  (let ((-abbrevCode
         (ido-completing-read "Open:" (mapcar (lambda (-x) (car -x)) xah-filelist))))
    (find-file (cdr (assoc -abbrevCode xah-filelist)))))



(defun xah-sync-css ()
  "Save current file and copy to all other xahsite dirs.
Version 2015-08-31"
  (interactive)
  (let* (
         (-fromPath (buffer-file-name))
         (-fromFileName (file-name-nondirectory -fromPath ))
         -toPath
         )
    (save-buffer)
    (mapc
     (lambda (-x)
       (progn
         (setq -toPath (concat (xahsite-url-to-filepath (format "http://%s/" -x)) -fromFileName))
         (when (not (string= -fromPath -toPath ))
           (if (file-exists-p -toPath)
               (progn
                 (copy-file -toPath (concat -toPath "~" (format-time-string "%Y%m%d_%H%M%S") "~") "OK-IF-ALREADY-EXISTS") ;backup
                 (copy-file -fromPath -toPath "OK-IF-ALREADY-EXISTS")
                 (message "wrote to 「%s」." -toPath))
             (progn (error "logic error. The file 「%s」 doesn't exist, it should already." -toPath)))))) [
       "ergoemacs.org"
       "wordyenglish.com"
       "xaharts.org"
       "xahlee.info"
       "xahlee.org"
       "xahmusic.org"
       "xahsl.org"
       "xahporn.org"
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
        -p1 -p2
        -inputStr
        -file
        -title
        (-pathDelimitors "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·，。\\`") ; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
        )

    (if (use-region-p)
        (setq -p1 (region-beginning) -p2 (region-end))
      (let (-p0)
        (setq -p0 (point))
        (skip-chars-backward -pathDelimitors)
        (setq -p1 (point))
        (goto-char -p0)
        (skip-chars-forward -pathDelimitors)
        (setq -p2 (point))))

    (setq -inputStr (buffer-substring-no-properties -p1 -p2))
    (setq -file (xahsite-web-path-to-filepath -inputStr))

    (if (file-exists-p -file)
        (progn
          (setq -title
                (if (string-match-p ".+html\\'" -file)
                    (xah-html-get-html-file-title -file)
                  (file-name-nondirectory -file)))
          (setq -title (xah-replace-pairs-in-string -title [["&amp;" "&"] ["&lt;" "<"] ["&gt;" ">" ]]))

          (delete-region -p1 -p2)
          (insert -title "\n" (xahsite-filepath-to-url -file)))
      (progn (user-error "file doesn't exist.")))))

(defun xah-copy-url-current-file ()
  "Put the current file's URL into the kill-ring."
  (interactive)
  (let (-url)
    (setq -url (xahsite-filepath-to-url (buffer-file-name)))
    (kill-new -url)
    (message "URL copied %s" -url)))



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
          (let (-p1 -p2) ; -p1 and -p2 are beginning/end of text block
            (progn
              (if (re-search-backward "\n[ \t]*\n" nil "move")
                  (progn (re-search-forward "\n[ \t]*\n")
                         (setq -p1 (point)))
                (setq -p1 (point)))
              (if (re-search-forward "\n[ \t]*\n" nil "move")
                  (progn (re-search-backward "\n[ \t]*\n")
                         (setq -p2 (point)))
                (setq -p2 (point))))
            (save-restriction
              (narrow-to-region -p1 -p2)
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

        ("grep" . "grep -r -F \"xxx\" --include='*html' ~/web")
        ("firefox" . "setsid firefox &")

        ("delete empty file" . "find . -type f -empty")
        ("chmod file" . "find . -type f -exec chmod 644 {} ';'")
        ("delete emacs backup~" . "find ~/web/ -name \"*~\" -delete")
        ("find empty dir" . "find . -depth -empty -type d")
        ("delete empty dir" . "find . -depth -empty -type d -delete")

        ("empty trash" . "rm -r /home/xah/.local/share/Trash")

        ("chmod2" . "find . -type d -exec chmod 755 {} ';'")
        ("lynx" . "lynx -dump -assume_local_charset=utf-8 -display_charset=utf-8 -width=100")
        ("viewp" . "setsid feh --randomize --recursive --auto-zoom --action \"gvfs-trash '%f'\" --geometry 1600x980+10+10 .")

        ("clojure" . "java -cp /home/xah/apps/clojure-1.6.0/clojure-1.6.0.jar clojure.main")
        ("multimedia keys" . "<kbd>◼</kbd>, <kbd>⏯</kbd>, <kbd>⏮</kbd>, <kbd>⏭</kbd>, <kbd>🔇</kbd>")))

(defun xah-shell-commands (*cmd-abbrev)
  "insert shell command from a list of abbrevs.

URL `http://ergoemacs.org/misc/emacs_abbrev_shell_elisp.html'
version 2015-02-05"
  (interactive
   (list
    (ido-completing-read "shell abbrevs:" (mapcar (lambda (x) (car x)) xah-shell-abbrev-alist) "PREDICATE" "REQUIRE-MATCH")))
  (progn
    (insert (cdr (assoc *cmd-abbrev xah-shell-abbrev-alist)))))

(defun xah-to-xah-elisp-mode  ()
  "redo my tutorial's code elisp markup"
  (interactive)
  (xah-make-backup)
  (goto-char 1)
  (while
      (search-forward "<pre class=\"elisp\">" nil "NOERROR")
    (replace-match "<pre class=\"emacs-lisp\">" "FIXEDCASE" "LITERAL" )

    (let* (
           ( -xx (xah-html-get-precode-langCode))
           (langCode (elt -xx 0))
           (-p1 (elt -xx 1))
           (-p2 (elt -xx 2)))

      (xah-html-remove-span-tag-region -p1 -p2)
      (goto-char -p1)
      (xah-html-htmlize-precode xah-html-lang-name-map))))

(defun xah-slide-show ()
  "start external program to do slideshow of current dir.
Linux only. Requires 「feh」 image viewer.
Version 2015-10-20"
  (interactive)
  (progn
    (shell-command (format "setsid feh --randomize --recursive --auto-zoom --action \"gvfs-trash '%%f'\" --geometry 1600x1000 '%s'" (expand-file-name default-directory)))
    ;; (shell-command (format "gthumb --slideshow ." (expand-file-name default-directory)) )
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
["Copyright © 1999-2016 PayPal. All rights reserved." ""]
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
         (-p1 (point-min) )
         (-p2 (point-max) )
         )
    (save-excursion
      (save-restriction
        (narrow-to-region -p1 -p2)
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
  "Query replace some invisible Unicode chars.
The chars to be searched are:
 ZERO WIDTH NO-BREAK SPACE (codepoint 65279, #xfeff)
 RIGHT-TO-LEFT MARK (codepoint 8207, #x200f)
 RIGHT-TO-LEFT OVERRIDE (codepoint 8238, #x202e)

Search begins at cursor position. (respects `narrow-to-region')

This is useful for text copied from twitter or Google Plus, because they often contain BOM mark. See URL `http://xahlee.info/comp/unicode_BOM_byte_orde_mark.html'

URL `http://ergoemacs.org/emacs/elisp_unicode_replace_invisible_chars.html'
Version 2015-10-25"
  (interactive)
  (query-replace-regexp "\u200f\\|\u202e\\|\ufeff" ""))

(defun xah-replace-BOM-mark-dir ()
  "temp hack. replace some invisible Unicode chars.
see `xah-replace-BOM-mark-etc'
Version 2015-10-11"
  (interactive)
  (require 'xah-find)
  (let (-dir)
    (setq -dir (ido-read-directory-name "Directory: " default-directory default-directory "MUSTMATCH"))
    (xah-find-replace-text (char-to-string 65279) "" -dir "\\.html\\'" t t t t)))

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

  (let (-inputStr -tempStr -p1 -p2
                 (case-fold-search t) )
    (save-excursion
      ;; (skip-chars-backward "0123456789abcdef")
      ;; (search-backward-regexp "[[:xdigit:]]+" nil t)
      (search-backward-regexp "[0123456789abcdef]+" nil t)
      (setq -p1 (point) )
      (search-forward-regexp "[0123456789abcdef]+" nil t)
      (setq -p2 (point) )

      (setq -inputStr (buffer-substring-no-properties -p1 -p2) )

      (let ((case-fold-search nil) )
        (setq -tempStr (replace-regexp-in-string "\\`0x" "" -inputStr )) ; C, Perl, …
        (setq -tempStr (replace-regexp-in-string "\\`#x" "" -tempStr )) ; elisp …
        (setq -tempStr (replace-regexp-in-string "\\`#" "" -tempStr ))  ; CSS …
        )

      ;; (message "Hex 「%s」 is 「%d」" -tempStr (string-to-number -tempStr 16))
      (message "input 「%s」 Hex 「%s」 is 「%d」" -inputStr -tempStr (string-to-number -tempStr 16)))))



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

;; (defun xah-find-keybinding-source (*key)
;; " 2014-10-11 from http://stackoverflow.com/questions/18801018/how-to-find-in-which-map-a-key-binding-is-from-programatically-in-emacs"
;;   (list
;;    (minor-mode-key-binding *key)
;;    (local-key-binding *key)
;;    (global-key-binding *key)
;;    ;; (overlay-key-binding *key)
;;    ))

(defvar gitgrep-history nil)

(defun gitgrep (*search-string)
"call git grep to search symbols in a project.

2014-11-19 by “Left Right” https://plus.google.com/113859563190964307534/posts/CyEsoyhkTVe
"
  (interactive
   (let ((-sym (thing-at-point 'symbol)))
     (list
      (completing-read
       "String to search for: "
       (list -sym
             (buffer-name)
             (buffer-file-name))
       'identity nil -sym gitgrep-history -sym))))
  (grep (format "git --no-pager grep -P -n '%s'" *search-string)))

(defun xah-toggle-background-color ()
  "Toggle background color between seashell and honeydew.
URL `http://ergoemacs.org/emacs/elisp_toggle_command.html'
Version 2015-12-17"
  (interactive)
  ;; use a property “state”. Value is t or nil
  (if (get 'xah-toggle-background-color 'state)
      (progn
        (set-background-color "seashell")
        (put 'xah-toggle-background-color 'state nil))
    (progn
      (set-background-color "honeydew")
      (put 'xah-toggle-background-color 'state t))))

(defun xah-cycle-background-color (*n)
  "Cycle background color among a preset list.

If `universal-argument' is called first, cycle n steps. Default is 1 step.

URL `http://ergoemacs.org/emacs/elisp_toggle_command.html'
Version 2015-12-17"
  (interactive "p")
  ;; uses a property “state”. Value is a integer.
  (let* (
         (-values ["cornsilk" "pale green" "pale turquoise" "thistle" "seashell" "honeydew"])
         (-index-before
          (if (get 'xah-cycle-background-color 'state)
              (get 'xah-cycle-background-color 'state)
            0))
         (-index-after (% (+ -index-before (length -values) *n) (length -values)))
         (-next-value (aref -values -index-after)))

    (put 'xah-cycle-background-color 'state -index-after)

    (set-background-color -next-value)
    (message "background color changed to %s" -next-value)))



(defun xah-browse-url-of-buffer ()
  "Similar to `browse-url-of-buffer' but visit xahlee.org.

save the file first.
Then, if `universal-argument' is called, visit the corresponding xahsite URL.
For example, if current buffer is of this file:
 ~/web/xahlee_info/index.html
then after calling this function,
default browser will be launched and opening this URL:
 http://xahlee.info/index.html
version 2016-06-12"
  (interactive)
  (let (-url)
    (setq -url
          (if current-prefix-arg
              (xahsite-filepath-to-url (buffer-file-name))
            (buffer-file-name)))

    (when (buffer-modified-p ) 
      (xah-clean-whitespace-and-save (point-min) (point-max))
      (save-buffer))
    (message "browsing %s" -url)
    (cond
     ((string-equal system-type "windows-nt") ; Windows
      (when (string-match "^c:/" -url) (setq -url (concat "file:///" -url)))
      (browse-url -url))
     ((string-equal system-type "gnu/linux")
      (let ( (process-connection-type nil))
        (start-process "" nil "setsid" "firefox" (concat "file://" buffer-file-name )))
      ;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. ⁖ with nautilus
      )
     ;; ((string-equal system-type "gnu/linux")
     ;;  (start-process "xahbrowse"
     ;;                 nil "setsid"
     ;;                 "firefox"
     ;;                 (concat "file://" buffer-file-name )))
     ((string-equal system-type "darwin") ; Mac
      (browse-url -url )))))



(defun list-non-matching-lines ()
  "Show lines *not* matching the regexp."
  (interactive)
  (let ((orig-buf (current-buffer))
        (new-buf "*List Non-matching Lines*"))
    (switch-to-buffer new-buf nil :force-same-window)
    (insert-buffer-substring orig-buf)
    (goto-char (point-min))
    (let ((inhibit-read-only t)) ; Always make the buffer editable
      (call-interactively #'flush-lines))
    (special-mode)))
 ; Make the new buffer read-only; also allowing bindings like `q'




(require 'cl-lib)

(defun invert-occur (regexp)
  "Find all lines not matching REGEXP."
  (interactive "sInverting match regexp: ")
  (invert-occur--publish (invert-occur--find regexp)))

(defun invert-occur--find (regexp)
  (save-excursion
    (setf (point) (point-min))
    (cl-loop while (< (point) (point-max))
             for line upfrom 1
             for beg = (point)
             for end = (line-end-position)
             unless (re-search-forward regexp end :noerror)
             collect (list line beg end)
             do (setf (point) (1+ end)))))

(define-derived-mode invert-occur-mode special-mode "ioccur"
  "Major more for displaying `invert-occur' results.")
(define-key invert-occur-mode-map (kbd "RET") #'invert-occur--follow)
(define-key invert-occur-mode-map [mouse-2] #'invert-occur--follow)

(defun invert-occur--publish (results)
  (let ((source-buffer (current-buffer))
        (inhibit-read-only t))
    (with-current-buffer (get-buffer-create "*invert-occur*")
      (invert-occur-mode)
      (erase-buffer)
      (cl-loop for (line beg end) in results
               for marker = (set-marker (make-marker) beg source-buffer)
               for button = (propertize (format "% 3d " line)
                                        'mouse-face '(highlight)
                                        'target marker)
               do (insert button)
               do (insert-buffer-substring source-buffer beg end)
               do (insert "\n"))
      (setf (point) (point-min))
      (pop-to-buffer (current-buffer)))))

(defun invert-occur--follow ()
  (interactive)
  (let ((marker (get-text-property (point) 'target)))
    (pop-to-buffer (marker-buffer marker))
    (setf (point) (marker-position marker))))
