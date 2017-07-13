;; -*- coding: utf-8; lexical-binding: t; -*-
;; part of Xah Lee's emacs init file.
;; 2007-06
;; Xah Lee,
;; ∑ http://xahlee.org/



(defun xah-display-minor-mode-key-priority  ()
  "Print out minor mode's key priority.
URL `http://ergoemacs.org/emacs/minor_mode_key_priority.html'
Version 2017-01-27"
  (interactive)
  (mapc
   (lambda (x) (prin1 (car x)) (terpri))
   minor-mode-map-alist))

(defun xah-open-file-from-clipboard ()
  "Open the file path from OS's clipboard.
The clipboard should contain a file path or url to xah site. Open that file in emacs.
Version 2017-03-21"
  (interactive)
  (let (
        ($inputStr
         (with-temp-buffer
           (yank)
           (buffer-string)))
        $fpath
        )
    (if (string-match-p "\\`http://" $inputStr)
        (progn
          (setq $fpath (xahsite-url-to-filepath $inputStr "addFileName"))
          (if (file-exists-p $fpath)
              (progn (find-file $fpath))
            (progn (error "file doesn't exist 「%s」" $fpath))))
      (progn ; not starting “http://”
        (setq $inputStr (xah-remove-uri-fragment $inputStr))
        (setq $fpath (xahsite-web-path-to-filepath $inputStr default-directory))
        (if (file-exists-p $fpath)
            (progn (find-file $fpath))
          (progn (user-error "file doesn't exist.")))))))

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
  (let* (($inputStr (if (use-region-p)
                 (buffer-substring-no-properties (region-beginning) (region-end))
               (let ($p0 $p1 $p2
                         ($charSkipRegex "^  \"\t\n`'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›❮❯·。\\`"))
                 (setq $p0 (point))
                 ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
                 (skip-chars-backward $charSkipRegex)
                 (setq $p1 (point))
                 (goto-char $p0)
                 (skip-chars-forward $charSkipRegex)
                 (setq $p2 (point))
                 (goto-char $p0)
                 (buffer-substring-no-properties $p1 $p2))))
         ($path (replace-regexp-in-string ":\\'" "" $inputStr)))
    (if (string-match-p "\\`https?://" $path)
        (browse-url $path)
      (progn ; not starting “http://”
        (if (string-match "^\\`\\(.+?\\):\\([0-9]+\\)\\'" $path)
            (progn
              (let (
                    ($fpath (match-string 1 $path))
                    ($line-num (string-to-number (match-string 2 $path))))
                (if (file-exists-p $fpath)
                    (progn
                      (find-file $fpath)
                      (goto-char 1)
                      (forward-line (1- $line-num)))
                  (progn
                    (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" $fpath))
                      (find-file $fpath))))))
          (progn
            (if (file-exists-p $path)
                (find-file $path)
              (if (file-exists-p (concat $path ".el"))
                  (find-file (concat $path ".el"))
                (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" $path))
                  (find-file $path ))))))))))

(defun xah-open-file-path-under-cursor ()
  "Open the file path under cursor.
If there is text selection, use the text selection for path.
If path starts with “http://”, launch browser vistiting that URL, or open the corresponding file, if it's xah site.

Input path can be {relative, full path, URL}. See: `xahsite-web-path-to-filepath' for types of paths supported.

Version 2017-04-21"
  (interactive)
  (let* (
         ($inputStr1
          (xah-remove-uri-fragment
           (if (use-region-p)
               (buffer-substring-no-properties (region-beginning) (region-end))
             (let ($p0 $p1 $p2
                       ($charSkipRegex "^  \"\t\n`'|()[]{}「」<>〔〕“”〈〉《》【】〖〗«»‹›❮❯❬❭·。\\`"))
               (setq $p0 (point))
               ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
               (skip-chars-backward $charSkipRegex)
               (setq $p1 (point))
               (goto-char $p0)
               (skip-chars-forward $charSkipRegex)
               (setq $p2 (point))
               (goto-char $p0)
               (buffer-substring-no-properties $p1 $p2)))))
         ($inputStr2 (replace-regexp-in-string ":\\'" "" $inputStr1))
         $p
         )
    (if (string-equal $inputStr2 "")
        (progn (user-error "No path under cursor" ))
      (progn
        ;; convenience. if the input string start with a xah domain name, make it a url string
        (setq $p
              (cond
               ((string-match "\\`//" $inputStr2 ) (concat "http:" $inputStr2)) ; relative http protocol, used in css
               ((string-match "\\`ergoemacs\\.org" $inputStr2 ) (concat "http://" $inputStr2))
               ((string-match "\\`wordyenglish\\.com" $inputStr2 ) (concat "http://" $inputStr2))
               ((string-match "\\`xaharts\\.org" $inputStr2 ) (concat "http://" $inputStr2))
               ((string-match "\\`xahlee\\.info" $inputStr2 ) (concat "http://" $inputStr2))
               ((string-match "\\`xahlee\\.org" $inputStr2 ) (concat "http://" $inputStr2))
               ((string-match "\\`xahmusic\\.org" $inputStr2 ) (concat "http://" $inputStr2))
               ((string-match "\\`xahporn\\.org" $inputStr2 ) (concat "http://" $inputStr2))
               ((string-match "\\`xahsl\\.org" $inputStr2 ) (concat "http://" $inputStr2))
               (t $inputStr2)))
        (if (string-match-p "\\`https?://" $p)
            (if (xahsite-url-is-xah-website-p $p)
                (let (($fp (xahsite-url-to-filepath $p )))
                  (if (file-exists-p $fp)
                      (find-file $fp)
                    (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" $fp)) (find-file $fp))))
              (browse-url $p))
          (progn ; not starting “http://”
            (let (($fff (xahsite-web-path-to-filepath $p default-directory)))
              (if (file-exists-p $fff)
                  (progn (find-file $fff))
                (if (file-exists-p (concat $fff ".el"))
                    (progn (find-file (concat $fff ".el")))
                  (when (y-or-n-p (format "file doesn't exist: 「%s」. Create?" $fff)) (find-file $fff )))))))))))



(defun xah-toggle-margin-right ()
  "Toggle the right margin between `fill-column' or window width.
This command is convenient when reading novel, documentation.
Version 2016-07-21"
  (interactive)
  (if (null (cdr (window-margins)))
      (set-window-margins nil 0 (- (window-body-width) fill-column))
    (set-window-margins nil 0 0)))

(defun xah-toggle-line-spacing ()
  "Toggle line spacing between no extra space to extra half line height.
URL `http://ergoemacs.org/emacs/emacs_toggle_line_spacing.html'
Version 2017-06-02"
  (interactive)
  (if line-spacing
      (setq line-spacing nil)
    (setq line-spacing 0.5))
  (redraw-frame (selected-frame)))

(defun xah-toggle-read-novel-mode ()
  "Setup current buffer to be suitable for reading long novel/article text.

• Line wrap at word boundaries.
• Set a right margin.
• line spacing is increased.
• variable width font is used.

Call again to toggle back.
URL `http://ergoemacs.org/emacs/emacs_novel_reading_mode.html'
Version 2017-02-27"
  (interactive)
  (if (null (get this-command 'state-on-p))
      (progn
        (set-window-margins nil 0 9)
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
  (let ($p1 $p2 $searchStr )
    (if (use-region-p)
        (progn
          (setq $p1 (region-beginning))
          (setq $p2 (region-end))
          (setq $searchStr (buffer-substring-no-properties $p1 $p2)))
      (progn
        (setq $searchStr (word-at-point))))
    (list-matching-lines (regexp-quote $searchStr))))

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



(defun xah-sync-css ()
  "Save current file and copy to all other xahsite dirs.
Version 2016-10-01"
  (interactive)
  (let* (
         ($fromPath (buffer-file-name))
         ($fromFileName (file-name-nondirectory $fromPath ))
         $toPath
         )
    (copy-file
     $fromPath
     (concat $fromPath "~" (format-time-string "%Y%m%d_%H%M%S") "~")
     "OK-IF-ALREADY-EXISTS") ;backup
    (save-buffer)
    (mapc
     (lambda ($x)
       (progn
         (setq $toPath (concat (xahsite-url-to-filepath (format "http://%s/" $x)) $fromFileName))
         (when (not (string= $fromPath $toPath ))
           (if (file-exists-p $toPath)
               (progn
                 (copy-file $fromPath $toPath "OK-IF-ALREADY-EXISTS")
                 (message "wrote to 「%s」." $toPath))
             (progn (error "logic error. The file 「%s」 doesn't exist, it should already." $toPath)))))) [
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

This is Xah Lee's personal command assuming a particular dir structure.

Version 2016-11-02"
  (interactive)
  (let (
        $p1 $p2
        $inputStr
        $file
        $title
        $temp
        $urlFragmentPart
        ($pathDelimitors "^  \"\t\n'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·，。\\`") ; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
        )

    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (let ($p0)
        (setq $p0 (point))
        (skip-chars-backward $pathDelimitors)
        (setq $p1 (point))
        (goto-char $p0)
        (skip-chars-forward $pathDelimitors)
        (setq $p2 (point))))

    (setq $inputStr (buffer-substring-no-properties $p1 $p2))
    (setq $temp (split-uri-hashmark $inputStr))
    (setq $file (xahsite-web-path-to-filepath (aref $temp 0)))
    (setq $urlFragmentPart (aref $temp 1))

    (if (file-exists-p $file)
        (progn
          (setq $title
                (if (string-match-p ".+html\\'" $file)
                    (xah-html-get-html-file-title $file t)
                  (file-name-nondirectory $file)))
          (setq $title (if (null $title) "" $title ))
          (setq $title (xah-replace-pairs-in-string $title [["&amp;" "&"] ["&lt;" "<"] ["&gt;" ">" ]]))

          (delete-region $p1 $p2)
          (insert $title "\n" (xahsite-filepath-to-url $file) $urlFragmentPart))
      (progn (user-error "file doesn't exist.")))))

(defun xah-copy-url-current-file ()
  "Put the current file's URL into the kill-ring."
  (interactive)
  (let ($url)
    (setq $url (xahsite-filepath-to-url (buffer-file-name)))
    (kill-new $url)
    (message "URL copied %s" $url)))



(defun compact-uncompact-block-chinese ()
  "Remove or add line ending chars on current text block.
 (text block is delimited by blank lines)
This command is similar to a toggle of `fill-paragraph'.
When there is a text selection, act on the region.
Version 2017-02-02"
  (interactive)
  ;; This command symbol has a property “'stateIsCompact-p”.
  (let (currentStateIsCompact (deactivate-mark nil))
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
          (let ($p1 $p2) ; $p1 and $p2 are beginning/end of text block
            (progn
              (if (re-search-backward "\n[ \t]*\n" nil "move")
                  (progn (re-search-forward "\n[ \t]*\n")
                         (setq $p1 (point)))
                (setq $p1 (point)))
              (if (re-search-forward "\n[ \t]*\n" nil "move")
                  (progn (re-search-backward "\n[ \t]*\n")
                         (setq $p2 (point)))
                (setq $p2 (point))))
            (save-restriction
              (narrow-to-region $p1 $p2)
              (goto-char (point-min))
              (while (search-forward "\n" nil t) (replace-match "" nil t))))))
      (put this-command 'stateIsCompact-p (if currentStateIsCompact nil t)))))

;; (dolist (hook '(window-configuration-change-hook
;;                 window-size-change-functions
;;                 after-setting-font-hook))
;;   (add-hook hook 'xah-display-page-break-as-line))



(defcustom xah-interactive-abbrev-alist nil "A alist for interactive abbreves. Key and value are strings. Key is for abbrev. Value is the text to be inserted." :group 'xah)

(setq xah-interactive-abbrev-alist
      '(
        ("rsync1" . "rsync -z -r -v -t --delete --chmod=Dugo+x --chmod=ugo+r --exclude='*~' --exclude='.bash_history' --exclude='logs/' --exclude='xahbackup/' --exclude='.git/*' --rsh='ssh -l u40651120' ~/web/ u40651120@s168753655.onlinehome.us:~/")
        ("ssh" . "ssh -l u40651120 xahlee.org ")
        ("img1" . "convert -quality 85% ")
        ("imgScale" . "convert -scale 50% -quality 85% ")
        ("img256" . "convert +dither -colors 256 ")
        ("imgBatch" . "find . -name \"*png\" | xargs -l -i basename \"{}\" \".png\" | xargs -l -i  convert -quality 85% \"{}.png\" \"{}.jpg\"")
        ("img-bmp2png" . "find . -name \"*bmp\" | xargs -l -i basename \"{}\" \".bmp\" | xargs -l -i  convert \"{}.bmp\" \"{}.png\"")
        ("gif to webm" . "avconv -f gif -i cat.gif cat.webm")
        ("gif to mp4" . "avconv -f gif -i cat.gif cat.mp4")

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

(defun xah-interactive-abbrev ()
  "Prompt to insert string from a alist `xah-shell-abbrev-alist'.

URL `http://ergoemacs.org/emacs/emacs_interactive_abbrev.html'
version 2017-06-07"
  (interactive)
  (let ((x
         (ido-completing-read
          "abbrevs:"
          (mapcar (lambda (x) (car x)) xah-interactive-abbrev-alist)
          "PREDICATE" "REQUIRE-MATCH")))
    (insert (cdr (assoc x xah-interactive-abbrev-alist)))))

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
  "remove paypal umimportant text.
2017-03-22"
  (interactive)
  (progn
    (xah-replace-pairs-region
     (point-min) (point-max)
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

      ["Please do not reply to this email. This mailbox is not monitored and you will not receive a response. For assistance, log in to your PayPal account and click \"Help\" in the top right corner of any PayPal page." ""]
      ;; ["Please do not reply to this email. This mailbox is not monitored and you will not receive a response. For assistance, log in to your PayPal account and click Help in the top right corner of any PayPal page." ""]

      ["You can receive plain text emails instead of HTML emails. To change your Notifications preferences, log in to your account, go to your Profile, and click \"My settings\"." ""]

      ;; ["You can receive plain text emails instead of HTML emails. To change your Notifications preferences, log in to your account, go to your Profile, and click My settings." ""]
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
           ($p1 (point-min))
           ($p2 (point-max)))
      (save-excursion
        (save-restriction
          (narrow-to-region $p1 $p2)
          (progn
            (goto-char (point-min))
            (while (re-search-forward "[ \t]+\n" nil t)
              (replace-match "\n")))
          (progn
            (goto-char (point-min))
            (while (re-search-forward "\n\n\n+" nil t)
              (replace-match "\n\n"))))))))

(defun xah-replace-BOM-mark-etc ()
  "Query replace some invisible Unicode chars.
The chars to be searched are:
 ZERO WIDTH NO-BREAK SPACE (codepoint 65279, #xfeff)
 RIGHT-TO-LEFT MARK (codepoint 8207, #x200f)
 RIGHT-TO-LEFT OVERRIDE (codepoint 8238, #x202e)
 OBJECT REPLACEMENT CHARACTER (codepoint 65532, #xfffc)

Search begins at cursor position. (respects `narrow-to-region')

This is useful for text copied from twitter or Google Plus, because they often contain BOM mark. See URL `http://xahlee.info/comp/unicode_BOM_byte_orde_mark.html'

URL `http://ergoemacs.org/emacs/elisp_unicode_replace_invisible_chars.html'
Version 2016-07-24"
  (interactive)
  (query-replace-regexp "\u200f\\|\u202e\\|\ufeff\\|\ufffc" ""))

(defun xah-replace-BOM-mark-dir ()
  "temp hack. replace some invisible Unicode chars.
see `xah-replace-BOM-mark-etc'
Version 2015-10-11"
  (interactive)
  (require 'xah-find)
  (let ($dir)
    (setq $dir (ido-read-directory-name "Directory: " default-directory default-directory "MUSTMATCH"))
    (xah-find-replace-text (char-to-string 65279) "" $dir "\\.html\\'" t t t t)))

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

  (let ($inputStr $tempStr $p1 $p2
                 (case-fold-search t) )
    (save-excursion
      ;; (skip-chars-backward "0123456789abcdef")
      ;; (search-backward-regexp "[[:xdigit:]]+" nil t)
      (search-backward-regexp "[0123456789abcdef]+" nil t)
      (setq $p1 (point) )
      (re-search-forward "[0123456789abcdef]+" nil t)
      (setq $p2 (point) )

      (setq $inputStr (buffer-substring-no-properties $p1 $p2) )

      (let ((case-fold-search nil) )
        (setq $tempStr (replace-regexp-in-string "\\`0x" "" $inputStr )) ; C, Perl, …
        (setq $tempStr (replace-regexp-in-string "\\`#x" "" $tempStr )) ; elisp …
        (setq $tempStr (replace-regexp-in-string "\\`#" "" $tempStr ))  ; CSS …
        )

      ;; (message "Hex 「%s」 is 「%d」" $tempStr (string-to-number $tempStr 16))
      (message "input 「%s」 Hex 「%s」 is 「%d」" $inputStr $tempStr (string-to-number $tempStr 16)))))



(defun xah-unfontify-region-or-buffer ()
  "Unfontify text selection or buffer.
URL `http://ergoemacs.org/emacs/elisp_uncolor_region.html'
Version 2017-05-31"
  (interactive)
  (if (use-region-p)
      (font-lock-unfontify-region (region-beginning) (region-end))
    (font-lock-unfontify-buffer)))

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
         ($values ["cornsilk" "pale green" "pale turquoise" "thistle" "seashell" "honeydew"])
         ($index-before
          (if (get 'xah-cycle-background-color 'state)
              (get 'xah-cycle-background-color 'state)
            0))
         ($index-after (% (+ $index-before (length $values) *n) (length $values)))
         ($next-value (aref $values $index-after)))

    (put 'xah-cycle-background-color 'state $index-after)

    (set-background-color $next-value)
    (message "background color changed to %s" $next-value)))

(defun xah-browse-url-of-buffer ()
  "Similar to `browse-url-of-buffer' but visit xahlee.org.

save the file first.
Then, if `universal-argument' is called, visit the corresponding xahsite URL.
For example, if current buffer is of this file:
 ~/web/xahlee_info/index.html
then after calling this function,
default browser will be launched and opening this URL:
 http://xahlee.info/index.html
version 2017-04-10"
  (interactive)
  (let ($url)
    (setq $url
          (if current-prefix-arg
              (xahsite-filepath-to-url (buffer-file-name))
            (buffer-file-name)))
    (when (buffer-modified-p )
      (xah-clean-whitespace (point-min) (point-max))
      (save-buffer))
    (message "browsing %s" $url)
    (cond
     ((string-equal system-type "windows-nt") ; Windows
      (when (string-match "^c:/" $url) (setq $url (concat "file:///" $url)))
      (browse-url $url))
     ((string-equal system-type "gnu/linux")
      ;; (let ( (process-connection-type nil))
      ;;   (start-process "" nil "setsid" "firefox" (concat "file://" buffer-file-name )))
      ;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. eg with nautilus
      (browse-url $url ))
     ;; ((string-equal system-type "gnu/linux")
     ;;  (start-process "xahbrowse"
     ;;                 nil "setsid"
     ;;                 "firefox"
     ;;                 (concat "file://" buffer-file-name )))
     ((string-equal system-type "darwin") ; Mac
      (browse-url $url )))))



;; from newsgroup gnu.emacs.help, by Richard Riley, 2009-08-02
(defun xah-open-current-file-as-admin ()
  "Open the current buffer as unix root.
This command works on unixes only."
  (interactive)
  (when buffer-file-name (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun xah-run-current-java-file ()
  "Execute the current file's class with Java.
For example, if the current buffer is the file x.java,
then it'll call “java x” in a shell."
  (interactive)
  (let* (
         ($fnm (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
         ($prog-name "java"))
    (shell-command (concat $prog-name " " $fnm " &"))))

(defun xah-python-2to3-current-file ()
  "Convert current buffer from python 2 to python 3.
This command calls python3's script 「2to3」.
URL `http://ergoemacs.org/emacs/elisp_python_2to3.html'
Version 2016-02-16"
  (interactive)
  (let* (
         ($fName (buffer-file-name))
         ($fSuffix (file-name-extension $fName)))
    (when (buffer-modified-p)
      (save-buffer))
    (if (or (string-equal $fSuffix "py") (string-equal $fSuffix "py3"))
        (progn
          (shell-command (format "2to3 -w %s" $fName))
          (revert-buffer  "IGNORE-AUTO" "NOCONFIRM" "PRESERVE-MODES"))
      (error "file 「%s」 doesn't end in “.py” or “.py3”." $fName))))

(defun xah-change-file-line-ending-style (*files *style)
  "Change current file or dired marked file's newline convention.

When called non-interactively, *style is one of 'unix 'dos 'mac or any of accepted emacs coding system. See `list-coding-systems'.

URL `http://ergoemacs.org/emacs/elisp_convert_line_ending.html'
Version 2016-10-16"
  (interactive
   (list
    (if (eq major-mode 'dired-mode )
        (dired-get-marked-files)
      (list (buffer-file-name)))
    (ido-completing-read "Line ending:" '("Linux/MacOSX/Unix" "MacOS9" "Windows") "PREDICATE" "REQUIRE-MATCH")))
  (let* (
         ($codingSystem
          (cond
           ((equal *style "Linux/MacOSX/Unix") 'unix)
           ((equal *style "MacOS9") 'mac)
           ((equal *style "Windows") 'dos)
           (t (error "code logic error 65327. Expect one of it." )))))
    (mapc
     (lambda (x) (xah-convert-file-coding-system x $codingSystem))
     *files)))

(defun xah-convert-file-coding-system (*fpath *coding-system)
  "Convert file's encoding.
 *fpath is full path to file.
 *coding-system is one of 'unix 'dos 'mac or any of accepted emacs coding system. See `list-coding-systems'.

If the file is already opened, it will be saved after this command.

URL `http://ergoemacs.org/emacs/elisp_convert_line_ending.html'
Version 2015-07-24"
  (let ($buffer
        ($bufferOpened-p (get-file-buffer *fpath)))
    (if $bufferOpened-p
        (with-current-buffer $bufferOpened-p
          (set-buffer-file-coding-system *coding-system)
          (save-buffer))
      (progn
        (setq $buffer (find-file *fpath))
        (set-buffer-file-coding-system *coding-system)
        (save-buffer)
        (kill-buffer $buffer)))))

(defun xah-remove-wikipedia-link ()
  "delet wikipedia link at cursor position
Version 2017-06-05"
  (interactive)
  (require 'xah-html-mode)
  (let ( p2
        deletedText
        )
    (when (search-forward "</a>")
      (progn
        (setq p2 (point))
        (search-backward "<a href=\"http://en.wikipedia.org/wiki/")
        (setq deletedText (buffer-substring (point) p2))
        (xah-html-remove-html-tags (point) p2)
        (message "%s" deletedText)
        deletedText
        ))))

(global-set-key (kbd "<end> 3") 'xah-remove-wikipedia-link)



(defun xah-remove-all-wikipedia-link ()
  "delete all wikipedia links in a html file, except image links etc.
Version 2017-06-19"
  (interactive)
  (let (p1
        p2 deletedText
        (resultList '()))
    (goto-char (point-min))
    (while (search-forward "<a href=\"http://en.wikipedia.org/wiki/" nil t)
      (progn
        (search-backward "<a href" )
        (setq p1 (point))
        (search-forward ">")
        (setq p2 (point))

        (setq deletedText (buffer-substring-no-properties p1 p2))
        (push deletedText resultList)
        (delete-region p1 p2)

        (search-forward "</a>")
        (setq p2 (point))
        (search-backward "</a>")
        (setq p1 (point))
        (delete-region p1 p2)))

    (mapc (lambda (x) (princ x) (terpri )) resultList)))

(global-set-key (kbd "<end> 4") 'xah-remove-all-wikipedia-link)


;; don't use much anymore

;; (defun xah-dec-to-bin (decStr)
;;   "convert the decimal number string decStr into a binary (string)"
;;   (require 'calc-bin)
;;   (let ((calc-number-radix 2))
;;     (math-format-radix (string-to-number decStr))))



(defun xah-display-control-l-as-line ()
  "Display the formfeed ^L char as line.
URL `http://ergoemacs.org/emacs/emacs_form_feed_section_paging.html'
Version 2017-06-18"
  (interactive)
  ;; 2016-10-11 thanks to Steve Purcell's page-break-lines.el
  (progn
    (when (not buffer-display-table)
      (setq buffer-display-table (make-display-table)))
    (aset buffer-display-table ?\^L
          (vconcat (make-list 70 (make-glyph-code ?─ 'font-lock-comment-face))))
    (redraw-frame)))
