;; -*- coding: utf-8 -*-
;; 2014-01-05

(defun xah-cut-line-or-region ()
  "Cut the current line, or text selection."
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (kill-region (line-beginning-position) (line-beginning-position 2))))

(defun xah-copy-line-or-region ()
  "Copy current line, or text selection."
  (interactive)
  (if (use-region-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-beginning-position 2))))

(defun xah-copy-all ()
  "Put the whole buffer content into the `kill-ring'.
If `narrow-to-region' is in effect, then copy that region only."
  (interactive)
  (kill-new (buffer-string))
  (message "Buffer content copied."))

(defun xah-cut-all ()
  "Cut the whole buffer content into the `kill-ring'.
If `narrow-to-region' is in effect, then cut that region only."
  (interactive)
  (kill-new (buffer-string))
  (delete-region (point-min) (point-max)))

(defun xah-shrink-whitespaces ()
  "Remove white spaces around cursor to just one or none.
If current line does have visible chars, then shrink whitespace surrounding cursor to just one space.
If current line does not have visible chars, then shrink al neighboring blank lines to just one.
If current line is a single space, remove that space.

Calling this command 3 times will always result in no whitespaces around cursor."
  (interactive)
  (let ((pos (point))
        line-has-meat-p ; current line contains non-white space chars
        space-tab-neighbor-p
        whitespace-begin whitespace-end
        space-or-tab-begin space-or-tab-end
        )
    (save-excursion
      ;; todo: might consider whitespace as defined by syntax table, and also consider whitespace chars in unicode if syntax table doesn't already considered it.
      (setq space-tab-neighbor-p (if (or (looking-at " \\|\t") (looking-back " \\|\t")) t nil))
      (beginning-of-line) 
      (setq line-has-meat-p (search-forward-regexp "[[:graph:]]" (line-end-position) t))

      (goto-char pos)
      (skip-chars-backward "\t ")
      (setq space-or-tab-begin (point))

      (skip-chars-backward "\t \n")
      (setq whitespace-begin (point))

      (goto-char pos)
      (skip-chars-forward "\t ")
      (setq space-or-tab-end (point))
      (skip-chars-forward "\t \n")
      (setq whitespace-end (point)))

    (if line-has-meat-p
        (let (deleted-text)
          (when space-tab-neighbor-p
            ;; remove all whitespaces in the range
            (setq deleted-text (delete-and-extract-region space-or-tab-begin space-or-tab-end))
            ;; insert a whitespace only if we have removed something
            ;; different that a simple whitespace
            (if (not (string= deleted-text " "))
                (insert " "))))

      (progn
        (delete-blank-lines)
        ;; (delete-region whitespace-begin whitespace-end)
        ;; (insert "\n\n")
        ))))

(defun xah-shrink-whitespaces2 ()
  "Remove white spaces around cursor to just one."
  (interactive)
  (let ((pos (point))
        p1 p2 )
    (save-excursion 
      (skip-chars-backward " \t\n"  )
      (setq p1 (point))
      (goto-char pos)
      (skip-chars-forward " \t\n"   )
      (setq p2 (point))
      (delete-region p1 p2)
      (insert " ")
      )))

 (defun xah-compact-uncompact-block ()
  "Remove or add line ending chars on current paragraph.
This command is similar to a toggle of `fill-paragraph'.
When there is a text selection, act on the region."
  (interactive)

  ;; This command symbol has a property “'stateIsCompact-p”.
  (let (currentStateIsCompact (bigFillColumnVal 90002000) (deactivate-mark nil))
    ;; 90002000 is just random. you can use `most-positive-fixnum'

    (save-excursion
      ;; Determine whether the text is currently compact.
      (setq currentStateIsCompact
            (if (eq last-command this-command)
                (get this-command 'stateIsCompact-p)
              (if (> (- (line-end-position) (line-beginning-position)) fill-column) t nil)))

      (if (use-region-p)
          (if currentStateIsCompact
              (fill-region (region-beginning) (region-end))
            (let ((fill-column bigFillColumnVal))
              (fill-region (region-beginning) (region-end))))
        (if currentStateIsCompact
            (fill-paragraph nil)
          (let ((fill-column bigFillColumnVal))
            (fill-paragraph nil))))

      (put this-command 'stateIsCompact-p (if currentStateIsCompact nil t)))))

(defun xah-toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles between: “all lower”, “Init Caps”, “ALL CAPS”."
  (interactive)

  (let (p1 p2 (deactivate-mark nil) (case-fold-search nil))
    (if (use-region-p)
        (setq p1 (region-beginning) p2 (region-end))
      (let ((bds (bounds-of-thing-at-point 'symbol)))
        (setq p1 (car bds) p2 (cdr bds))))

;; (message "bds %s %s" p1 p2)

    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char p1)
        (cond
         ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all caps"))
         ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "init caps"))
         ((looking-at "[[:lower:]]") (put this-command 'state "all lower"))
         ((looking-at "[[:upper:]]") (put this-command 'state "all caps"))
         (t (put this-command 'state "all lower")))))

    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region p1 p2) (put this-command 'state "init caps"))
     ((string= "init caps" (get this-command 'state))
      (upcase-region p1 p2) (put this-command 'state "all caps"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region p1 p2) (put this-command 'state "all lower")))))

(defun xah--backward-real-double-quote ()
  "Move cursor bakcward to a \", but excluding those with backslash."
  (interactive)
  (search-backward "\"" nil t)
  (while (looking-back "\\\\")
    (search-backward "\"" nil t)))

(defun xah--forward-real-double-quote ()
  "Move cursor forward to a \", but excluding those with backslash."
  (interactive)
  (search-forward "\"" nil t)
  (while (looking-back "\\\\\"")
    (search-forward "\"" nil t)))

(defun xah-select-text-in-quote ()
  "Select text between ASCII quotes, single or double."
  (interactive)
  (let (p1 p2)
    (if (nth 3 (syntax-ppss))
        (progn
          (backward-up-list 1 "ESCAPE-STRINGS" "NO-SYNTAX-CROSSING")
          (setq p1 (point))
          (forward-sexp 1)
          (setq p2 (point))
          (goto-char (1+ p1))
          (set-mark (1- p2)))
      (progn
        (error "Cursor not inside quote")))))

(defun xah-select-text-in-bracket ()
  "Select text between the nearest brackets.
⁖  () [] {} «» ‹› “” 〖〗 【】 「」 『』 （） 〈〉 《》 〔〕 ⦗⦘ 〘〙 ⦅⦆ 〚〛 ⦃⦄ ⟨⟩."
  (interactive)
  (with-syntax-table (standard-syntax-table)
    (modify-syntax-entry ?\« "(»")
    (modify-syntax-entry ?\» ")«")
    (modify-syntax-entry ?\‹ "(›")
    (modify-syntax-entry ?\› ")‹")
    (modify-syntax-entry ?\“ "(”")
    (modify-syntax-entry ?\” ")“")
    (modify-syntax-entry ?\‘ "(’")
    (modify-syntax-entry ?\’ ")‘")
    (let (pos p1 p2)
      (setq pos (point))
      (search-backward-regexp "\\s(" nil t )
      (setq p1 (point))
      (forward-sexp 1)
      (setq p2 (point))
      (goto-char (1+ p1))
      (set-mark (1- p2)))))

(defun xah-select-text-in-bracket-or-quote ()
  "Select text between the nearest brackets or quote."
  (interactive)
  (if (nth 3 (syntax-ppss))
      (xah-select-text-in-quote)
    (if  
        (or  
         (string= major-mode "xah-html-mode")
         (string= major-mode "xml-mode")
         (string= major-mode "nxml-mode")
         (string= major-mode "html-mode"))
        (xah-select-text-in-html-bracket)
      (xah-select-text-in-bracket))))

(defun xah-select-text-in-html-bracket ()
  "Select text between <…> or >…<."
  (interactive)
  (let (p0 p1< p1> p2< p2> 
           distance-p1<
           distance-p1>
           )
    (setq p0 (point))
    (search-backward "<" nil "NOERROR" )
    (setq p1< (point))
    (goto-char p0)
    (search-backward ">" nil "NOERROR" )
    (setq p1> (point))
    (setq distance-p1< (abs (- p0 p1<)))
    (setq distance-p1> (abs (- p1> p0)))
    (if (< distance-p1< distance-p1>)
        (progn 
          (goto-char p0)
          (search-forward ">" nil "NOERROR" )
          (setq p2> (point))
          (goto-char (1+ p1<))
          (set-mark (1- p2>)))
      (progn 
        (goto-char p0)
        (search-forward "<" nil "NOERROR" )
        (setq p2< (point))
        (goto-char (1+ p1>))
        (set-mark (1- p2<))))))

(defun xah-select-current-line ()
  "Select the current line."
  (interactive)
  (end-of-line)
  (set-mark (line-beginning-position)))

(defun xah-select-current-block ()
  "Select the current block of text between blank  lines."
  (interactive)
  (let (p1 p2)
    (progn
      (if (re-search-backward "\n[ \t]*\n" nil "move")
          (progn (re-search-forward "\n[ \t]*\n")
                 (setq p1 (point)))
        (setq p1 (point)))
      (if (re-search-forward "\n[ \t]*\n" nil "move")
          (progn (re-search-backward "\n[ \t]*\n")
                 (setq p2 (point)))
        (setq p2 (point))))
    (set-mark p1)))

;; by Nikolaj Schumacher, 2008-10-20. Released under GPL.
(defun xah-semnav-up (arg)
  (interactive "p")
  (when (nth 3 (syntax-ppss))
    (if (> arg 0)
        (progn
          (skip-syntax-forward "^\"")
          (goto-char (1+ (point)))
          (setq arg (1- arg) ))
      (skip-syntax-backward "^\"")
      (goto-char (1- (point)))
      (setq arg (1+ arg) )))
  (up-list arg))

;; by Nikolaj Schumacher, 2008-10-20. Released under GPL.
(defun xah-extend-selection (arg &optional incremental)
  "Select the current word.
Subsequent calls expands the selection to larger semantic unit.

This command works mostly in lisp syntax."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (or (and transient-mark-mode mark-active)
                         (eq last-command this-command))))
  (if incremental
      (progn
        (xah-semnav-up (- arg))
        (forward-sexp)
        (mark-sexp -1))
    (if (> arg 1)
        (xah-extend-selection (1- arg) t)
      (if (looking-at "\\=\\(\\s_\\|\\sw\\)*\\_>")
          (goto-char (match-end 0))
        (unless (memq (char-before) '(?\) ?\"))
          (forward-sexp)))
      (mark-sexp -1))))



(defun xah-next-user-buffer ()
  "Switch to the next user buffer.
User buffers are those whose name does not start with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (string-equal "*" (substring (buffer-name) 0 1)) (< i 20))
      (setq i (1+ i)) (next-buffer))))

(defun xah-previous-user-buffer ()
  "Switch to the previous user buffer.
User buffers are those whose name does not start with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (string-equal "*" (substring (buffer-name) 0 1)) (< i 20))
      (setq i (1+ i)) (previous-buffer) )))

(defun xah-next-emacs-buffer ()
  "Switch to the next emacs buffer.
Emacs buffers are those whose name starts with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (not (string-equal "*" (substring (buffer-name) 0 1))) (< i 20))
      (setq i (1+ i)) (next-buffer) )))

(defun xah-previous-emacs-buffer ()
  "Switch to the previous emacs buffer.
Emacs buffers are those whose name starts with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (not (string-equal "*" (substring (buffer-name) 0 1))) (< i 20))
      (setq i (1+ i)) (previous-buffer) )))



(defvar xah-recently-closed-buffers (cons nil nil) "A list of recently closed buffers. The max number to track is controlled by the variable `xah-recently-closed-buffers-max'.")
(defvar xah-recently-closed-buffers-max 30 "The maximum length for `xah-recently-closed-buffers'.")

(defun xah-close-current-buffer ()
  "Close the current buffer.

Similar to (kill-buffer (current-buffer)) with the following addition:

• Prompt user to save if the buffer has been modified even if the buffer is not associated with a file.
• Make sure the buffer shown after closing is a user buffer.
• If the buffer is editing a source file in an org-mode file, prompt the user to save before closing.
• If the buffer is a file, add the path to the list `xah-recently-closed-buffers'.
• If it is the minibuffer, exit the minibuffer

A emacs buffer is one who's name starts with *.
Else it is a user buffer."
  (interactive)
  (let (emacsBuff-p
        isEmacsBufferAfter
        (org-p (string-match "^*Org Src" (buffer-name))))

    (setq emacsBuff-p (if (string-match "^*" (buffer-name)) t nil) )

    (if (string= major-mode "minibuffer-inactive-mode")
        (minibuffer-keyboard-quit) ; if the buffer is minibuffer
      (progn
        ;; offer to save buffers that are non-empty and modified, even
        ;; for non-file visiting buffer. (because kill-buffer does not
        ;; offer to save buffers that are not associated with files)
        (when (and (buffer-modified-p)
                   (not emacsBuff-p)
                   (not (string-equal major-mode "dired-mode"))
                   (if (equal (buffer-file-name) nil)
                       (if (string-equal "" (save-restriction (widen) (buffer-string))) nil t)
                     t))
          (if (y-or-n-p (format "Buffer %s modified; Do you want to save? " (buffer-name)))
              (save-buffer)
            (set-buffer-modified-p nil)))
        ;;
        (when (and (buffer-modified-p)
                   org-p)
          (if (y-or-n-p (format "Buffer %s modified; Do you want to save? " (buffer-name)))
              (org-edit-src-save)
            (set-buffer-modified-p nil)))

        ;; save to a list of closed buffer
        (when (not (equal buffer-file-name nil))
          (setq xah-recently-closed-buffers
                (cons (cons (buffer-name) (buffer-file-name)) xah-recently-closed-buffers))
          (when (> (length xah-recently-closed-buffers) xah-recently-closed-buffers-max)
            (setq xah-recently-closed-buffers (butlast xah-recently-closed-buffers 1))))

        ;; close
        (kill-buffer (current-buffer))

        ;; if emacs buffer, switch to a user buffer
        (if (string-match "^*" (buffer-name))
            (setq isEmacsBufferAfter t)
          (setq isEmacsBufferAfter nil))
        (when isEmacsBufferAfter
          (xah-next-user-buffer) ) ))))

(defun xah-open-last-closed ()
  "Open the last closed file."
  (interactive)
  (find-file (cdr (pop xah-recently-closed-buffers)) ) )



(defun xah-describe-major-mode ()
  "Show inline doc for current major-mode."
  ;; code by Kevin Rodgers. 2009-02-25.
  ;; Modified to translate keybindings (2013)
  (interactive)
  (describe-function major-mode))


(defun xah-click-to-search (φclick)
  "Mouse click to start `isearch-forward-symbol-at-point' (emacs 24.4) at clicked point."
  (interactive "e")
  (let ((p1 (posn-point (event-start φclick))))
    (goto-char p1)
    (isearch-forward-symbol-at-point)
    ;; (describe-char p1)
    ))

(defun xah-click-describe-char (φclick)
  "Mouse click to `describe-char' at clicked point."
  (interactive "e")
  (let ((p1 (posn-point (event-start φclick))))
    (goto-char p1)
    (describe-char p1)
    ))
