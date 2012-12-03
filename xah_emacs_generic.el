;; -*- coding: utf-8 -*-
;; some general command definitions
;; their purpose can be useful for anyone
;; they don't depend on my setup, or any assumptions

;; 2007-06, 2009-09-12
;;   Xah Lee
;; ∑ http://xahlee.org/



(require 'browse-url)

(defun delete-current-file (ξno-backup-p)
  "Delete the file associated with the current buffer. Close the current buffer too.

A backup file is created with filename appended “~‹date time stamp›~”. Existing file of the same name is overwritten.

if ΞNO-BACKUP-P is non-nil (when called with `universal-argument'), don't create backup.

If no file is associated, just close buffer without prompt for save."
  (interactive "P")
  (let (fName)
    (when (buffer-file-name) ; buffer is associated with a file
      (setq fName (buffer-file-name))
      (save-buffer fName)
      (if ξno-backup-p
          (progn )
        (copy-file fName (concat fName "~" (format-time-string "%Y%m%d_%H%M%S") "~") t)
        )
      (delete-file fName)
      (message "「%s」 deleted." fName)
      )
    (kill-buffer (current-buffer))
    ) )


(defun make-backup ()
  "Make a backup copy of current buffer's file.
Create a backup of current buffer's file.
The new file name is the old file name with datetime stamp and “~” appended, in the same dir. If such a file already exist, it's overwritten.
If the current buffer is not associated with a file, its a error."
  (interactive)
  (let ((currentFileName (buffer-file-name)) backupFileName)
    (setq backupFileName (concat currentFileName "~" (format-time-string "%Y%m%d_%H%M%S") "~"))
    (copy-file currentFileName backupFileName t)
    (message (concat "Backup saved as: " (file-name-nondirectory backupFileName)))
    )
  )

;; Added to ergoemacs minor mode
;; (defun open-in-desktop ()
;;   "Open the current file in desktop.
;; Works in Microsoft Windows and Mac OS X."
;;   (interactive)
;;   (cond
;;    ((string-equal system-type "windows-nt")
;;     (w32-shell-execute "explore"
;;                        (replace-regexp-in-string "/" "\\" default-directory t t)))
;;    ((string-equal system-type "darwin") (shell-command "open ."))
;;    ) )

                                        ; from newsgroup gnu.emacs.help, by Richard Riley, 2009-08-02
(defun open-current-file-as-admin ()
  "Open the current buffer as unix root.
This command works on unixes only."
  (interactive)
  (when buffer-file-name (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun run-current-file ()
  "Execute or compile the current file.
For example, if the current buffer is the file x.pl,
then it'll call “perl x.pl” in a shell.
The file can be php, perl, python, ruby, javascript, bash, ocaml, vb, elisp.
File suffix is used to determine what program to run.

If the file is modified, ask if you want to save first. (This command always run the saved version.)

If the file is emacs lisp, run the byte compiled version if exist."
  (interactive)
  (let (suffixMap fName fSuffix progName cmdStr)

    ;; a keyed list of file suffix to comand-line program path/name
    (setq suffixMap 
          '(
            ("php" . "php")
            ("pl" . "perl")
            ("py" . "python")
            ("rb" . "ruby")
            ("js" . "node")             ; node.js
            ("sh" . "bash")
            ("ml" . "ocaml")
            ("vbs" . "cscript")
            ;; ("pov" . "/usr/local/bin/povray +R2 +A0.1 +J1.2 +Am2 +Q9 +H480 +W640")
            )
          )

    (setq fName (buffer-file-name))
    (setq fSuffix (file-name-extension fName))
    (setq progName (cdr (assoc fSuffix suffixMap)))
    (setq cmdStr (concat progName " \""   fName "\""))

    (when (buffer-modified-p)
      (progn 
        (when (y-or-n-p "Buffer modified. Do you want to save first?")
          (save-buffer) ) ) )

    (if (string-equal fSuffix "el") ; special case for emacs lisp
        (progn 
          (load (file-name-sans-extension fName)) 
          )
      (if progName
          (progn
            (message "Running…")
            (shell-command cmdStr "*run-current-file output*" )
            )
        (message "No recognized program file suffix for this file.")
        )
      )

    ;;    ;; if file is povray, open the generated image; refresh from disk
    ;;    (when (equal fSuffix "pov")
    ;;      (let ((revert-without-query (list "\\.png$")))
    ;;        (find-file-other-window
    ;;         (concat (file-name-sans-extension fName) ".png"))
    ;;        (redraw-display)
    ;;        ;; (revert-buffer t t)
    ;;        )
    ;;
;;;;       (let ((povImgFile (concat (file-name-sans-extension fName) ".png")))
;;;;         (find-file povImgFile)
;;;;         (redraw-display)
;;;;         ;; (revert-buffer t t)
;;;;         )
    ;;)
    ))

(defun run-current-java-file ()
  "Execute the current file's class with Java.
For example, if the current buffer is the file x.java,
then it'll call “java x” in a shell."
  (interactive)
  (let (fnm prog-name cmd-str)
    (setq fnm (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
    (setq prog-name "java")
    (setq cmd-str (concat prog-name " " fnm " &"))
    (shell-command cmd-str))
  )

;; ; the following 2 about dired are for opening all marked files. They are pulled from dired-x.
;; (defun dired-do-find-marked-files (&optional noselect)
;;   "Find all marked files displaying all of them simultaneously.
;; With optional NOSELECT just find files but do not select them.

;; The current window is split across all files marked, as evenly as possible.
;; Remaining lines go to bottom-most window.  The number of files that can be
;; displayed this way is restricted by the height of the current window and
;; `window-min-height'.

;; To keep dired buffer displayed, type \\[split-window-vertically] first.
;; To display just marked files, type \\[delete-other-windows] first."
;;   (interactive "P")
;;   (dired-simultaneous-find-file (dired-get-marked-files) noselect))

;; (defun dired-simultaneous-find-file (file-list noselect)
;;   "Visit all files in FILE-LIST and display them simultaneously.
;; The current window is split across all files in FILE-LIST, as evenly as
;; possible.  Remaining lines go to the bottom-most window.  The number of
;; files that can be displayed this way is restricted by the height of the
;; current window and the variable `window-min-height'.  With non-nil
;; NOSELECT the files are merely found but not selected."

;;   ;; We don't make this function interactive because it is usually too clumsy
;;   ;; to specify FILE-LIST interactively unless via dired.

;;   (let (size)

;;     (if noselect
;;         ;; Do not select the buffer.
;;         (find-file-noselect (car file-list))

;;       ;; We will have to select the buffer.  Calculate and check window size.
;;       (setq size (/ (window-height) (length file-list)))
;;       (or (<= window-min-height size)
;;           (error "Too many files to visit simultaneously.  Try C-u prefix"))
;;       (find-file (car file-list)))

;;     ;; Decrement.
;;     (setq file-list (cdr file-list))

;;     (while file-list

;;       (if noselect
;;           ;; Do not select the buffer.
;;           (find-file-noselect (car file-list))

;;         ;; Vertically split off a window of desired size.  Upper window will
;;         ;; have SIZE lines.  Select lower (larger) window.  We split it again.
;;         (select-window (split-window nil size))
;;         (find-file (car file-list)))

;;       ;; Decrement.
;;       (setq file-list (cdr file-list)))))

;; (defun count-region (posBegin posEnd)
;;   "Print number of words and chars in region."
;;   (interactive "r")
;;   (message "Counting …")
;;   (save-excursion
;;     (let (wCnt charCnt)
;;       (setq wCnt 0)
;;       (setq charCnt (- posEnd posBegin))
;;       (goto-char posBegin)
;;       (while (and (< (point) posEnd)
;;                   (re-search-forward "\\w+\\W*" posEnd t))
;;         (setq wCnt (1+ wCnt)))

;;       (message "Words: %d. Chars: %d." wCnt charCnt)
;;       )))

(defun count-words-region-or-line ()
  "Print number of words and chars in text selection or line."
  (interactive)
  (let (bds p1 p2 )
    (setq bds (get-selection-or-unit 'line))
    (setq p1 (elt bds 1) )
    (setq p2 (elt bds 2) )

    (save-excursion
      (let (wCnt charCnt)
        (setq wCnt 0)
        (setq charCnt (- p2 p1))
        (goto-char p1)
        (while (and (< (point) p2) (re-search-forward "\\w+\\W*" p2 t))
          (setq wCnt (1+ wCnt)))

        (message "Words: %d. Chars: %d." wCnt charCnt) )) ) )

(defun fix-timestamp (input-string &optional ξfrom-to)
  "Change timestamp under cursor into a yyyy-mm-dd format.
If there's a text selection, use that as input, else use current line.

Any “day of week”, or “time” info, or any other parts of the string, are discarded.
For example:
 「TUESDAY, FEB 15, 2011 05:16 ET」 ⇒ 「2011-02-15」
 「November 28, 1994」              ⇒ 「1994-11-28」
 「Nov. 28, 1994」                  ⇒ 「1994-11-28」
 「11/28/1994」                     ⇒ 「1994-11-28」
 「1994/11/28」                     ⇒ 「1994-11-28」

When called in lisp program, the optional second argument ΞFROM-TO is a vector [from to] of region boundary. (it can also be a list)
If ΞFROM-TO is non-nil, the region is taken as input (and INPUT-STRING is ignored).

Code detail: URL `http://ergoemacs.org/emacs/elisp_parse_time.html'"
  (interactive
   (progn
     (require 'xeu_elisp_util)
     (let ((bds (get-selection-or-unit 'line)))
       (list nil (vector (elt bds 1) (elt bds 2))) )
     )
   )
  (let (
        (ξstr (if ξfrom-to (buffer-substring-no-properties (elt ξfrom-to 0) (elt ξfrom-to 1) ) input-string))
        (workOnRegionP (if ξfrom-to t nil)))
    (require 'parse-time)

    (setq ξstr (replace-regexp-in-string "^ *\\(.+\\) *$" "\\1" ξstr)) ; remove white spaces

    (setq ξstr
          (cond
           ;; USA convention of mm/dd/yyyy
           ((string-match "\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9][0-9][0-9]\\)" ξstr)
            (concat (match-string 3 ξstr) "-" (match-string 1 ξstr) "-" (match-string 2 ξstr))
            )
           ;; USA convention of m/dd/yyyy
           ((string-match "\\([0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9][0-9][0-9]\\)" ξstr)
            (concat (match-string 3 ξstr) "-0" (match-string 1 ξstr) "-" (match-string 2 ξstr))
            )

           ;; USA convention of mm/dd/yy
           ((string-match "\\([0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9]\\)" ξstr)
            (concat (format-time-string "%C") (match-string 3 ξstr) "-" (match-string 1 ξstr) "-" (match-string 2 ξstr))
            )
           ;; USA convention of m/dd/yy
           ((string-match "\\([0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9]\\)" ξstr)
            (concat (format-time-string "%C") (match-string 3 ξstr) "-0" (match-string 1 ξstr) "-" (match-string 2 ξstr))
            )

           ;; yyyy/mm/dd
           ((string-match "\\([0-9][0-9][0-9][0-9]\\)/\\([0-9][0-9]\\)/\\([0-9][0-9]\\)" ξstr)
            (concat (match-string 1 ξstr) "-" (match-string 2 ξstr) "-" (match-string 3 ξstr))
            )

           ;; some ISO 8601. yyyy-mm-ddThh:mm
           ((string-match "\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)T[0-9][0-9]:[0-9][0-9]" ξstr)
            (concat (match-string 1 ξstr) "-" (match-string 2 ξstr) "-" (match-string 3 ξstr))
            )
           ;; some ISO 8601. yyyy-mm-dd
           ((string-match "\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)-\\([0-9][0-9]\\)" ξstr)
            (concat (match-string 1 ξstr) "-" (match-string 2 ξstr) "-" (match-string 3 ξstr))
            )
           ;; some ISO 8601. yyyy-mm
           ((string-match "\\([0-9][0-9][0-9][0-9]\\)-\\([0-9][0-9]\\)" ξstr)
            (concat (match-string 1 ξstr) "-" (match-string 2 ξstr))
            )

           ;; else
           (t
            (progn
              (setq ξstr (replace-regexp-in-string "January " "Jan. " ξstr))
              (setq ξstr (replace-regexp-in-string "February " "Feb. " ξstr))
              (setq ξstr (replace-regexp-in-string "March " "Mar. " ξstr))
              (setq ξstr (replace-regexp-in-string "April " "Apr. " ξstr))
              (setq ξstr (replace-regexp-in-string "May " "May. " ξstr))
              (setq ξstr (replace-regexp-in-string "June " "Jun. " ξstr))
              (setq ξstr (replace-regexp-in-string "July " "Jul. " ξstr))
              (setq ξstr (replace-regexp-in-string "August " "Aug. " ξstr))
              (setq ξstr (replace-regexp-in-string "September " "Sep. " ξstr))
              (setq ξstr (replace-regexp-in-string "October " "Oct. " ξstr))
              (setq ξstr (replace-regexp-in-string "November " "Nov. " ξstr))
              (setq ξstr (replace-regexp-in-string "December " "Dec. " ξstr))

              (setq ξstr (replace-regexp-in-string "\\([0-9]+\\)st" "\\1" ξstr))
              (setq ξstr (replace-regexp-in-string "\\([0-9]+\\)nd" "\\1" ξstr))
              (setq ξstr (replace-regexp-in-string "\\([0-9]+\\)rd" "\\1" ξstr))
              (setq ξstr (replace-regexp-in-string "\\([0-9]\\)th" "\\1" ξstr))

              (let (dateList ξyear ξmonth ξdate ξyyyy ξmm ξdd )
                (setq dateList (parse-time-string ξstr))
                (setq ξyear (nth 5 dateList))
                (setq ξmonth (nth 4 dateList))
                (setq ξdate (nth 3 dateList))

                (setq ξyyyy (number-to-string ξyear))
                (setq ξmm (if ξmonth (format "%02d" ξmonth) "" ) )
                (setq ξdd (if ξdate (format "%02d" ξdate) "" ) )
                (concat ξyyyy "-" ξmm "-" ξdd) ) ) ) ) )

    (if workOnRegionP
        (progn (delete-region  (elt ξfrom-to 0) (elt ξfrom-to 1) )
               (insert ξstr) )
      ξstr ) ))

(defun change-file-newline (fpath eol-system)
  "Change file's line ending to unix convention.
FPATH is full path to file.
eol-system is one of “'unix”, “'dos”, “'mac”.
The “'dos” means Windows's convention.
The “'mac” means Mac OS Classic's convention.
For Mac OS X, use “'unix”."
  (let (mybuffer)
    (setq mybuffer (find-file fpath))
    (set-buffer-file-coding-system eol-system)
    (save-buffer)
    (kill-buffer mybuffer)
    )
  )

(defun dired-2unix-eol-marked-files ()
  "Change marked file's newline convention to unix,
or file under cursor if no file is marked."
  (interactive)
  (mapc
   (lambda (ff) (change-file-newline ff 'unix))
   (dired-get-marked-files))
  )

(defun dired-utf-8-unix-marked-files ()
  "Change marked file's newline convention to unix,
or file under cursor if no file is marked."
  (interactive)
  (mapc
   (lambda (ff) (change-file-newline ff 'utf-8-unix))
   (dired-get-marked-files))
  )

;; --------------------------------------------------
;; don't use much anymore

                                        ; (beep-for-n-min 1)
(defun beep-for-n-min (n)
  "Make 1 beep sound per second for N minutes.
WARNING: when beeping starts, emacs will freeze.
Press \\[keyboard-quit] to cancel the beep."
  (dotimes (i (* 60 n)) (progn (beep) (sleep-for 1)))
  )

(defun set-alarm-after-n-min (n)
  "Make a alarm clock beep alert after N minutes.
The beep will last for 5 minutes.
WARNING: when beeping starts, emacs will freeze.
Press \\[keyboard-quit] to cancel the beep."
  (interactive "nHow many minutes from now: ")
  (run-with-timer
   (* 60 n) nil
   'beep-for-n-min 5
   ))

(defun xah-erase-buffer ()
  "Delete the entire contents of the current buffer.
This command is similar to `erase-buffer', but also works if the buffer is
image (image-mode)."
  (interactive)
  (when (equal major-mode 'image-mode)
    (text-mode))
  (erase-buffer))
