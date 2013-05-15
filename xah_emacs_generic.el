;; -*- coding: utf-8 -*-
;; some general command definitions
;; their purpose can be useful for anyone
;; they don't depend on my setup, or any assumptions

;; 2007-06, 2009-09-12
;;   Xah Lee
;; ∑ http://xahlee.org/



(require 'browse-url)

(defun delete-current-file (ξno-backup-p)
  "Delete the file associated with the current buffer.

Also close the current buffer.  If no file is associated, just close buffer without prompt for save.

A backup file is created with filename appended “~‹date time stamp›~”. Existing file of the same name is overwritten.

when called with `universal-argument', don't create backup."
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
  "Make a backup copy of current file.

The backup file name has the form 「‹name›~‹timestamp›~」, in the same dir. If such a file already exist, it's overwritten.
If the current buffer is not associated with a file, its a error."
  (interactive)
  (let ((currentFileName (buffer-file-name)) backupFileName)
    (if (file-exists-p currentFileName)
        (progn
          (setq backupFileName (concat currentFileName "~" (format-time-string "%Y%m%d_%H%M%S") "~"))
          (copy-file currentFileName backupFileName t)
          (message (concat "Backup saved as: " (file-name-nondirectory backupFileName)))
          )
      (progn ; file doesn't exist happens when it's new file not yet saved.
        (message (format "file 「%s」 doesn't exist." currentFileName)) ) ) ) )

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

;; from newsgroup gnu.emacs.help, by Richard Riley, 2009-08-02
(defun open-current-file-as-admin ()
  "Open the current buffer as unix root.
This command works on unixes only."
  (interactive)
  (when buffer-file-name (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun run-current-file ()
  "Execute the current file.
For example, if the current buffer is the file xx.py,
then it'll call “python xx.py” in a shell.
The file can be php, perl, python, ruby, javascript, bash, ocaml, vb, elisp.
File suffix is used to determine what program to run.

If the file is modified, ask if you want to save first.

If the file is emacs lisp, run the byte compiled version if exist."
  (interactive)
  (let* (
         (suffixMap
          `(
            ("php" . "php")
            ("pl" . "perl")
            ("py" . "python")
            ("py3" . ,(if (string-equal system-type "windows-nt") "c:/Python32/python.exe" "python3"))
            ("rb" . "ruby")
            ("js" . "node")             ; node.js
            ("sh" . "bash")
            ("ml" . "ocaml")
            ("vbs" . "cscript")
            ;; ("pov" . "/usr/local/bin/povray +R2 +A0.1 +J1.2 +Am2 +Q9 +H480 +W640")
            )
          )
         (fName (buffer-file-name))
         (fSuffix (file-name-extension fName))
         (progName (cdr (assoc fSuffix suffixMap)))
         (cmdStr (concat progName " \""   fName "\""))
         )

    (when (buffer-modified-p)
      (when (y-or-n-p "Buffer modified. Do you want to save first?")
          (save-buffer) ) )

    (if (string-equal fSuffix "el") ; special case for emacs lisp
        (load (file-name-sans-extension fName))
      (if progName
          (progn
            (message "Running…")
            (shell-command cmdStr "*run-current-file output*" )
            )
        (message "No recognized program file suffix for this file.")
        ) ) ))

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

(defun python-2to3-current-file ()
  "Convert current buffer from python 2 to python 3.

this command calls python3's script 「2to3」."
  (interactive)
  (let* (
         (fName (buffer-file-name))
         (fSuffix (file-name-extension fName))
         )
    (when (buffer-modified-p)
      (progn
        (when (y-or-n-p "Buffer modified. Do you want to save first?")
          (save-buffer) ) ) )

    (if (or (string-equal fSuffix "py") (string-equal fSuffix "py3") )
        (progn
          (shell-command (format "2to3 -w %s" fName))
          (revert-buffer  "IGNORE-AUTO" "NOCONFIRM" "PRESERVE-MODES")
          )
      (progn
            (error "file 「%s」 doesn't end in “.py” or “.py3”." fName)
            )
      )
    ))

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
  "Print number of words and chars in text selection or line.
In emacs 24, you can use `count-words'."
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

(defun change-file-line-ending (fpath lineEndingStyle)
  "Change file's newline character.
 「fpath」 is full path to file.
 「lineEndingStyle」 is one of 'unix 'dos 'mac or any of accepted emacs coding system. See `list-coding-systems'.

If the file is already opened, it will be saved after this command.
"
  (let (mybuffer
        (bufferOpened-p (get-file-buffer fpath))
        )
    (if bufferOpened-p
        (progn (with-current-buffer bufferOpened-p (set-buffer-file-coding-system lineEndingStyle) (save-buffer) ))
      (progn
        (setq mybuffer (find-file fpath))
        (set-buffer-file-coding-system lineEndingStyle)
        (save-buffer)
        (kill-buffer mybuffer) ) ) ) )

(defun change-file-line-ending-style (fileList lineEndingStyle)
  "Change current file or dired marked file's newline convention.
When called in lisp program, “lineEndingStyle” is one of 'unix 'dos 'mac or any of accepted emacs coding system. See `list-coding-systems'.
"
  (interactive
   (list
    (if (eq major-mode 'dired-mode )
        (dired-get-marked-files)
      (list (buffer-file-name)) )
    (ido-completing-read "Style:" '("Unix" "Mac OS 9" "Windows") "PREDICATE" "REQUIRE-MATCH"))
   )
  (let* (
         (nlStyle
          (cond
           ((equal lineEndingStyle "Unix") 'unix)
           ((equal lineEndingStyle "Mac OS 9") 'mac)
           ((equal lineEndingStyle "Windows") 'dos)
           (t (error "code logic error 65327. Expect one of it." ))
           ))
         )
    (mapc
     (lambda (ff) (change-file-line-ending ff nlStyle))
     fileList)) )


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
