;; -*- coding: utf-8 -*-
;; some general command definitions
;; their purpose can be useful for anyone
;; they don't depend on my setup, or any assumptions

;; 2007-06, 2009-09-12
;;   Xah Lee
;; ∑ http://xahlee.org/



(require 'browse-url)

(defun xah-delete-current-file (&optional φno-backup-p)
  "Delete the file associated with the current buffer.

Also close the current buffer.  If no file is associated, just close buffer.

A backup file is created with filename appended “~‹date time stamp›~”. Existing file of the same name is overwritten. If the file is not associated with buffer, the backup file name starts with “xx_”.

When called with `universal-argument', don't create backup."
  (interactive "P")
  (let* (
         (fName (buffer-file-name)) 
         (bufferIsFile-p (if (null fName) nil t ))
         (backupName (concat fName "~" (format-time-string "%Y%m%d_%H%M%S") "~")))
    (if bufferIsFile-p
        (progn 
          (save-buffer fName)
          (if φno-backup-p
              nil
            (copy-file fName backupName t))
          (delete-file fName)
          (message "deleted and backup created at 「%s」." backupName))
      (progn 
        (if φno-backup-p
            nil
          (write-region (point-min) (point-max) (concat "xx_~" (format-time-string "%Y%m%d_%H%M%S") "~")))))
    (kill-buffer (current-buffer))))

(defun xah-make-backup ()
  "Make a backup copy of current file.

The backup file name has the form 「‹name›~‹timestamp›~」, in the same dir. If such a file already exist, it's overwritten.
If the current buffer is not associated with a file, its a error."
  (interactive)
  (let ((currentFileName (buffer-file-name)) backupFileName)
    (if (file-exists-p currentFileName)
        (progn
          (setq backupFileName (concat currentFileName "~" (format-time-string "%Y%m%d_%H%M%S") "~"))
          (copy-file currentFileName backupFileName t)
          (message (concat "Backup saved as: " (file-name-nondirectory backupFileName))))
      (progn ; file doesn't exist happens when it's new file not yet saved.
        (message (format "file 「%s」 doesn't exist." currentFileName))))))

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
(defun xah-open-current-file-as-admin ()
  "Open the current buffer as unix root.
This command works on unixes only."
  (interactive)
  (when buffer-file-name (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun xah-run-current-file ()
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
            ("js" . "node") ; node.js
            ("sh" . "bash")
            ("ml" . "ocaml")
            ("vbs" . "cscript")
            ;; ("pov" . "/usr/local/bin/povray +R2 +A0.1 +J1.2 +Am2 +Q9 +H480 +W640")
            ))
         (fName (buffer-file-name))
         (fSuffix (file-name-extension fName))
         (progName (cdr (assoc fSuffix suffixMap)))
         (cmdStr (concat progName " \""   fName "\"")))

    (when (buffer-modified-p)
      (when (y-or-n-p "Buffer modified. Do you want to save first?")
        (save-buffer)))

    (if (string-equal fSuffix "el") ; special case for emacs lisp
        (load (file-name-sans-extension fName))
      (if progName
          (progn
            (message "Running…")
            (shell-command cmdStr "*xah-run-current-file output*" ))
        (message "No recognized program file suffix for this file.")))))

(defun xah-run-current-java-file ()
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

(defun xah-python-2to3-current-file ()
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

(defun xah-change-file-line-ending (φfpath φline-ending-style)
  "Change file's newline character.
 「φfpath」 is full path to file.
 「φline-ending-style」 is one of 'unix 'dos 'mac or any of accepted emacs coding system. See `list-coding-systems'.

If the file is already opened, it will be saved after this command.
"
  (let (mybuffer
        (bufferOpened-p (get-file-buffer φfpath))
        )
    (if bufferOpened-p
        (progn (with-current-buffer bufferOpened-p (set-buffer-file-coding-system φline-ending-style) (save-buffer) ))
      (progn
        (setq mybuffer (find-file φfpath))
        (set-buffer-file-coding-system φline-ending-style)
        (save-buffer)
        (kill-buffer mybuffer) ) ) ) )

(defun xah-change-file-line-ending-style (φfile-list φline-ending-style)
  "Change current file or dired marked file's newline convention.
When called in lisp program, “φline-ending-style” is one of 'unix 'dos 'mac or any of accepted emacs coding system. See `list-coding-systems'.
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
           ((equal φline-ending-style "Unix") 'unix)
           ((equal φline-ending-style "Mac OS 9") 'mac)
           ((equal φline-ending-style "Windows") 'dos)
           (t (error "code logic error 65327. Expect one of it." ))
           ))
         )
    (mapc
     (lambda (ff) (xah-change-file-line-ending ff nlStyle))
     φfile-list)) )


;; don't use much anymore

;; (xah-beep-for-n-min 1)
(defun xah-beep-for-n-min (n)
  "Make 1 beep sound per second for N minutes.
WARNING: when beeping starts, emacs will freeze.
Press \\[keyboard-quit] to cancel the beep."
  (dotimes (i (* 60 n)) (progn (beep) (sleep-for 1)))
  )

(defun xah-set-alarm-after-n-min (n)
  "Make a alarm clock beep alert after N minutes.
The beep will last for 5 minutes.
WARNING: when beeping starts, emacs will freeze.
Press \\[keyboard-quit] to cancel the beep."
  (interactive "nHow many minutes from now: ")
  (run-with-timer
   (* 60 n) nil
   'xah-beep-for-n-min 5
   ))

(defun xah-erase-buffer ()
  "Delete the entire contents of the current buffer.
This command is similar to `erase-buffer', but also works if the buffer is
image (image-mode)."
  (interactive)
  (when (equal major-mode 'image-mode)
    (text-mode))
  (erase-buffer))
