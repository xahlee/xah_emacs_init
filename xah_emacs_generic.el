;; -*- coding: utf-8 -*-
;; some general command definitions
;; their purpose can be useful for anyone
;; they don't depend on my setup, or any assumptions

;; 2007-06, 2009-09-12
;;   Xah Lee
;; ∑ http://xahlee.org/



(require 'browse-url)

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

(defun xah-run-current-java-file ()
  "Execute the current file's class with Java.
For example, if the current buffer is the file x.java,
then it'll call “java x” in a shell."
  (interactive)
  (let* (
         (fnm (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))
         (prog-name "java"))
    (shell-command (concat prog-name " " fnm " &"))))

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

(defun xah-change-file-line-ending-style (φfile-list φline-ending-style)
  "Change current file or dired marked file's newline convention.

When called non-interactively, φline-ending-style is one of 'unix 'dos 'mac or any of accepted emacs coding system. See `list-coding-systems'.
URL `http://ergoemacs.org/emacs/elisp_convert_line_ending.html'
Version 2015-07-24
"
  (interactive
   (list
    (if (eq major-mode 'dired-mode )
        (dired-get-marked-files)
      (list (buffer-file-name)))
    (ido-completing-read "Line ending:" '("Linux/MacOSX/Unix" "MacOS9" "Windows") "PREDICATE" "REQUIRE-MATCH")))
  (let* (
         (ξcodingSystem
          (cond
           ((equal φline-ending-style "Unix") 'unix)
           ((equal φline-ending-style "Mac OS 9") 'mac)
           ((equal φline-ending-style "Windows") 'dos)
           (t (error "code logic error 65327. Expect one of it." )))))
    (mapc
     (lambda (x) (xah-convert-file-coding-system x ξcodingSystem))
     φfile-list)))

(defun xah-convert-file-coding-system (φfpath φcoding-system)
  "Convert file's encoding.
 φfpath is full path to file.
 φcoding-system is one of 'unix 'dos 'mac or any of accepted emacs coding system. See `list-coding-systems'.

If the file is already opened, it will be saved after this command.
URL `http://ergoemacs.org/emacs/elisp_convert_line_ending.html'
Version 2015-07-24
"
  (let (ξbuffer
        (ξbufferOpened-p (get-file-buffer φfpath)))
    (if ξbufferOpened-p
        (progn
          (with-current-buffer ξbufferOpened-p
            (set-buffer-file-coding-system φcoding-system)
            (save-buffer)))
      (progn
        (setq ξbuffer (find-file φfpath))
        (set-buffer-file-coding-system φcoding-system)
        (save-buffer)
        (kill-buffer ξbuffer)))))


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
