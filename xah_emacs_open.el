;; -*- coding: utf-8; lexical-binding: t; -*-

(defun xah-open-in-textedit ()
  "Open the current file or `dired' marked files in Mac's TextEdit.
This command is for macOS only.

URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2017-11-21"
  (interactive)
  (let* (
         ($file-list
          (if (string-equal major-mode "dired-mode")
              (dired-get-marked-files)
            (list (buffer-file-name))))
         ($do-it-p (if (<= (length $file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when $do-it-p
      (cond
       ((string-equal system-type "darwin")
        (mapc
         (lambda ($fpath)
           (shell-command
            (format "open -a TextEdit.app \"%s\"" $fpath))) $file-list))))))

(defun xah-open-in-gimp ()
  "Open the current file or `dired' marked files in image editor gimp.
Works in linux and Mac. Not tested on Microsoft Windows.

URL `http://ergoemacs.org/emacs/emacs_dired_convert_images.html'
Version 2017-11-02"
  (interactive)
  (let* (
         ($file-list
          (if (string-equal major-mode "dired-mode")
              (dired-get-marked-files)
            (list (buffer-file-name))))
         ($do-it-p (if (<= (length $file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when $do-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda ($fpath)
           (w32-shell-execute "gimp" (replace-regexp-in-string "/" "\\" $fpath t t))) $file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda ($fpath)
           (shell-command
            (format "open -a /Applications/GIMP.app \"%s\"" $fpath))) $file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda ($fpath) (let ((process-connection-type nil)) (start-process "" nil "gimp" $fpath))) $file-list))))))

