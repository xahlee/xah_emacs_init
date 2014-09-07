;; -*- coding: utf-8 -*-
;; part of Xah Lee's emacs init file.
;; 2014-04-24
;; Xah Lee,
;; ∑ http://xahlee.org/

(defun xah-browse-url-of-buffer ()
  "Similar to `browse-url-of-buffer' but visit xahlee.org.

save the file first.
Then, if `universal-argument' is called, visit the corresponding xahsite URL.
For example, if current buffer is of this file:
 ~/web/xahlee_info/index.html
then after calling this function,
default browser will be launched and opening this URL:
 http://xahlee.info/index.html"
  (interactive)
  (let (myURL)
    (setq myURL
          (if current-prefix-arg
              (xahsite-filepath-to-url (buffer-file-name))
            (buffer-file-name)))

    (when (buffer-modified-p ) 
      (xah-clean-whitespace 1 (point-max))
      (save-buffer))

    (cond
     ((string-equal system-type "windows-nt") ; Windows
      (when (string-match "^c:/" myURL) (setq myURL (concat "file:///" myURL)))
      (browse-url-firefox myURL))
     ((string-equal system-type "gnu/linux")
      (browse-url-firefox myURL))
     ((string-equal system-type "darwin") ; Mac
      ;; (browse-url-firefox myURL)
      (browse-url myURL )))))

(defun xah-browse-url-of-buffer-firefox ()
  "Same as `browse-url-of-buffer' but using Firefox.
You need Firefox's path in the path environment variable within emacs.
e.g.
 (setenv \"PATH\" (concat \"C:/Program Files (x86)/Mozilla Firefox/\" \";\" (getenv \"PATH\") ) )
On Mac OS X, you don't need to. This command makes this shell call:
 「open -a Firefox.app http://example.com/」"
  (interactive)
  (let ()
    (cond
     ((string-equal system-type "windows-nt") ; Windows
      (shell-command (concat "firefox file://" buffer-file-name))
      )
     ((string-equal system-type "gnu/linux")
      (shell-command (concat "firefox file://" buffer-file-name))
      )
     ((string-equal system-type "darwin") ; Mac
      (shell-command (concat "open -a Firefox.app file://" buffer-file-name))
       ) )
    ))

(defun xah-browse-url-Google-Chrome (φuri)
  "Same as `browse-url' but using Google Chrome."
  (interactive)
  (let ()
    (shell-command (concat "chrome " φuri))
    ))

(defun xah-browse-url-of-buffer-firefox-2 ()
  "Same as `browse-url-of-buffer' but using Firefox.
You need to have the firefox path in `exec-path'. e.g.:
 (add-to-list 'exec-path \"c:/Program Files (x86)/Mozilla Firefox/\")"
  (interactive)
  (let ()
    (require 'browse-url)
    (browse-url-firefox (concat "file:///" buffer-file-name))
    ))
