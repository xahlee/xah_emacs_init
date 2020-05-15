;; 2020-05-15

(getenv "HOME") ; "C:\\Users\\xah\\AppData\\Roaming"
(getenv "HOMEPATH") ; "\\Users\\xah"

(expand-file-name "~/" ) ;; "c:/Users/xah/"

(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (progn
    (setenv "HOME" (getenv "HOMEPATH"))))
 ((string-equal system-type "darwin") ; Mac OS X
  (progn
    ))
 ((string-equal system-type "gnu/linux") ; linux
  (progn
    )))
