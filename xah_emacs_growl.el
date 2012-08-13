;;; growl.el --- Growl notifications
;; Copyright (C) 2006 Brian Templeton

;; Author: Brian Templeton <bpt@tunes.org>

;; Keywords: growl notification mac osx

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;;; Code:

(defvar growl-program "/usr/local/bin/growlnotify")


(defun growl (title message)
  (start-process "growl" " growl"
                 growl-program
                 title
                 "-a" "Emacs")
  (process-send-string " growl" message)
  (process-send-string " growl" "\n")
  (process-send-eof " growl"))

(defun growl-rcirc-print-hook (process sender response target text)
  (when (and (string-match (rcirc-nick process) text)
             (not (string= (rcirc-nick process) sender))
             (not (string= (rcirc-server-name process) sender)))
    (growl "Beep:"
           (format "%s invoked you in %s" sender target))))

(eval-after-load 'rcirc
  '(add-hook 'rcirc-print-hooks 'growl-rcirc-print-hook))

(provide 'growl)
;;; growl.el ends here