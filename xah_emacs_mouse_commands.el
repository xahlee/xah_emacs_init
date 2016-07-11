;; -*- coding: utf-8 -*-

(defun xah-scroll-down-10-lines ()
  "scroll down 10 lines"
  (interactive)
  (scroll-down 10))

(defun xah-scroll-up-10-lines ()
  "scroll up 10 lines"
  (interactive)
  (scroll-up 10))

(defun xah-cursor-down-50-lines ()
  "Move cursor down 50 logical lines.
Version 2015-07-06"
  (interactive)
  (forward-line 50))

(defun xah-cursor-up-50-lines ()
  "Move cursor up 50 logical lines.
Version 2015-07-06"
  (interactive)
  (forward-line -50))

(defun xah-cursor-down-25-lines ()
  "Move cursor down 25 logical lines.
Version 2015-07-06"
  (interactive)
  (forward-line 25))

(defun xah-cursor-up-25-lines ()
  "Move cursor up 25 logical lines.
Version 2015-07-06"
  (interactive)
  (forward-line -25))

(defvar xah-forward-n-words 4 "integer used by `xah-forward-n-words'")
(setq xah-forward-n-words 4)

(defun xah-forward-n-words ()
  "`forward-word' `xah-forward-n-words' times."
  (interactive)
  (forward-word xah-forward-n-words))

(defun xah-backward-n-words ()
  "`backward-word' `xah-forward-n-words' times."
  (interactive)
  (backward-word xah-forward-n-words))

(defvar xah-forward-n-chars 50 "a integer used by `xah-forward-n-chars'")
(setq xah-forward-n-chars 50)

(defun xah-forward-n-chars ()
  "`forward-char' `xah-forward-n-chars' times."
  (interactive)
  (forward-char xah-forward-n-chars))

(defun xah-backward-n-chars ()
  "`backward-char' `xah-forward-n-chars' times."
  (interactive)
  (backward-char xah-forward-n-chars))


(defun xah-mouse-click-to-search (_click)
  "Mouse click to start `isearch-forward-symbol-at-point' (emacs 24.4) at clicked point.
URL `http://ergoemacs.org/emacs/emacs_mouse_click_highlight_word.html'
Version 2015-04-22"
  (interactive "e")
  (let ((p1 (posn-point (event-start _click))))
    (goto-char p1)
    (isearch-forward-symbol-at-point)))

(defun xah-click-describe-char (_click)
  "Mouse click to `describe-char' at clicked point.
URL `http://ergoemacs.org/emacs/emacs_mouse_wheel_config.html'
Version 2015-04-22"
  (interactive "e")
  (let ((p1 (posn-point (event-start _click))))
    (goto-char p1)
    (describe-char p1)))

(defun xah-set-mouse-wheel-normal ()
  "Set mouse wheel to `mwheel-scroll'"
  (interactive)
  (cond
   ((string-equal system-type "gnu/linux")
    (progn
      (global-set-key (kbd "<mouse-4>") 'mwheel-scroll)
      (global-set-key (kbd "<mouse-5>") 'mwheel-scroll)))
   ((string-equal system-type "windows-nt") ; Windows
    (progn
      (global-set-key (kbd "<wheel-up>") 'mwheel-scroll)
      (global-set-key (kbd "<wheel-down>") 'mwheel-scroll)))
   ((string-equal system-type "darwin") ; Mac
    (progn
      (global-set-key (kbd "<wheel-up>") 'mwheel-scroll)
      (global-set-key (kbd "<wheel-down>") 'mwheel-scroll))))
  (message "Mouse wheel set to normal"))

(defun xah-set-mouse-scroll-by-50-line ()
  "Set mouse wheel to move cursor by n lines.
Version 2015-07-06"
  (interactive)
  (cond
   ((string-equal system-type "gnu/linux")
    (progn
      (global-set-key (kbd "<mouse-4>") 'xah-cursor-up-50-lines)
      (global-set-key (kbd "<mouse-5>") 'xah-cursor-down-50-lines)))
   ((string-equal system-type "windows-nt") ; Windows
    (progn
      (global-set-key (kbd "<wheel-up>") 'xah-cursor-up-50-lines)
      (global-set-key (kbd "<wheel-down>") 'xah-cursor-down-50-lines)))
   ((string-equal system-type "darwin") ; Mac
    (progn
      (global-set-key (kbd "<wheel-up>") 'xah-cursor-up-50-lines)
      (global-set-key (kbd "<wheel-down>") 'xah-cursor-down-50-lines))))
  (message "Mouse wheel set to move by 50 lines."))

(defun xah-set-mouse-scroll-by-block ()
  "Set mouse wheel to scroll by text block.
Version 2015-07-06"
  (interactive)
  (cond
   ((string-equal system-type "gnu/linux")
    (progn
      (global-set-key (kbd "<mouse-4>") 'xah-backward-block)
      (global-set-key (kbd "<mouse-5>") 'xah-forward-block)))
   ((string-equal system-type "windows-nt") ; Windows
    (progn
      (global-set-key (kbd "<wheel-up>") 'xah-backward-block)
      (global-set-key (kbd "<wheel-down>") 'xah-forward-block)))
   ((string-equal system-type "darwin") ; Mac
    (progn
      (global-set-key (kbd "<wheel-up>") 'xah-backward-block)
      (global-set-key (kbd "<wheel-down>") 'xah-forward-block))))
  (message "Mouse wheel set to move by block."))

(defun xah-set-mouse-wheel-mode ()
  "Set mouse wheel to move by line, block or other.
This command will prompt you.
When emacs is idle for 10 seconds, the normal wheel behavior will be restored."
  (interactive)
  (let (-mode
        (-mouse-wheel-modes
         '(
           "50 lines"
           "block"
           "normal"
           )))
    (setq -mode (ido-completing-read "set wheel mode to:" -mouse-wheel-modes))
    (cond
     ((string-equal -mode "normal") (xah-set-mouse-wheel-normal))
     ((string-equal -mode "block") (xah-set-mouse-scroll-by-block))
     ((string-equal -mode "50 lines") (xah-set-mouse-scroll-by-50-line))
     (t (error "%s" "program logic error. No choice found")))

    (message "Wheel behavior will revert back to normal 10 seconds after idle.")

    (setq xah-fly-mouse-mode-timer-id (run-with-idle-timer 10 nil 'xah-set-mouse-wheel-normal))))
