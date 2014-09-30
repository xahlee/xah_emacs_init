;; -*- coding: utf-8 -*-
;; 2014-01-05

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
