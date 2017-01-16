;; -*- coding: utf-8; lexical-binding: t; -*-
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
