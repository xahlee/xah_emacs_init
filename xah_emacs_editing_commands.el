;; -*- coding: utf-8 -*-
;; some general commands related to editing text

;; 2011-05-27
;;   Xah Lee
;; ∑ http://xahlee.org/

(defun xah-copy-file-path (&optional φdirPathOnly-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
If `universal-argument' is called, copy only the dir path."
  (interactive "P")
  (let ((fPath
         (if (equal major-mode 'dired-mode)
             default-directory
           (buffer-file-name)
           )))
    (kill-new
     (if (equal φdirPathOnly-p nil)
         fPath
       (file-name-directory fPath)
       )))
  (message "File path copied."))

(defun xah-delete-cut-text-block ()
  "delete the current text block (paragraph) and also put it to `kill-ring'."
  (interactive)
  (let (p1 p2)
    (progn
      (if (re-search-backward "\n[ \t]*\n" nil "NOERROR")
          (progn (re-search-forward "\n[ \t]*\n")
                 (setq p1 (point)))
        (setq p1 (point)))
      (if (re-search-forward "\n[ \t]*\n" nil "NOERROR")
          (progn (re-search-backward "\n[ \t]*\n")
                 (setq p2 (point) ))
        (setq p2 (point))) )
    (kill-region p1 p2)
    (delete-blank-lines) ))

(defun xah-copy-to-register-1 ()
  "Copy current line or text selection to register 1.
See also: `xah-paste-from-register-1', `copy-to-register'."
  (interactive)
  (let* (
         (bds (get-selection-or-unit 'line ))
         (inputStr (elt bds 0) )
         (p1 (elt bds 1) )
         (p2 (elt bds 2)))
    (copy-to-register ?1 p1 p2)
    (message "copied to register 1: 「%s」." inputStr)
))

(defun xah-paste-from-register-1 ()
  "Paste text from register 1.
See also: `xah-copy-to-register-1', `insert-register'."
  (interactive)
  (when (region-active-p)
    (delete-region (region-beginning) (region-end) )
    )
  (insert-register ?1 t))

(defun xah-compact-parens ()
  "Removing whitespaces in ending repetition of parenthesises.
Removes whitespace from cursor point to end of code block (that is, 2 or more blank lines.).
if there's a text selection, act on the region.
Warning: This command does not preserve texts inside double quotes."
  (interactive)
  (let (inputStr resultText p1 p2)

    (setq inputStr
          (if (region-active-p)
              (progn (setq p1 (region-beginning) ) (setq p2 (region-end) ))
            (save-excursion
              (setq p1 (point) )
              (search-forward-regexp "\n\n" nil t)
              (setq p2 (- (point) 2))
              )))
    (save-excursion 
      (save-restriction 
        (narrow-to-region p1 p2)
        (goto-char (point-min))

        (while (search-forward-regexp "[ \t\n]+)[ \t\n]+)" nil t) (replace-match "))"))
        ))))



(defun xah-copy-rectangle-to-clipboard (p1 p2)
  "Copy region as column (rectangle) to operating system's clipboard.
This command will also put the text in register 0.

See also: `kill-rectangle', `copy-to-register'."
  (interactive "r")
  (let ((x-select-enable-clipboard t))
    (copy-rectangle-to-register ?0 p1 p2)
    (kill-new
     (with-temp-buffer
       (insert-register ?0)
       (buffer-string) ))))

(defun xah-insert-form-feed ()
  "insert a form feed char (ASCII 12)"
  (interactive)
  (insert ""))

