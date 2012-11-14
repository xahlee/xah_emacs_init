;; -*- coding: utf-8 -*-
;; some general commands related to editing text

;; 2011-05-27
;;   Xah Lee
;; ∑ http://xahlee.org/

(defun copy-file-path (prefixArgCode)
  "Copy the current buffer's file path or dired path to `kill-ring'.
If `universal-argument' is called, copy only the dir path."
  (interactive "P")
  (let ((fPath
         (if (equal major-mode 'dired-mode)
             default-directory
           (buffer-file-name)
           )))
    (kill-new 
     (if (equal prefixArgCode nil)
         fPath
       (file-name-directory fPath)
       )))  
  (message "File path copied.") )

(defun compact-parens ()
  "Removing white spaces in ending parenthesises.
Removes white space from cursor point to end of code block (\n\n).
Or, act on a text selection.
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

    (setq resultText (replace-regexp-pairs-in-string (buffer-substring-no-properties p1 p2) '(["[ \t\n]+)[ \t\n]+" " )"])) )

    (delete-region p1 p2)
    (insert resultText) ) )



;; (defun my-delete-word (arg)
;;   "Delete characters forward until encountering the end of a word.
;; With argument, do this that many times.
;; This command does not push erased text to kill-ring."
;;   (interactive "p")
;;   (delete-region (point) (progn (forward-word arg) (point))))

;; (defun my-backward-delete-word (arg)
;;   "Delete characters backward until encountering the beginning of a word.
;; With argument, do this that many times.
;; This command does not push erased text to kill-ring."
;;   (interactive "p")
;;   (my-delete-word (- arg)))

;; (defun my-delete-line ()
;;   "Delete text from current position to end of line char."
;;   (interactive)
;;   (delete-region
;;    (point)
;;    (save-excursion (move-end-of-line 1) (point)))
;;   (delete-char 1)
;; )

