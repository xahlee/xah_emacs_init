;; -*- coding: utf-8 -*-
;; some general commands related to editing text

;; 2011-05-27
;;   Xah Lee
;; ∑ http://xahlee.org/

(defun xah-copy-file-path (&optional dirPathOnly-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
If `universal-argument' is called, copy only the dir path."
  (interactive "P")
  (let ((fPath
         (if (equal major-mode 'dired-mode)
             default-directory
           (buffer-file-name)
           )))
    (kill-new
     (if (equal dirPathOnly-p nil)
         fPath
       (file-name-directory fPath)
       )))
  (message "File path copied.") )

(defun xah-delete-cut-text-block ()
  "delete the current text block (paragraph) and also put it to `kill-ring'."
  (interactive)
  (let (p1 p2)
    (progn
      (if (re-search-backward "\n[ \t]*\n" nil "NOERROR")
          (progn (re-search-forward "\n[ \t]*\n")
                 (setq p1 (point) ) )
        (setq p1 (point)))
      (if (re-search-forward "\n[ \t]*\n" nil "NOERROR")
          (progn (re-search-backward "\n[ \t]*\n")
                 (setq p2 (point) ))
        (setq p2 (point) ) ) )
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
         (p2 (elt bds 2) )
         )
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
       (buffer-string) )) ) )

