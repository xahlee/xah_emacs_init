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



(defun xah-next-user-buffer ()
  "Switch to the next user buffer.
User buffers are those whose name does not start with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (string-equal "*" (substring (buffer-name) 0 1)) (< i 20))
      (setq i (1+ i)) (next-buffer))))

(defun xah-previous-user-buffer ()
  "Switch to the previous user buffer.
User buffers are those whose name does not start with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (string-equal "*" (substring (buffer-name) 0 1)) (< i 20))
      (setq i (1+ i)) (previous-buffer) )))

(defun xah-next-emacs-buffer ()
  "Switch to the next emacs buffer.
Emacs buffers are those whose name starts with *."
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (and (not (string-equal "*" (substring (buffer-name) 0 1))) (< i 20))
      (setq i (1+ i)) (next-buffer) )))

(defun xah-previous-emacs-buffer ()
  "Switch to the previous emacs buffer.
Emacs buffers are those whose name starts with *."
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (and (not (string-equal "*" (substring (buffer-name) 0 1))) (< i 20))
      (setq i (1+ i)) (previous-buffer) )))



(defvar xah-recently-closed-buffers (cons nil nil) "A list of recently closed buffers. The max number to track is controlled by the variable `xah-recently-closed-buffers-max'.")
(defvar xah-recently-closed-buffers-max 40 "The maximum length for `xah-recently-closed-buffers'.")

(defun xah-close-current-buffer ()
  "Close the current buffer.

Similar to (kill-buffer (current-buffer)) with the following addition:

• Prompt user to save if the buffer has been modified even if the buffer is not associated with a file.
• Make sure the buffer shown after closing is a user buffer.
• If the buffer is editing a source file in an org-mode file, prompt the user to save before closing.
• If the buffer is a file, add the path to the list `xah-recently-closed-buffers'.
• If it is the minibuffer, exit the minibuffer

A emacs buffer is one who's name starts with *.
Else it is a user buffer."
  (interactive)
  (let (emacsBuff-p
        isEmacsBufferAfter
        (org-p (string-match "^*Org Src" (buffer-name))))

    (setq emacsBuff-p (if (string-match "^*" (buffer-name)) t nil) )

    (if (string= major-mode "minibuffer-inactive-mode")
        (minibuffer-keyboard-quit) ; if the buffer is minibuffer
      (progn
        ;; offer to save buffers that are non-empty and modified, even
        ;; for non-file visiting buffer. (because kill-buffer does not
        ;; offer to save buffers that are not associated with files)
        (when (and (buffer-modified-p)
                   (not emacsBuff-p)
                   (not (string-equal major-mode "dired-mode"))
                   (if (equal (buffer-file-name) nil)
                       (if (string-equal "" (save-restriction (widen) (buffer-string))) nil t)
                     t))
          (if (y-or-n-p (format "Buffer %s modified; Do you want to save? " (buffer-name)))
              (save-buffer)
            (set-buffer-modified-p nil)))
        ;;
        (when (and (buffer-modified-p)
                   org-p)
          (if (y-or-n-p (format "Buffer %s modified; Do you want to save? " (buffer-name)))
              (org-edit-src-save)
            (set-buffer-modified-p nil)))

        ;; save to a list of closed buffer
        (when (not (equal buffer-file-name nil))
          (setq xah-recently-closed-buffers
                (cons (cons (buffer-name) (buffer-file-name)) xah-recently-closed-buffers))
          (when (> (length xah-recently-closed-buffers) xah-recently-closed-buffers-max)
            (setq xah-recently-closed-buffers (butlast xah-recently-closed-buffers 1))))

        ;; close
        (kill-buffer (current-buffer))

        ;; if emacs buffer, switch to a user buffer
        (if (string-match "^*" (buffer-name))
            (setq isEmacsBufferAfter t)
          (setq isEmacsBufferAfter nil))
        (when isEmacsBufferAfter
          (xah-next-user-buffer) ) ))))

(defun xah-open-last-closed ()
  "Open the last closed file."
  (interactive)
  (find-file (cdr (pop xah-recently-closed-buffers)) ) )

(defun xah-list-recently-closed ()
  "List recently closed file."
  (interactive)
  (let ((buf (generate-new-buffer "*recently closed*")))
    (switch-to-buffer buf)
    (mapc (lambda (f) (insert (cdr f) "\n") )
          xah-recently-closed-buffers)))

(defun xah-open-recently-closed ()
  "Open recently closed file."
  (interactive)
  (find-file (ido-completing-read "open:" (mapcar (lambda (f) (cdr f)) xah-recently-closed-buffers))))



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
