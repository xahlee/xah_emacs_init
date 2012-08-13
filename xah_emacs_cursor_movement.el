;; -*- coding: utf-8 -*-
;; some general cursor movement commands
;; 2011-05-27
;;   Xah Lee
;; ∑ http://xahlee.org/


;; (defun forward-section ()
;;   "Move cursor forward to next occurrence of the SECTION SIGN § char (unicode 167)."
;;   (interactive)
;;   (when (not (search-forward-regexp "§" nil t))
;;     (goto-char (point-max)) ) )

;; (defun backward-section ()
;;   "Move cursor forward to previous occurrence of the SECTION SIGN § char (unicode 167)."
;;   (interactive)
;;   (when (not (search-backward-regexp "§" nil t))
;;     (goto-char (point-min)) ) )


;; (defun goto-point-min ()
;;   "Goto the beginning of buffer.
;; This is different from `beginning-of-buffer'
;; because that marks the previous position."
;;   (interactive)
;;   (goto-char (point-min))
;; )

;; (defun goto-point-max ()
;;   "Goto the end of buffer.
;; This is different from `end-of-buffer'
;; because that marks the previous position."
;;   (interactive)
;;   (goto-char (point-max))
;; )

;; (defun search-pagebreak-next ()
;;   "Move cursor to the next occurrence of form feed character ^L (ascii 12).
;; This command is a replacement for forward-page.
;; forward-page does not work when the ^L character does not appear on a line by itself."
;;   (interactive)
;;   (search-forward "" nil t))

;; (defun search-pagebreak-prev ()
;;   "Move cursor to the previous occurrence of form feed character ^L (ascii 12).
;; See: `search-pagebreak-next'."
;;   (interactive)
;;   (search-backward "" nil t))


;; 2011-07-24 from http://www.emacswiki.org/emacs/JohnConnors
(defvar point-stack nil "a list to store cursor position. Each entry is a list of the form (bufferObject pointPosition)")

(defun point-stack-push ()
  "Push current location and buffer info onto stack."
  (interactive)
  (message "Location marked.")
  (setq point-stack (cons (list (current-buffer) (point)) point-stack)))

(defun point-stack-pop ()
  "Pop a location off the stack and move to buffer"
  (interactive)
  (if (null point-stack)
      (message "Stack is empty.")
    (switch-to-buffer (caar point-stack))
    (goto-char (cadar point-stack))
    (setq point-stack (cdr point-stack))))
