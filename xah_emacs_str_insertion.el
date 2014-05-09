;; -*- coding: utf-8 -*-
;; some custome string insertion functions
;; 2007-06, 2011-05-27
;; Xah Lee,
;; http://ergoemacs.org/emacs/xah_emacs_init.html

(defun xah-insert-column-counter (n)
  "Insert a sequence of numbers vertically.

 (this command is similar to emacs 24.x's `rectangle-number-lines'.)

For example, if your text is:

a b
c d
e f

and your cursor is after “a”, then calling this function with argument
3 will change it to become:

a1 b
c2 d
e3 f

If there are not enough existing lines after the cursor
when this function is called, it aborts at the last line.

This command is conveniently used together with `kill-rectangle' and `string-rectangle'.
"
  (interactive "nEnter the max integer: ")
  (let ((i 1) colpos )
    (setq colpos (- (point) (line-beginning-position)))
    (while (<= i n)
      (insert (number-to-string i))
      (forward-line) (beginning-of-line) (forward-char colpos)
      (setq i (1+ i))
      )
))

(defun xah-insert-alphabets-az (&optional useUppercase-p)
  "Insert letters a to z vertically.
If `universal-argument' is called first, use CAPITAL letters.
Note: this command is similar to `rectangle-number-lines', starting at 65 or 97, and with a format of 「%c」."
  (interactive "P")
  (let ((startChar (if useUppercase-p 65 97 )))
    (dotimes (ii 26 )
      (insert (format "%c\n" (+ startChar ii)))
      ) ) )

(defun xah-insert-unicode-drawing-box ()
  "Insert a drawing box of Unicode chars."
  (interactive)
  (insert "
┌─┬─┐
│ │ │
├─┼─┤
│ │ │
└─┴─┘
"))
