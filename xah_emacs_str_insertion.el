;; -*- coding: utf-8 -*-
;; some custome string insertion functions
;; 2007-06, 2011-05-27
;; Xah Lee,
;; http://ergoemacs.org/emacs/xah_emacs_init.html

(defun insert-column-counter (n)
  "Insert a sequence of numbers vertically.
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

This command is conveniently used together with `kill-rectangle' and `string-rectangle'."
  (interactive "nEnter the max integer: ")
  (let ((i 1) colpos )
    (setq colpos (- (point) (line-beginning-position)))
    (while (<= i n)
      (insert (number-to-string i))
      (forward-line) (beginning-of-line) (forward-char colpos)
      (setq i (1+ i))
      )
))

(defun insert-alphabets ()
  "Insert letters a to z.
Note: this command is similar to `rectangle-number-lines' with a format of 「%c」."
  (interactive)
  (dotimes (ii 26 )
(insert (format "%c\n" (+ 97 ii)))
))

(defun insert-unicode-drawing-box ()
  "Insert a drawing box of Unicode chars."
  (interactive)
  (insert "
┌─┬─┐
│ │ │
├─┼─┤
│ │ │
└─┴─┘
"))




