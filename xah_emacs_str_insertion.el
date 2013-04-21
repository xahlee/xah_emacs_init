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

(defun insert-date (&optional prefixArtCode)
  "Insert current date and or time,
in the format yyyy-mm-dd.

When called with `universal-argument', insert date and time, e.g. 2012-05-28T07:06:23-07:00

See also `current-date-time-string'."
  (interactive "P")
  (when (region-active-p)
    (delete-region (region-beginning) (region-end) ) )

  (cond
   ((equal prefixArtCode nil ) (insert (format-time-string "%Y-%m-%d")))
   (t (current-date-time-string)) ) )

;; (put 'insert-date 'delete-selection t)

(defun insert-date-time ()
  "Insert current date-time string in full ISO 8601 format.
Example: 2010-11-29T23:23:35-08:00

Replaces currents text selection if there's one.
This function calls: `current-date-time-string'."
  (interactive)
  (when (region-active-p)
    (delete-region (region-beginning) (region-end) )
    )
  (insert (current-date-time-string)))

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
