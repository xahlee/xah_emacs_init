;; -*- coding: utf-8 -*-
;; some custome string insertion functions
;; 2007-06, 2011-05-27
;; Xah Lee,
;; http://ergoemacs.org/emacs/xah_emacs_init.html

(random t)

(defun insert-random-hex ()
  "Insert a random 4-digit hexidecimal number."
  (interactive)
  (let (myCharset (possibleCharsCount 16))
    (setq myCharset "1234567890abcdef" )
    (dotimes (ii 4)
      (insert (elt myCharset (random possibleCharsCount))) ) )
  ;; (insert (format "%4x" (random 65535)) )
  )

(defun insert-random-string ()
  "Insert a random alphanumerics string of length 5.
The possible chars are 0 to 9, and a to z (lower case)."
  (interactive)
  (let (myCharset (possibleCharsCount 36))
    (setq myCharset "1234567890abcdefghijklmnopqrstuvwxyz" )
    (dotimes (ii 5)
      (insert (elt myCharset (random possibleCharsCount))) ) ) )

(defun insert-random-uuid ()
  "Insert a random universally unique identifier (UUID).

UUID is a 32 digits hexadecimal formatted in certain way with dash.
Example of a UUID: 1df63142-a513-c850-31a3-535fc3520c3d
."
  (interactive)
  (insert
   (format "%04x%04x-%04x-%04x-%04x-%06x%06x"
           (random (expt 16 4))
           (random (expt 16 4))
           (random (expt 16 4))
           (random (expt 16 4))
           (random (expt 16 4))
           (random (expt 16 6))
           (random (expt 16 6)) ) ) )

;; primarily Christopher Wellons. 2011-11-18
(defun insert-random-uuid-2 ()
  "Insert a UUID. This uses a simple hashing of variable data."
  (interactive)
  (let ((myStr (md5 (format "%s%s%s%s%s%s%s%s%s%s"
                        (user-uid)
                        (emacs-pid)
                        (system-name)
                        (user-full-name)
                        (current-time)
                        (emacs-uptime)
                        (garbage-collect)
                        (buffer-string)
                        (random)
                        (recent-keys)))))

    (insert (format "%s-%s-4%s-a%s-%s"
                    (substring myStr 0 8)
                    (substring myStr 8 12)
                    (substring myStr 13 16)
                    (substring myStr 17 20)
                    (substring myStr 20 32)))))


;; (defun insert-random-number ()
;;   "Insert a random number."
;;   (interactive)
;;   (insert (number-to-string (random )))
;;   )

(defun insert-random-number ()
  "Insert a random number of length 5."
  (interactive)
  (let (myCharset (possibleCharsCount 10))
    (setq myCharset "1234567890" )
    (dotimes (ii 5)
      (insert (elt myCharset (random possibleCharsCount))) ) ) )

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
