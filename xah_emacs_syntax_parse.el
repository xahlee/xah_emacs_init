;; 2014-05-30

(defun xah-syntax-bracket-forward ()
  "Move cursor to the 1st closing bracket, according to current syntax table.
This command is dumb, it'll not ignore brackets inside comment or string."
  (interactive)
  (let ((ξdist (skip-syntax-forward "^)")) )
    (message "Distance traveled: %s" ξdist)
    (forward-char 1)
  ))

(defun xah-syntax-bracket-backward ()
  "Move cursor to previous opening bracket, according to current syntax table.
This command is dumb, it'll not ignore brackets inside comment or string."
  (interactive)
  (let ((ξdist (skip-syntax-backward "^(")))
    (message "Distance traveled: %s" ξdist)
    (backward-char 1)
  ))

(defun xah-forward-comment (φcount)
  "Call `forward-comment' interactively.

when called interactively with no argument, call
 (forward-comment 1)
Or with argument from `universal-argument'.

Note: a comment is considered a unit, starting with comment char and end with comment end char.
The command ignore whether cursor is currently inside a comment, also ignore if cursor is inside a string (stupid). It just go forward (when φcount is positive) and look for commet start char.

test cases:

\"so ▮; x\"
"
  (interactive "p")
  (progn
    (prin1 (forward-comment φcount))
  ))

(defun xah-scan-list (φcount φcurrent-depth)
  "Call `scan-lists' interactively."
  (interactive "nCount:\nnDepth:")
  (let ((parse-sexp-ignore-comments t))
    (goto-char (scan-lists (point) φcount φcurrent-depth))
  ))

(defun xah-scan-sexps ()
  "Call `scan-sexps' interactively.
Call this (scan-sexps (point) 1)
It returns a position.
Note: (scan-sexps n) is equivalent to (scan-list n 0) , i think.
 `scan-list'

scan-sexps just move thru complete sexp units and stop on anything that's mis-matched brackets.
• Atom without paren counts as 1 sexp.
• atoms enclosed by a well-matched paren is 1 sexp.
For example, the following are all 1 sexp.

 ▮x
 ▮(y)
 ▮((z))
 ▮((z) ())
 ▮\"\"
 ▮\"x\"

scan-sexps will error if encounting on unmatched paren, ⁖

 ▮( x
 ▮ x )

"
  (interactive)
  (let ((parse-sexp-ignore-comments t))
    (goto-char (scan-sexps (point) 1))
  ))

(defun xah-parse-partial-sexp ()
  "Call `syntax-ppss' or `parse-partial-sexp'.
Also print its result. Return the parser state.

If no `universal-argument' received, call
 (syntax-ppss (point))
Else, call
 (parse-partial-sexp (point-min) (point))

Note:
 (syntax-ppss POS)
 is basically equivalent to
 (parse-partial-sexp (point-min) pos)
"
  (interactive)
  (let* ((parse-sexp-ignore-comments t)
        (parse-sexp-lookup-properties parse-sexp-lookup-properties)
        (ξstate
         (if current-prefix-arg
             (progn (message "parse-partial-sexp called")
                    (parse-partial-sexp (point-min) (point))
                    )
           (progn (message "syntax-ppss called")
                  (syntax-ppss (point))
                  )
           ) ) )

    (print (format
"0 depth: %S
1 innermost open bracket: %S
 2 last start complete sexp: %S
3 inside string: %S
4 inside comment: %S
5 is after a quote: %S
 6 min paren-depth: %S
7 comment style: %S
8 comment/string start: %S"
            (nth 0 ξstate )
            (nth 1 ξstate )
            (nth 2 ξstate )
            (nth 3 ξstate )
            (nth 4 ξstate )
            (nth 5 ξstate )
            (nth 6 ξstate )
            (nth 7 ξstate )
            (nth 8 ξstate ))
           )

    ξstate

    ))
