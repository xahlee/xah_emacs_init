;; 2014-05-30

(defun σ-syntax-bracket-forward ()
  "Move curvor to the 1st closing bracket, according to current syntax table."
  (interactive)
  (let ()
    (skip-syntax-forward "^)")
    (forward-char 1)
  ))

(defun σ-syntax-bracket-backward ()
  "Move curvor previous opening bracket, according to current syntax table."
  (interactive)
  (let ()
    (skip-syntax-backward "^(")
    (backward-char 1)
    (backward-char 1)
  ))

(defun σ-forward-comment (φcount)
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

(defun σ-scan-list (φcount φcurrent-depth)
  "Call `scan-lists' interactively."
  (interactive "nCount:\nnDepth:")
  (let ((parse-sexp-ignore-comments t))
    (goto-char (scan-lists (point) φcount φcurrent-depth))
  ))

(defun σ-scan-sexps (φcount)
  "Call `scan-sexps' interactively.
note: (scan-sexps n) is equivalent to (scan-list n 0) , i think."
  (interactive "nCount:")
  (let ((parse-sexp-ignore-comments t))
    (goto-char (scan-sexps (point) φcount))
  ))

(defun σ-parse-partial-sexp ()
  "Call `syntax-ppss' or `parse-partial-sexp'.
Also print it results. Return the parser state.

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
  (let ((parse-sexp-ignore-comments t)
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
