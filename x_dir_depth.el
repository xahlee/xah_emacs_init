;2008-06-27

;problem link: /Users/xah/web/diklo/xah_emacs_tutorial/emacs/jargons_high_level_lang.html  /Users/xah/web/diklo/xah_emacs_tutorial/cmaci/notation/math_namings.html

(defun getDirDepth (myPath myRoot)
  "Return the level of the path with respect to root path.

• Both argument must be full path.
• myPath can be a path to dir or file.
• myRoot is a root dir path.
• Paths must be given without ending slash, even if it is a dir.
• myPath must be a subdir of myRoot. That is, both paths share the beginning strings.

Example cases:
/Users/xah/web/diklo  ← myPath
/Users/xah/web        ← myRoot
returns 1

/Users/xah/web/book.html
/Users/xah/web
returns 1

/Users/xah
/Users/xah/web
returns -1

This function works with unix path only."
  (let (charsP charsR tmpStr)
    (setq charsP (length myPath))
    (setq charsR (length myRoot))
    (if (> charsP charsR)
        (progn
          (setq tmpStr (substring myPath charsR))
          (count ?/ tmpStr)
          )
      (progn
        (setq tmpStr (substring myRoot charsP))
        (- 0 (count ?/ tmpStr))
        )
      )
    ))
