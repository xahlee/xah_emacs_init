
(defun htmlize-comp-lang-source-code-string (sourceCodeStr langModeName)
  "Take SOURCECODESTR and return a htmlized version using LANGMODENAME.
This function requries the htmlize.el by Hrvoje Niksic."
  (require 'htmlize)
  (let (htmlizeOutputBuf p1 p2 resultStr)

    ;; put code in a temp buffer, set the mode, fontify
    (with-temp-buffer
      (insert sourceCodeStr)
      (funcall (intern langModeName))
      (font-lock-fontify-buffer)
      (setq htmlizeOutputBuf (htmlize-buffer))
      )

    ;; extract the fontified source code in htmlize output
    (with-current-buffer htmlizeOutputBuf
      (setq p1 (search-forward "<pre>"))
      (setq p2 (search-forward "</pre>"))
      (setq resultStr (buffer-substring-no-properties (+ p1 1) (- p2 6))))

    (kill-buffer htmlizeOutputBuf)
    resultStr
    ))

(defun htmlize-pre-block (langModeMap)
  "Replace text enclosed by <pre> tag to htmlized code.
For example, if the cursor is somewhere between the pre tags:
 <pre class=\"lang-code\">…▮…</pre>

after calling, the text inside the pre tag will be htmlized.
That is, wrapped with many span tags.

The opening tag must be of the form <pre class=\"lang-code\">.
The “lang-code” determines what emacs mode is used to colorize the
text.

 “lang-code” can be any of {c, elisp, java, js, html, xml, css, …}.
 (See source code for a full list)

See also: `dehtmlize-pre-block'.

This function requires htmlize.el by Hrvoje Niksic."
  (interactive (list ξ-language-name-map))
  (let (inputStr langCode p1 p2 modeName )

    (save-excursion
      (re-search-backward "<pre class=\"\\([-A-Za-z0-9]+\\)\"") ; tag begin position
      (setq langCode (match-string 1))
      (setq p1 (search-forward ">")) ; lang source code string begin 
      (search-forward "</pre>")
      (setq p2 (search-backward "<")) ; lang source code string end
      (search-forward "</pre>") ; tag end position
      (setq inputStr (buffer-substring-no-properties p1 p2))

      (setq modeName
            (let ((tempVar (assoc langCode langModeMap) ))
              (if tempVar (cdr tempVar) "text-mode" ) ) )

      (delete-region p1 p2)
      (goto-char p1)
      (insert (htmlize-comp-lang-source-code-string inputStr modeName)) ) ) )
