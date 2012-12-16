(defun htmlize-text (ξstring major-mode-name &optional from-to-positions-pair)
  "HTMLize text. That is, syntax color by adding span tags.

When called interactively, work on current pre tag text block or text selection.
A “pre tag text block” is text enclosed by the tags
 <pre class=\"lang-code\"> …▮…</pre>

after calling, the text inside the pre tag will be htmlized.
The “lang-code” can be any of {c, bash, cl, clojure, elisp, haskell, html, xml, js, …}.
See source code for exact list.

If there's a text selection, ask for a major-mode name and htmlize that region.

When called in lisp code:
First argument ΞSTRING is the input string.
Second argument MAJOR-MODE-NAME should be a possible value of the variable `major-mode'.
If third argument FROM-TO-POSITIONS-PAIR is nil, the function returns a string, else, work on that region.
It should be a vector or list for positions of the region,
 e.g. [pos1 pos2]

This function requires the `htmlize-buffer' from 〔htmlize.el〕 by Hrvoje Niksic."
  (interactive
   (let (ξlangCode ξmajorMode p1 p2 langNameMap)

     (if (region-active-p)
         (progn
           (setq ξmajorMode (read-from-minibuffer "Major mode name:" nil nil nil nil "text-mode" nil))
           (list nil ξmajorMode (vector (region-beginning) (region-end)))
           )
       (progn
         (save-excursion
           (re-search-backward "<pre class=\"\\([-A-Za-z0-9]+\\)\">")
           (setq ξlangCode (match-string 1))
           (setq ξmajorMode (cdr (assoc ξlangCode langNameMap)))
           (setq p1 (search-forward ">"))
           (setq p2 (- (search-forward "</pre>") 6) )
           (list nil ξmajorMode (vector p1 p2) ) ) ) ) ) )

;; (when (not ξmajorMode) (error "Your language string 「class=\"%s\"」 doesn't match any supported language string. Supported language string are %S" ξmajorMode langNameMap ))

  (let (workOnStringP inputStr outputStr
                      (ξfrom (elt from-to-positions-pair 0))
                      (ξto (elt from-to-positions-pair 1))
                      )
    (setq workOnStringP (if from-to-positions-pair nil t))
    ;; (insert-buffer-substring BUFFER &optional START END)

    (setq inputStr (if workOnStringP ξstring (buffer-substring-no-properties ξfrom ξto)))
    (setq outputStr
          (let (htmlizeOutputBuffer resultStr)
            ;; put code in a temp buffer, set the mode, fontify
            (with-temp-buffer
              (insert inputStr)
              (funcall (intern major-mode-name))
              (font-lock-fontify-buffer)
              (setq htmlizeOutputBuffer (htmlize-buffer))
              )
            ;; extract the fontified source code in htmlize output
            (with-current-buffer htmlizeOutputBuffer
              (let (p1 p2 )
                (setq p1 (search-forward "<pre>"))
                (setq p2 (search-forward "</pre>"))
                (setq resultStr (buffer-substring-no-properties (+ p1 1) (- p2 6))) ) )
            (kill-buffer htmlizeOutputBuffer)
            resultStr ) )

    (if workOnStringP
        outputStr
      (save-excursion
        (delete-region ξfrom ξto)
        (goto-char ξfrom)
        (insert outputStr) )) ) )
