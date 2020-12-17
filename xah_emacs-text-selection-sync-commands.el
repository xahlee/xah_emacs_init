;; -*- coding: utf-8; lexical-binding: t; -*-

(defun xah-select-text-in-quote-by-syntax-table ()
  "Select text between ASCII quotes, single or double."
  (interactive)
  (let (p1 p2 (parse-sexp-lookup-properties nil)
           ($temp-syn-table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" $temp-syn-table)
    (with-syntax-table $temp-syn-table
      (if (nth 3 (syntax-ppss))
          (progn
            (if (>= emacs-major-version 25)
                (backward-up-list 1 "ESCAPE-STRINGS" "NO-SYNTAX-CROSSING")
              (backward-up-list 1))
            (setq p1 (point))
            (forward-sexp 1)
            (setq p2 (point))
            (goto-char (1+ p1))
            (set-mark (1- p2)))
        (progn
          (user-error "Cursor not inside quote"))))))

(defun xah-select-text-in-bracket-or-quote-by-syntax-table ()
  "Select text between the nearest brackets or quote.
 2016-12-19"
  (interactive)
  (let ((parse-sexp-lookup-properties nil)
            ($temp-syn-table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" $temp-syn-table)
    (modify-syntax-entry ?\« "(»" $temp-syn-table)
    (modify-syntax-entry ?\» ")«" $temp-syn-table)
    (modify-syntax-entry ?\‹ "(›" $temp-syn-table)
    (modify-syntax-entry ?\› ")‹" $temp-syn-table)
    (modify-syntax-entry ?\“ "(”" $temp-syn-table)
    (modify-syntax-entry ?\” ")“" $temp-syn-table)
    (when (or
           (string-equal major-mode "xah-html-mode")
           (string-equal major-mode "xml-mode")
           (string-equal major-mode "nxml-mode")
           (string-equal major-mode "html-mode"))
      (modify-syntax-entry ?\> "(<" $temp-syn-table)
      (modify-syntax-entry ?\< ")>" $temp-syn-table))

    (with-syntax-table $temp-syn-table
      (if (nth 3 (syntax-ppss))
          (xah-select-text-in-quote-by-syntax-table)
        (xah-select-text-in-bracket-by-syntax-table)))))

(defun xah-select-text-in-bracket-by-syntax-table ()
  "Select text between the nearest brackets.
eg  () [] {} «» ‹› “” 〖〗 【】 「」 『』 （） 〈〉 《》 〔〕 ⦗⦘ 〘〙 ⦅⦆ 〚〛 ⦃⦄ ⟨⟩."
  (interactive)
  (let ( p1 p2 (parse-sexp-lookup-properties nil)
            ($temp-syn-table (make-syntax-table)))
    (modify-syntax-entry ?\" "\"" $temp-syn-table)
    (modify-syntax-entry ?\« "(»" $temp-syn-table)
    (modify-syntax-entry ?\» ")«" $temp-syn-table)
    (modify-syntax-entry ?\‹ "(›" $temp-syn-table)
    (modify-syntax-entry ?\› ")‹" $temp-syn-table)
    (modify-syntax-entry ?\“ "(”" $temp-syn-table)
    (modify-syntax-entry ?\” ")“" $temp-syn-table)
    (when (or
           (string-equal major-mode "xah-html-mode")
           (string-equal major-mode "xml-mode")
           (string-equal major-mode "nxml-mode")
           (string-equal major-mode "html-mode"))
      (modify-syntax-entry ?\> "(<" $temp-syn-table)
      (modify-syntax-entry ?\< ")>" $temp-syn-table))

    (with-syntax-table $temp-syn-table
      (search-backward-regexp "\\s(" nil t )
      (setq p1 (point))
      (forward-sexp 1)
      (setq p2 (point))

      (goto-char (1+ p1))
      (set-mark (1- p2)))))

