;-*- coding: utf-8 -*-
; A collection of elisp functions

; 2008-11-20
;   Xah Lee
; ∑ http://xahlee.org/


(require 'xfrp_find_replace_pairs)
(require 'xeu_elisp_util)

(defun replace-html-named-entities (ξstring &optional ξfrom ξto)
  "Replace HTML entities to Unicode character.
For example, “&copy;” becomes “©”.

When called interactively, work on current text block or text selection. (a “text block” is text between empty lines)

When called in lisp code, if ξstring is non-nil, returns a changed string.  If ξstring nil, change the text in the region between positions ξfrom ξto.

The following HTML Entities are not replaced:
 &amp; &
 &lt; <
 &gt; >"
  (interactive
   (if (region-active-p)
       (list nil (region-beginning) (region-end))
     (let ((bds (get-selection-or-unit 'block)) )
       (list nil (elt bds 1) (elt bds 2))) ) )

  (let (workOnStringP inputStr outputStr)
    (setq workOnStringP (if ξstring t nil))
    (setq inputStr (if workOnStringP ξstring (buffer-substring-no-properties ξfrom ξto)))

    (setq outputStr
          (let ((case-fold-search nil))
            (replace-pairs-in-string inputStr
 [
  ["&nbsp;" " "]
  ["&ensp;" " "]
  ["&emsp;" " "]
  ["&thinsp;" " "]

  ["&rlm;" "‏"]
  ["&lrm;" "‎"]
  ["&zwj;" "‍"]
  ["&zwnj;" "‌"]

  ["&iexcl;" "¡"]
  ["&cent;" "¢"]
  ["&pound;" "£"]
  ["&curren;" "¤"]
  ["&yen;" "¥"]
  ["&brvbar;" "¦"]
  ["&sect;" "§"]
  ["&uml;" "¨"]
  ["&copy;" "©"]
  ["&ordf;" "ª"]
  ["&laquo;" "«"]
  ["&not;" "¬"]
  ["&shy;" "­"]
  ["&reg;" "®"]
  ["&macr;" "¯"]
  ["&deg;" "°"]
  ["&plusmn;" "±"]
  ["&sup2;" "²"]
  ["&sup3;" "³"]
  ["&acute;" "´"]
  ["&micro;" "µ"]
  ["&para;" "¶"]
  ["&middot;" "·"]
  ["&cedil;" "¸"]
  ["&sup1;" "¹"]
  ["&ordm;" "º"]
  ["&raquo;" "»"]
  ["&frac14;" "¼"]
  ["&frac12;" "½"]
  ["&frac34;" "¾"]
  ["&iquest;" "¿"]
  ["&Agrave;" "À"]
  ["&Aacute;" "Á"]
  ["&Acirc;" "Â"]
  ["&Atilde;" "Ã"]
  ["&Auml;" "Ä"]
  ["&Aring;" "Å"]
  ["&AElig;" "Æ"]
  ["&Ccedil;" "Ç"]
  ["&Egrave;" "È"]
  ["&Eacute;" "É"]
  ["&Ecirc;" "Ê"]
  ["&Euml;" "Ë"]
  ["&Igrave;" "Ì"]
  ["&Iacute;" "Í"]
  ["&Icirc;" "Î"]
  ["&Iuml;" "Ï"]
  ["&ETH;" "Ð"]
  ["&Ntilde;" "Ñ"]
  ["&Ograve;" "Ò"]
  ["&Oacute;" "Ó"]
  ["&Ocirc;" "Ô"]
  ["&Otilde;" "Õ"]
  ["&Ouml;" "Ö"]
  ["&times;" "×"]
  ["&Oslash;" "Ø"]
  ["&Ugrave;" "Ù"]
  ["&Uacute;" "Ú"]
  ["&Ucirc;" "Û"]
  ["&Uuml;" "Ü"]
  ["&Yacute;" "Ý"]
  ["&THORN;" "Þ"]
  ["&szlig;" "ß"]
  ["&agrave;" "à"]
  ["&aacute;" "á"]
  ["&acirc;" "â"]
  ["&atilde;" "ã"]
  ["&auml;" "ä"]
  ["&aring;" "å"]
  ["&aelig;" "æ"]
  ["&ccedil;" "ç"]
  ["&egrave;" "è"]
  ["&eacute;" "é"]
  ["&ecirc;" "ê"]
  ["&euml;" "ë"]
  ["&igrave;" "ì"]
  ["&iacute;" "í"]
  ["&icirc;" "î"]
  ["&iuml;" "ï"]
  ["&eth;" "ð"]
  ["&ntilde;" "ñ"]
  ["&ograve;" "ò"]
  ["&oacute;" "ó"]
  ["&ocirc;" "ô"]
  ["&otilde;" "õ"]
  ["&ouml;" "ö"]
  ["&divide;" "÷"]
  ["&oslash;" "ø"]
  ["&ugrave;" "ù"]
  ["&uacute;" "ú"]
  ["&ucirc;" "û"]
  ["&uuml;" "ü"]
  ["&yacute;" "ý"]
  ["&thorn;" "þ"]
  ["&yuml;" "ÿ"]
  ["&fnof;" "ƒ"]
  ["&Alpha;" "Α"]
  ["&Beta;" "Β"]
  ["&Gamma;" "Γ"]
  ["&Delta;" "Δ"]
  ["&Epsilon;" "Ε"]
  ["&Zeta;" "Ζ"]
  ["&Eta;" "Η"]
  ["&Theta;" "Θ"]
  ["&Iota;" "Ι"]
  ["&Kappa;" "Κ"]
  ["&Lambda;" "Λ"]
  ["&Mu;" "Μ"]
  ["&Nu;" "Ν"]
  ["&Xi;" "Ξ"]
  ["&Omicron;" "Ο"]
  ["&Pi;" "Π"]
  ["&Rho;" "Ρ"]
  ["&Sigma;" "Σ"]
  ["&Tau;" "Τ"]
  ["&Upsilon;" "Υ"]
  ["&Phi;" "Φ"]
  ["&Chi;" "Χ"]
  ["&Psi;" "Ψ"]
  ["&Omega;" "Ω"]
  ["&alpha;" "α"]
  ["&beta;" "β"]
  ["&gamma;" "γ"]
  ["&delta;" "δ"]
  ["&epsilon;" "ε"]
  ["&zeta;" "ζ"]
  ["&eta;" "η"]
  ["&theta;" "θ"]
  ["&iota;" "ι"]
  ["&kappa;" "κ"]
  ["&lambda;" "λ"]
  ["&mu;" "μ"]
  ["&nu;" "ν"]
  ["&xi;" "ξ"]
  ["&omicron;" "ο"]
  ["&pi;" "π"]
  ["&rho;" "ρ"]
  ["&sigmaf;" "ς"]
  ["&sigma;" "σ"]
  ["&tau;" "τ"]
  ["&upsilon;" "υ"]
  ["&phi;" "φ"]
  ["&chi;" "χ"]
  ["&psi;" "ψ"]
  ["&omega;" "ω"]
  ["&thetasym;" "ϑ"]
  ["&upsih;" "ϒ"]
  ["&piv;" "ϖ"]
  ["&bull;" "•"]
  ["&hellip;" "…"]
  ["&prime;" "′"]
  ["&Prime;" "″"]
  ["&oline;" "‾"]
  ["&frasl;" "⁄"]
  ["&weierp;" "℘"]
  ["&image;" "ℑ"]
  ["&real;" "ℜ"]
  ["&trade;" "™"]
  ["&alefsym;" "ℵ"]
  ["&larr;" "←"]
  ["&uarr;" "↑"]
  ["&rarr;" "→"]
  ["&darr;" "↓"]
  ["&harr;" "↔"]
  ["&crarr;" "↵"]
  ["&lArr;" "⇐"]
  ["&uArr;" "⇑"]
  ["&rArr;" "⇒"]
  ["&dArr;" "⇓"]
  ["&hArr;" "⇔"]
  ["&forall;" "∀"]
  ["&part;" "∂"]
  ["&exist;" "∃"]
  ["&empty;" "∅"]
  ["&nabla;" "∇"]
  ["&isin;" "∈"]
  ["&notin;" "∉"]
  ["&ni;" "∋"]
  ["&prod;" "∏"]
  ["&sum;" "∑"]
  ["&minus;" "−"]
  ["&lowast;" "∗"]
  ["&radic;" "√"]
  ["&prop;" "∝"]
  ["&infin;" "∞"]
  ["&ang;" "∠"]
  ["&and;" "∧"]
  ["&or;" "∨"]
  ["&cap;" "∩"]
  ["&cup;" "∪"]
  ["&int;" "∫"]
  ["&there4;" "∴"]
  ["&sim;" "∼"]
  ["&cong;" "≅"]
  ["&asymp;" "≈"]
  ["&ne;" "≠"]
  ["&equiv;" "≡"]
  ["&le;" "≤"]
  ["&ge;" "≥"]
  ["&sub;" "⊂"]
  ["&sup;" "⊃"]
  ["&nsub;" "⊄"]
  ["&sube;" "⊆"]
  ["&supe;" "⊇"]
  ["&oplus;" "⊕"]
  ["&otimes;" "⊗"]
  ["&perp;" "⊥"]
  ["&sdot;" "⋅"]
  ["&lceil;" "⌈"]
  ["&rceil;" "⌉"]
  ["&lfloor;" "⌊"]
  ["&rfloor;" "⌋"]
  ["&lang;" "〈"]
  ["&rang;" "〉"]
  ["&loz;" "◊"]
  ["&spades;" "♠"]
  ["&clubs;" "♣"]
  ["&hearts;" "♥"]
  ["&diams;" "♦"]
  ["&quot;" "\""]
  ["&OElig;" "Œ"]
  ["&oelig;" "œ"]
  ["&Scaron;" "Š"]
  ["&scaron;" "š"]
  ["&Yuml;" "Ÿ"]
  ["&circ;" "ˆ"]
  ["&tilde;" "˜"]
  ["&ndash;" "–"]
  ["&mdash;" "—"]
  ["&lsquo;" "‘"]
  ["&rsquo;" "’"]
  ["&sbquo;" "‚"]
  ["&ldquo;" "“"]
  ["&rdquo;" "”"]
  ["&bdquo;" "„"]
  ["&dagger;" "†"]
  ["&Dagger;" "‡"]
  ["&permil;" "‰"]
  ["&lsaquo;" "‹"]
  ["&rsaquo;" "›"]
  ["&euro;" "€"]
  ]
 )
            )  )

    (if workOnStringP
        outputStr
      (save-excursion
        (delete-region ξfrom ξto)
        (goto-char ξfrom)
        (insert outputStr) )) ) )

(defun remove-vowel-old (&optional ξstring ξfrom ξto)
  "Remove the following letters: {a e i o u}.

When called interactively, work on current text block or text selection. (a “text block” is text between empty lines)

When called in lisp code, if ξstring is non-nil, returns a changed string.  If ξstring nil, change the text in the region between positions ξfrom ξto."
  (interactive
   (if (region-active-p)
       (list nil (region-beginning) (region-end))
     (let ((bds (bounds-of-thing-at-point 'paragraph)) )
       (list nil (car bds) (cdr bds)) ) ) )

  (let (workOnStringP inputStr outputStr)
    (setq workOnStringP (if ξstring t nil))
    (setq inputStr (if workOnStringP ξstring (buffer-substring-no-properties ξfrom ξto)))
    (setq outputStr
          (let ((case-fold-search t))
            (replace-regexp-in-string "a\\|e\\|i\\|o\\|u\\|" "" inputStr) )  )

    (if workOnStringP
        outputStr
      (save-excursion
        (delete-region ξfrom ξto)
        (goto-char ξfrom)
        (insert outputStr) )) ) )

(defun remove-vowel (ξstring &optional ξfrom-to-pair)
  "Remove the following letters: {a e i o u}.

When called interactively, work on current text block or text selection. (a “text block” is text between empty lines)

When called in lisp code, if ξfrom-to-pair is non-nil, change the text
in the region between positions [from to]. ξfrom-to-pair should be a
list or vector pair.  Else, returns a changed string."
  (interactive
   (if (region-active-p)
       (list nil (vector (region-beginning) (region-end)))
     (let ((bds (bounds-of-thing-at-point 'paragraph)) )
       (list nil (vector (car bds) (cdr bds))) ) ) )

  (let (workOnStringP inputStr outputStr ξfrom ξto )
    (when ξfrom-to-pair 
        (setq ξfrom (elt ξfrom-to-pair 0) )
        (setq ξto (elt ξfrom-to-pair 1) )
      )

    (setq workOnStringP (if ξfrom-to-pair nil t))
    (setq inputStr (if workOnStringP ξstring (buffer-substring-no-properties ξfrom ξto)))
    (setq outputStr
          (let ((case-fold-search t))
            (replace-regexp-in-string "a\\|e\\|i\\|o\\|u\\|" "" inputStr) )  )

    (if workOnStringP
        outputStr
      (save-excursion
        (delete-region ξfrom ξto)
        (goto-char ξfrom)
        (insert outputStr) )) ) )

(defun what-hexadecimal-value ()
  "Prints the decimal value of a hexadecimal string under cursor.
Samples of valid input:

  ffff
  0xffff
  #xffff
  FFFF
  0xFFFF
  #xFFFF

Test cases
  64*0xc8+#x12c 190*0x1f4+#x258
  100 200 300   400 500 600"
  (interactive )

  (let (inputStr tempStr p1 p2 )
    (save-excursion
      (search-backward-regexp "[\\`0-9A-Fa-fx#]" nil t)
      (forward-char)
      (setq p1 (point) )
      (search-forward-regexp "[\\`0-9A-Fa-fx#]" nil t)
      (backward-char)
      (setq p2 (point) ) )

    (setq inputStr (buffer-substring-no-properties p1 p2) )

    (let ((case-fold-search nil) )
      (setq tempStr (replace-regexp-in-string "\\`0x" "" inputStr )) ; C, Perl, …
      (setq tempStr (replace-regexp-in-string "\\`#x" "" tempStr )) ; elisp …
      (setq tempStr (replace-regexp-in-string "\\`#" "" tempStr ))  ; CSS …
      )
    
    (message "Hex %s is %d" tempStr (string-to-number tempStr 16 ) )
    ))
