(defvar ξurl-encode-chars-pairs nil "A list of pairs of chars that needs to be percent encoded. WARNING: a hack. Incomplete.")
(setq ξurl-encode-chars-pairs
[
                              ["'" "%27"]
                              ["(" "%28"]
                              [")" "%29"]
                              ["–" "%E2%80%93"]
                              ["&" "&amp;"]
                              ["," "%2C"]
                              ["\"" "%22"]
                              ]
 )

(defun xah-url-percent-decode-string (φstring)
  "Decode URL percent-encoded string.
e.g. 「%28」 ⇒ 「'」.
WARNING: the decoding is incomplete.
2015-01-07 http://ergoemacs.org/emacs/elisp_decode_uri_percent_encoding.html
"
  (xah-replace-pairs-in-string φstring (mapcar (lambda (ξx) (vector (elt ξx 1) (elt ξx 0))) ξurl-encode-chars-pairs)))

(defun xah-url-percent-encode-string (φstring)
  "Returns URL percent-encoded
Example:
 http://en.wikipedia.org/wiki/Python_(programming_language)
⇒
 http://en.wikipedia.org/wiki/Python_%28programming_language%29
WARNING: the encoding is incomplete.
2015-01-07 http://ergoemacs.org/emacs/elisp_decode_uri_percent_encoding.html
"
(progn
    (xah-replace-pairs-in-string φstring ξurl-encode-chars-pairs)))
