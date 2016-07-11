;-*- coding: utf-8 -*-
; part of Xah Lee's emacs init file.
; 2011-05-27
; Xah Lee,
; ∑ http://xahlee.org/

(require 'subr-x)

(defun xahsite-server-root-path ()
  "Returns the full path of xah lee website local file web root.
Ends in a slash.
e.g. c:/Users/h3/web/"
(expand-file-name "~/web/")
)

(defun xahsite-domain-names ()
  "Returns a vector of xah web domain names."
  [
   "ergoemacs.org"
   "wordyenglish.com"
   ;; "wordyenglish.info"
   "xaharts.org"
   "xahlee.info"
   "xahlee.org"
   "xahmusic.org"
   "xahporn.org"
   "xahsl.org"
   ]
  )

(defun xahsite-xahlee-info-external-docs ()
  "a vector of dir under xahlee.info that are external docs"
  [

 "REC-SVG11-20110816"
 "clojure-doc-1.8"
 "css3_spec_bg"
 "css_2.1_spec"
 "css_3_color_spec"
 "css_transitions"
 "dom-whatwg"
 "html5_whatwg"
 "java8_doc"
 "javascript_ecma-262_5.1_2011"
 "javascript_ecma-262_6_2015"
 "javascript_es6"
 "jquery_doc"
 "node_api"
 "php-doc"
 "python_doc_2.7.6"
 "python_doc_3.3.3"

;; grep -r -F "REC-SVG11-20110816" --include='*html' ~/web/xahlee_info >> xx33
;; grep -r -F "clojure-doc-1.8" --include='*html' ~/web/xahlee_info >> xx33
;; grep -r -F "css3_spec_bg" --include='*html' ~/web/xahlee_info >> xx33
;; grep -r -F "css_2.1_spec" --include='*html' ~/web/xahlee_info >> xx33
;; grep -r -F "css_3_color_spec" --include='*html' ~/web/xahlee_info >> xx33
;; grep -r -F "css_transitions" --include='*html' ~/web/xahlee_info >> xx33
;; grep -r -F "dom-whatwg" --include='*html' ~/web/xahlee_info >> xx33
;; grep -r -F "html5_whatwg" --include='*html' ~/web/xahlee_info >> xx33
;; grep -r -F "java8_doc" --include='*html' ~/web/xahlee_info >> xx33
;; grep -r -F "javascript_ecma-262_5.1_2011" --include='*html' ~/web/xahlee_info >> xx33
;; grep -r -F "javascript_ecma-262_6_2015" --include='*html' ~/web/xahlee_info >> xx33
;; grep -r -F "javascript_es6" --include='*html' ~/web/xahlee_info >> xx33
;; grep -r -F "jquery_doc" --include='*html' ~/web/xahlee_info >> xx33
;; grep -r -F "node_api" --include='*html' ~/web/xahlee_info >> xx33
;; grep -r -F "php-doc" --include='*html' ~/web/xahlee_info >> xx33
;; grep -r -F "python_doc_2.7.6" --include='*html' ~/web/xahlee_info >> xx33
;; grep -r -F "python_doc_3.3.3" --include='*html' ~/web/xahlee_info >> xx33

 ]
  )


(defun xahsite-local-link-p (_href-value)
  "Return true if it's a local file link, else false.

Non local link may start with these:

 http://
 https://
 mailto:
 irc:
 ftp:
 javascript:
 //
"
  ;; (not (string-match-p "\\`https?://\\|\\`mailto:\\|\\`irc:\\|\\`ftp:\\|\\`javascript:" _href-value) )

  (cond
   ((string-match-p "^//" _href-value) nil)
   ((string-match-p "^http://" _href-value) nil)
   ((string-match-p "^https://" _href-value) nil)
   ((string-match-p "^mailto:" _href-value) nil)
   ((string-match-p "^irc:" _href-value) nil)
   ((string-match-p "^ftp:" _href-value) nil)
   ((string-match-p "^javascript:" _href-value) nil)
   (t t)))

(defun xahsite-url-is-xah-website-p (_url)
  "Returns t if _url is a xah website, else nil.

yahoo.com                 ⇒ nil. not xah site.
http://yahoo.com          ⇒ nil. not xah site.
http://xahlee.org         ⇒ t
xahlee.org                ⇒ nil. just domain name.
http://w.xahlee.org       ⇒ nil
http://www.xahlee.org     ⇒ t
http://ergoemacs.org/     ⇒ t
http://www.ergoemacs.org/ ⇒ t

See: `xahsite-domain-names'."
  (catch 'myloop
    (mapc (lambda (x)
            (when (string-match-p (format "\\`http://\\(www\\.\\)*%s\.*/*" (regexp-quote x)) _url)
              (throw 'myloop t)))
          (xahsite-domain-names))
    nil
    ))
;; test cases
;; (xahsite-url-is-xah-website-p "yahoo.com")             ; nil. not xah site.
;; (xahsite-url-is-xah-website-p "http://yahoo.com")      ; nil. not xah site.
;; (xahsite-url-is-xah-website-p "http://xahlee.org")     ; t
;; (xahsite-url-is-xah-website-p "xahlee.org")            ; nil. just domain name.
;; (xahsite-url-is-xah-website-p "http://w.xahlee.org")   ; nil
;; (xahsite-url-is-xah-website-p "http://www.xahlee.org") ; t
;; (xahsite-url-is-xah-website-p "http://ergoemacs.org/") ; t
;; (xahsite-url-is-xah-website-p "http://www.ergoemacs.org/") ; t

(defun xahsite-is-link-to-xahsite-p (_href-value)
  "Returns true if _href-value points to a xah website, else false.
_href-value is the string in 「<a href=\"…\">」 or 「<img src=\"…\">」 or any such.

Technically, returns true if _href-value is a local link (relative file path) or is URL to xah site 「http://…‹xah domain›/」.

See: `xahsite-local-link-p', `xahsite-url-is-xah-website-p'."
  (if (xahsite-local-link-p _href-value)
      t
    (xahsite-url-is-xah-website-p _href-value)))



(defun xahsite-url-to-domain-name (_url)
  "Returns the domain name of a xah site.
e.g. http://ergoemacs.org/emacs/emacs.html ⇒ ergoemacs.org
"
(replace-regexp-in-string "\\`http://\\(www\\.\\)*\\([^.]+\\)\\.\\(info\\|org\\|com\\)/.+" "\\2.\\3" _url) )

(defun xahsite-get-domain-of-local-file-path (_abs-path)
  "Returns the domain name of full path _abs-path belong to.
e.g. 「c:/Users/h3/web/ergoemacs_org/emacs/xyz.html」
returns 「ergoemacs.org」.

This function depends on `xahsite-server-root-path'."
  (let ((case-fold-search nil)
        -str
        (-pathPart
         (string-remove-prefix
          (downcase (xahsite-server-root-path))
          (downcase _abs-path))))
    (if (string-match "\\`\\([^/]+?\\)/" -pathPart )
        (progn
          (setq -str (match-string 1 -pathPart))
          (replace-regexp-in-string "_" "." -str "FIXEDCASE" "LITERAL"))
      (error "「%s」 is not a full path for xah site." _abs-path ))))

(defun xahsite-get-path-relative-to-domain (_fpath)
  "Returns the path relative to that file's domain's root dir.
e.g. 「c:/Users/h3/web/ergoemacs_org/emacs/xyz.html」
returns 「emacs/xyz.html」"
  (let ((case-fold-search nil))
    (string-match (format "\\`%s[^/]+?/\\(.+\\)" (regexp-quote (xahsite-server-root-path)))
                  (replace-regexp-in-string "\\`C:/" "c:/" _fpath  "FIXEDCASE" "LITERAL"))
    (match-string 1 _fpath)))

(defun xahsite-filepath-to-url (_webpath)
  "Turn my website path ΦWEBPATH to my site's URL.
For example, the following path:
 C:/Users/xah/web/ergoemacs_org/emacs/emacs.html
or
 /Users/xah/web/ergoemacs_org/emacs/emacs.html
will become:
 http://ergoemacs.org/emacs/emacs.html"
  (format "http://%s/%s" (xahsite-get-domain-of-local-file-path _webpath) (xahsite-get-path-relative-to-domain _webpath)))

(defun xahsite-filepath-to-href-value (_linkFilePath _currentFilePathOrDir)
  "Return a URL or relative path.
All arguments should all be full paths.
If the two paths are in different domain, then return a URL (string starts with “http://”).
Else, return a relative path.

For reverse, see `xahsite-href-value-to-filepath'.
"
  (let ((sameDomain-p (string= (xahsite-get-domain-of-local-file-path _linkFilePath) (xahsite-get-domain-of-local-file-path _currentFilePathOrDir))))
    (if sameDomain-p
        (xah-file-relative-name-emacs24.1.1-fix _linkFilePath (file-name-directory _currentFilePathOrDir))
      (xahsite-filepath-to-url _linkFilePath))))
;; test
;; (xahsite-filepath-to-href-value "c:/Users/h3/web/xahlee_org/arts/blog.html" "c:/Users/h3/web/ergoemacs_org/emacs/emacs23_features.html")
;; (xahsite-filepath-to-href-value "c:/Users/h3/web/ergoemacs_org/index.html" "c:/Users/h3/web/ergoemacs_org/emacs/emacs23_features.html" )
;; (xahsite-filepath-to-href-value "c:/Users/h3/web/ergoemacs_org/emacs/emacs23_features.html" "c:/Users/h3/web/ergoemacs_org/index.html" )

(defun xahsite-href-value-to-filepath (_href-value _host-file-path)
  "Returns the file path of a link to xah website.

_href-value is the link string, in 「href=\"…\"」. The value can be a URL to xahsite or relative path.
_host-file-path is a full path of the host file name or its dir.

For reverse, see `xahsite-filepath-to-href-value'.
See also: `xahsite-url-to-filepath'
"
  (if (string-match-p "\\`http://" _href-value)
      (progn (xahsite-url-to-filepath _href-value "addFileName"))
    (progn
      (expand-file-name _href-value (file-name-directory _host-file-path )))))
;; test
;; (xahsite-href-value-to-filepath "http://xahlee.org/Netiquette_dir/death_of_a_troll.html" "c:/Users/h3/web/xahlee_info/comp/Google_Tech_Talk_Lisp_At_JPL_by_Ron_Garret.html")

(defun xahsite-url-to-filepath (_xahsiteURL &optional _add-file-name _redirect)
  "Returns the file path of a xah website URL _xahsiteURL.

If the optional argument _add-file-name is true, then append “index.html” if the resulting path is a dir.
If the optional argument _redirect is true, then also consider result of http redirect.

This function does not check input is actually a URL, nor if the result path file exists."
  ;; test cases:
  ;; (xahsite-url-to-filepath "http://xahlee.org/index.html") ; ⇒ "c:/Users/h3/web/xahlee_org/index.html"
  ;; (xahsite-url-to-filepath "http://xahlee.org/") ; ⇒ "c:/Users/h3/web/http://xahlee.org/index.html"
  ;; (xahsite-url-to-filepath "http://abc.org/x.html") ; ⇒ "c:/Users/h3/web/abc_org/x.html"
  ;; (xahsite-url-to-filepath "some water") ; ⇒ "c:/Users/h3/web/some water"

  (let ((-url _xahsiteURL) -fPath)
    (setq -url (xah-remove-uri-fragment -url)) ; remove HTML fragment, e.g. http://ergoemacs.org/emacs/elisp.html#comment-113416750
    (when _redirect (setq -url (xahsite-url-remap -url)))
    (when _add-file-name (setq -url (replace-regexp-in-string "/\\'" "/index.html" -url)))
    ;; (replace-regexp-in-string "%27" "'" (xah-remove-uri-fragment -url))

    (setq -fPath
          (format "%s%s" (xahsite-server-root-path)
                  ;; remove www
                  (replace-regexp-in-string
                   "\\`http://\\(www\\.\\)*\\([^.]+\\)\\.\\(info\\|org\\|com\\)/\\(.*\\)"
                   "\\2_\\3/\\4" -url)))
    -fPath
    ))

(defvar xahsite-xahlee-org-redirect nil "root dir map from xahlee.org to ergoemacs.org")
(setq xahsite-xahlee-org-redirect
["3d"
"M"
"MathGraphicsGallery_dir"
"MathematicaPrograming_dir"
"PerlMathematica_dir"
"SpecialPlaneCurves_dir"
"UnixResource_dir"
"Wallpaper_dir"
"cmaci"
"comp"
"complex"
"coq"
"haskell"
"java-a-day"
"js"
"kbd"
"linux"
"math"
"mswin"
"ocaml"
"pascal"
"perl-python"
"php"
"powershell"
"prog"
"projective_geometry"
"ruby"
"surface"
"tiling"
"tree"
"visual_basic"
"w"
] )

;; (defun xahsite-url-remap (_xahsite-url)
;;   "Returns a redirected url for a xah website URL _xahsite-url.

;; This function is not complete. i.e. it not contain complete url redirects as specified in web server."
;;   (let (
;;         -domain
;;         -path
;;         (-s _xahsite-url)
;;         (-stop nil)
;;         )
;;     (string-match "\\`http://\\(www\\.\\)*\\([^.]+\\)\\.\\(info\\|org\\|com\\)/\\(.*\\)" -s)
;;     (setq -domain (match-string "\\2.\\3" -s ) )
;;     (setq -path (match-string "\\4" -s ) )

;;     (cond
;;      ((string-match-p "xahlee.org" -domain)
;;       (let (
;;             (-i 0)
;;             (-len (length xahsite-xahlee-org-redirect))
;;             (-pathFirstPart (replace-regexp-in-string "\\`\\([^/]+?\\)/" "\\1" -path) )
;;             )
;;         (while (and (not -stop) (< -i -len) )
;;           (if (string= (elt xahsite-xahlee-org-redirect -i) -pathFirstPart)
;;               (progn
;;                 ;; ...
;;                 (setq -stop t ))
;;             (progn )
;;             )
;;           (setq -i (1+ -i))
;;           )
;;         )
;;       )
;;      ;; ...
;;      )

;;     (setq -s (replace-regexp-in-string "/Periodic_dosage_dir/sanga_pemci/" "/music/" -s "FIXEDCASE" "LITERAL"))
;;     (setq -s (replace-regexp-in-string "xahlee.org/emacs/" "ergoemacs.org/emacs/" -s "FIXEDCASE" "LITERAL"))
;;  ) )

(defun xahsite-url-remap (_xahsite-url)
  "Returns a redirected url for a xah website URL _xahsite-url.

This function is not complete. i.e. it not contain complete url redirects as specified in web server."
  (let ((-s _xahsite-url)
        (case-fold-search nil))
    (setq -s
          (cond

           ((string-match-p "\\`http://xahlee\\.org/Periodic_dosage_dir/cmaci_girzu.html" -s) "http://xahlee.info/math/math_index.html" )
           ((string-match-p "\\`http://xahlee\\.org/Periodic_dosage_dir/keyboarding.html" -s) "http://xahlee.info/kbd/keyboarding.html" )
           ((string-match-p "\\`http://xahlee\\.org/Periodic_dosage_dir/unicode.html" -s) "http://xahlee.info/comp/unicode_index.html" )
           ((string-match-p "\\`http://xahlee\\.org/Periodic_dosage_dir/skami_prosa.html" -s) "http://xahlee.info/comp/comp_index.html" )

           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(emacs\\)/\\(.*\\)" -s) (format "http://%s/%s/%s" "ergoemacs.org" (match-string 2 -s) (match-string 3 -s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(emacs_manual\\)/\\(.*\\)" -s) (format "http://%s/%s/%s" "ergoemacs.org" (match-string 2 -s) (match-string 3 -s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(M\\)/\\(.*\\)" -s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 -s) (match-string 3 -s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(MathGraphicsGallery_dir\\)/\\(.*\\)" -s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 -s) (match-string 3 -s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(MathematicaPrograming_dir\\)/\\(.*\\)" -s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 -s) (match-string 3 -s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(PerlMathematica_dir\\)/\\(.*\\)" -s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 -s) (match-string 3 -s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(SpecialPlaneCurves_dir\\)/\\(.*\\)" -s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 -s) (match-string 3 -s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(UnixResource_dir\\)/\\(.*\\)" -s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 -s) (match-string 3 -s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(Wallpaper_dir\\)/\\(.*\\)" -s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 -s) (match-string 3 -s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(cmaci\\)/\\(.*\\)" -s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 -s) (match-string 3 -s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(comp\\)/\\(.*\\)" -s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 -s) (match-string 3 -s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(complex\\)/\\(.*\\)" -s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 -s) (match-string 3 -s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(coq\\)/\\(.*\\)" -s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 -s) (match-string 3 -s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(haskell\\)/\\(.*\\)" -s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 -s) (match-string 3 -s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(java-a-day\\)/\\(.*\\)" -s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 -s) (match-string 3 -s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(js\\)/\\(.*\\)" -s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 -s) (match-string 3 -s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(kbd\\)/\\(.*\\)" -s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 -s) (match-string 3 -s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(linux\\)/\\(.*\\)" -s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 -s) (match-string 3 -s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(math\\)/\\(.*\\)" -s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 -s) (match-string 3 -s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(mswin\\)/\\(.*\\)" -s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 -s) (match-string 3 -s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(ocaml\\)/\\(.*\\)" -s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 -s) (match-string 3 -s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(pascal\\)/\\(.*\\)" -s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 -s) (match-string 3 -s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(perl-python\\)/\\(.*\\)" -s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 -s) (match-string 3 -s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(php\\)/\\(.*\\)" -s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 -s) (match-string 3 -s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(powershell\\)/\\(.*\\)" -s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 -s) (match-string 3 -s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(prog\\)/\\(.*\\)" -s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 -s) (match-string 3 -s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(projective_geometry\\)/\\(.*\\)" -s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 -s) (match-string 3 -s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(ruby\\)/\\(.*\\)" -s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 -s) (match-string 3 -s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(surface\\)/\\(.*\\)" -s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 -s) (match-string 3 -s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(tiling\\)/\\(.*\\)" -s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 -s) (match-string 3 -s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(tree\\)/\\(.*\\)" -s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 -s) (match-string 3 -s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(visual_basic\\)/\\(.*\\)" -s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 -s) (match-string 3 -s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(w\\)/\\(.*\\)" -s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 -s) (match-string 3 -s)))

           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(music\\)/\\(.*\\)" -s) (format "http://%s/%s/%s" "xahmusic.org" (match-string 2 -s) (match-string 3 -s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(lit\\)/\\(.*\\)" -s) (format "http://%s/%s/%s" "wordyenglish.com" (match-string 2 -s) (match-string 3 -s)))

           (t -s )))
    -s
    ))

(defun xah-remove-uri-fragment (_href-value)
  "remove URL _href-value fragment, anything after first 「#」 char, including it.
See also `split-uri-hashmark'"
  ;; test
  ;; (xah-remove-uri-fragment "a#b") ; "a"
  ;; (xah-remove-uri-fragment "#3")  ; ""
  ;; (xah-remove-uri-fragment "4")  ; "4"
  ;; (xah-remove-uri-fragment "#")   ; ""
  ;; (xah-remove-uri-fragment "")  ; ""
  (let ((-x (string-match-p "#" _href-value )))
    (if -x
        (substring _href-value 0 -x)
      _href-value )))

(defun split-uri-hashmark (_href-value)
  "Split a URL _href-value by 「#」 char, return a vector.
 e.g. \"y.html#z\" ⇒ [\"y.html\", \"#z\"]

Examples:
 「a#b」 ⇒ 「a」 「#b」
 「#」 ⇒ 「」 「#」
 「#3」 ⇒ 「」 「#3」
 「3#」 ⇒ 「3」 「#」
 「4」 ⇒  「4」 「」
 「」 ⇒  「」 「」

See also: `xah-remove-uri-fragment'"
  ;; test
  ;; (split-uri-hashmark "a#b") ; ["a" "#b"]
  ;; (split-uri-hashmark "#3")  ; ["" "#3"]
  ;; (split-uri-hashmark "#")   ; ["" "#"]
  ;; (split-uri-hashmark "4")  ; ["4" ""]
  ;; (split-uri-hashmark "")  ; ["" ""]
  (let ((-x (string-match-p "#" _href-value )))
    (if -x
        (vector (substring _href-value 0 -x) (substring _href-value -x))
      (vector _href-value "" ))))



(defun file-moved-p (_fpath _moved-dirs )
  "Return true if either paths are in _moved-dirs list or as a subdir.
_fpath is a full path to a file.
_moved-dirs is a list/sequence of file full paths.
Return true if _fpath is in _moved-dirs or is a subdir of _moved-dirs.
Technically, if any string in _moved-dirs is a prefix of _fpath."
  (let ( ( -found nil) ( -i 0))
    (while (and (not -found) (< -i (length _moved-dirs)))
      (setq -found (string-match-p (concat "\\`" (regexp-quote (elt _moved-dirs -i))) _fpath ))
      (setq -i (1+ -i)))
    -found
    ))
;; test
;; (file-moved-p "abc/d" ["abc/d" "don/" "12/3/"] ) ; true, because “abc/d” equal to one of the moved dir
;; (file-moved-p "abc/d/e" ["abc/d" "don/" "12/3/"] ) ; true, because “abc/d/e” is subdir of “abc/d”
;; (file-moved-p "abc/" ["abc/d" "don/" "12/3/"] ) ; false, because “abc/” isn't in any of the moved dirs

(defun xah-local-url-to-file-path (_local-file-url)
  "Turn a localhost file URL LOCALFILEURL into a file full path.

_local-file-url must be a full path.

For example, the following string shown in browser URL field:
; On Windows Vista 2009-06
 〔C:\\Users\\xah\\web\\emacs\\emacs.html〕  IE
 〔file:///C:/Users/xah/web/emacs/emacs.html〕  Firefox, Google Chrome, Safari
 〔file://localhost/C:/Users/xah/web/emacs/emacs.html〕  Opera
 becomes
 〔C:/Users/xah/web/emacs/emacs.html〕

 On Mac 2009-06
 〔file:///Users/xah/web/emacs/emacs.html〕  Safari, Firefox
 〔file://localhost/Users/xah/web/emacs/emacs.html〕  Opera
 becomes
 〔/Users/xah/web/emacs/emacs.html〕

 On Ubuntu Linux, 2011-05
 〔file:///media/HP/Users/xah/web/xahlee_org/index.html〕 firefox
 becomes
 〔/media/HP/Users/xah/web/xahlee_org/index.html〕
"
  (let ((case-fold-search nil))
    (xah-replace-regexp-pairs-in-string _local-file-url
                                    [
                                     ["\\`file://localhost" ""]
                                     ["\\`file://" ""]
                                     ["\\`/\\([A-Za-z]\\):" "\\1:"] ; Windows C:\\
                                     ["\\`C:" "c:"] ; need because a bug in `file-relative-name', it doesn't work when path C: is cap
                                     ["\\\\" "/"]   ; Windows \ → /
                                     ]
                                    "FIXEDCASE"
                                    )))

(defun xahsite-web-path-to-filepath (_input-str &optional _default-dir)
  "Returns a file full path of _input-str.
_input-str can have any of these form:

 x.html (relative path)
 c:/Users/h3/web/ergoemacs_org/a/x.html (Windows style)
 c:\\Users\\h3\\web\\ergoemacs_org\\emacs\\x.html (Windows style)
 /cygdrive/c/Users/h3/web/ergoemacs_org/a/x.html (Cygwin)
 /Users/xah/web/ergoemacs_org/a/x.html (unix style)
 ~/web/ergoemacs_org/a/x.html
 file://… (file URL. See: `xah-local-url-to-file-path')
 http://ergoemacs.org/a/x.html (URL)

if the _input-str is a relative path, _default-dir is used to resolve to full path."
  (let ( (-s _input-str))

    ;; (setq -s (replace-regexp-in-string "^file:///" "" -s "FIXEDCASE" "LITERAL" ) )
    ;; (setq -s (replace-regexp-in-string "^/media/OS/Users/h3" "~" -s "FIXEDCASE" "LITERAL" ) )

    (if (string-match-p "\\`https?://" -s)
        (progn (setq -s (xahsite-url-to-filepath -s "addFileName")))
      (progn
        (when (string-match-p "\\`file://" -s) (setq -s (xah-local-url-to-file-path -s)))
        (when (string-match-p "\\`[A-Za-z]:\\|\\\\" -s) ; change Microsoft Windows style path to unix
          (setq -s (replace-regexp-in-string "\\`[A-Za-z]:" "" (replace-regexp-in-string "\\\\" "/" -s t t))))
        (setq -s (replace-regexp-in-string "\\`/cygdrive/[a-zA-Z]" "" -s))
        (setq -s (expand-file-name -s _default-dir))))
    -s
    ))

(defun xah-path-ends-in-image-suffix-p (_path)
  "Returns t if _path ends in .jpg .png .gif .svg, else nil."
  (string-match-p "\.jpg\\'\\|\.png\\'\\|\.gif\\'\\|\.svg\\'" _path))

(defun xah-find-files-file-predicate-p (fname parentdir)
  "return t if fname is what we want. Else nil.
2016-07-09"
  (interactive)
  (and
   (string-match "\\.html$" fname)
   (not (string-match "^xx" fname))
   ))

(defun xah-find-files-dir-predicate-p (fname parentdir)
  "return t if fname is what we want. Else nil.
2016-07-09"
  (and
   (not
    (or
     (string-equal "java8_doc" fname)
     (string-equal "REC-SVG11-20110816" fname)
     (string-equal "clojure-doc-1.8" fname)
     (string-equal "css3_spec_bg" fname)
     (string-equal "css_2.1_spec" fname)
     (string-equal "css_3_color_spec" fname)
     (string-equal "css_transitions" fname)
     (string-equal "dom-whatwg" fname)
     (string-equal "html5_whatwg" fname)
     (string-equal "javascript_ecma-262_5.1_2011" fname)
     (string-equal "javascript_ecma-262_6_2015" fname)
     (string-equal "javascript_es6" fname)
     (string-equal "jquery_doc" fname)
     (string-equal "node_api" fname)
     (string-equal "php-doc" fname)
     (string-equal "python_doc_2.7.6" fname)
     (string-equal "python_doc_3.3.3" fname)
     (string-match "^xx" fname)))
   (find-lisp-default-directory-predicate fname parentdir)))

(defun xahsite-traverse-dir-file-list (dirpath)
  "a list of full paths to process"
  (require 'find-lisp)
  (find-lisp-find-files-internal
    dirpath
    'xah-find-files-file-predicate-p
    'xah-find-files-dir-predicate-p))

(defun xahsite-generate-sitemap (_domain-name)
  "Generate a sitemap.xml.gz file of xahsite at doc root.
_domain-name must match a existing one."
  (interactive
   (list (ido-completing-read "choose:" '( "ergoemacs.org" "wordyenglish.com" "xaharts.org" "xahlee.info" "xahlee.org" "xahmusic.org" "xahporn.org" "xahsl.org" ))))
  (let (
        (--sitemapFileName "sitemap" )
        (--websiteDocRootPath (concat (xahsite-server-root-path) (replace-regexp-in-string "\\." "_" _domain-name "FIXEDCASE" "LITERAL") "/")))

    (print (concat "begin: " (format-time-string "%Y-%m-%dT%T")))

    ;; rename sitemap file to backup ~ if already exist
    (let* (
           (f1 (concat --websiteDocRootPath --sitemapFileName ".xml"))
           (f2 (concat f1 ".gz")))
      (when (file-exists-p f1)
        (rename-file f1 (concat f1 "~") t))
      (when (file-exists-p f2)
        (rename-file f2 (concat f2 "~") t)))

    ;; create sitemap buffer
    (let (
          (-filePath (concat --websiteDocRootPath --sitemapFileName ".xml"))
          -sitemapBuffer
          )
      (setq -sitemapBuffer (find-file -filePath))
      (erase-buffer)
      (set-buffer-file-coding-system 'unix)
      (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">
")

      (mapc
       (lambda (-f)
         ;; xahsite-xahlee-info-external-docs
         (when (not
                (or
                 (string-match "/xx" -f) ; ; dir/file starting with xx are not public
                 (string-match "403error.html" -f)
                 (string-match "404error.html" -f)))
           (with-temp-buffer
             (insert-file-contents -f)
             (when (not (search-forward "<meta http-equiv=\"refresh\"" nil "noerror"))
               (with-current-buffer -sitemapBuffer
                 (insert "<url><loc>")
                 (insert (concat "http://" _domain-name "/" (substring -f (length --websiteDocRootPath))))
                 (insert "</loc></url>\n"))))))
       (xahsite-traverse-dir-file-list --websiteDocRootPath))

      (insert "</urlset>")

      (save-buffer)
      (kill-buffer -sitemapBuffer)

      (if "zip it"
          (progn
            (shell-command (concat "gzip " -filePath))
            (find-file (concat -filePath ".gz")))
        (progn (find-file -filePath ))))

    (print (concat "finished: " (format-time-string "%Y-%m-%dT%T")))))

(defun xahsite-remove-ads (_begin _end)
  "Remove all ads of in region _begin _end.

Remove Google adds, Amazon ads, and other ads, Google Analytics
 Tracker code, Disqus code, …."
"dummy"
)
