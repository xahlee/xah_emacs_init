;-*- coding: utf-8 -*-
; part of Xah Lee's emacs init file.
; 2011-05-27
; Xah Lee,
; ∑ http://xahlee.org/

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


(defun xahsite-local-link-p (φhref-value)
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
  ;; (not (string-match-p "\\`https?://\\|\\`mailto:\\|\\`irc:\\|\\`ftp:\\|\\`javascript:" φhref-value) )
  
  (cond
   ((string-match-p "^//" φhref-value) nil)
   ((string-match-p "^http://" φhref-value) nil)
   ((string-match-p "^https://" φhref-value) nil)
   ((string-match-p "^mailto:" φhref-value) nil)
   ((string-match-p "^irc:" φhref-value) nil)
   ((string-match-p "^ftp:" φhref-value) nil)
   ((string-match-p "^javascript:" φhref-value) nil)
   (t t)))

(defun xahsite-url-is-xah-website-p (φurl)
  "Returns t if φurl is a xah website, else nil.

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
            (when (string-match-p (format "\\`http://\\(www\\.\\)*%s\.*/*" (regexp-quote x)) φurl)
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

(defun xahsite-is-link-to-xahsite-p (φ-href-value)
  "Returns true if φ-href-value points to a xah website, else false.

φ-href-value is the string in 「<a href=\"…\">」 or 「<img src=\"…\">」 or any such.

Technically, returns true if φ-href-value is a local link (relative file path) or is URL to xah site 「http://…‹xah domain›/」.

See: `xahsite-local-link-p', `xahsite-url-is-xah-website-p'."
  (if (xahsite-local-link-p φ-href-value)
      t
    (xahsite-url-is-xah-website-p φ-href-value)))



(defun xahsite-url-to-domain-name (φurl)
  "Returns the domain name of a xah site.
e.g. http://ergoemacs.org/emacs/emacs.html ⇒ ergoemacs.org
"
(replace-regexp-in-string "\\`http://\\(www\\.\\)*\\([^.]+\\)\\.\\(info\\|org\\|com\\)/.+" "\\2.\\3" φurl) )

(defun xahsite-get-domain-of-local-file-path (φabs-path)
  "Returns the domain name of full path φabs-path belong to.
e.g. 「c:/Users/h3/web/ergoemacs_org/emacs/xyz.html」
returns 「ergoemacs.org」.

This function depends on `xahsite-server-root-path'."
  (let ((case-fold-search nil) ξstr (ξpathPart (substract-path (downcase φabs-path) (downcase (xahsite-server-root-path)))))
    (if (string-match "\\`\\([^/]+?\\)/" ξpathPart )
        (progn
          (setq ξstr (match-string 1 ξpathPart))
          (replace-regexp-in-string "_" "." ξstr "FIXEDCASE" "LITERAL"))
      (error "「%s」 is not a full path for xah site." φabs-path ))))

(defun xahsite-get-path-relative-to-domain (φfpath)
  "Returns the path relative to that file's domain's root dir.
e.g. 「c:/Users/h3/web/ergoemacs_org/emacs/xyz.html」
returns 「emacs/xyz.html」"
  (let ((case-fold-search nil))
    (string-match (format "\\`%s[^/]+?/\\(.+\\)" (regexp-quote (xahsite-server-root-path)))
                  (replace-regexp-in-string "\\`C:/" "c:/" φfpath  "FIXEDCASE" "LITERAL"))
    (match-string 1 φfpath)))

(defun xahsite-filepath-to-url (φwebpath)
  "Turn my website path ΦWEBPATH to my site's URL.
For example, the following path:
 C:/Users/xah/web/ergoemacs_org/emacs/emacs.html
or
 /Users/xah/web/ergoemacs_org/emacs/emacs.html
will become:
 http://ergoemacs.org/emacs/emacs.html"
  (format "http://%s/%s" (xahsite-get-domain-of-local-file-path φwebpath) (xahsite-get-path-relative-to-domain φwebpath)))

(defun xahsite-filepath-to-href-value (φlinkFilePath φcurrentFilePathOrDir)
  "Return a URL or relative path.
All arguments should all be full paths.
If the two paths are in different domain, then return a URL (string starts with “http://”).
Else, return a relative path.

For reverse, see `xahsite-href-value-to-filepath'.
"
  (let ((sameDomain-p (string= (xahsite-get-domain-of-local-file-path φlinkFilePath) (xahsite-get-domain-of-local-file-path φcurrentFilePathOrDir))))
    (if sameDomain-p
        (progn (file-relative-name-emacs24.1.1-fix φlinkFilePath (file-name-directory φcurrentFilePathOrDir)))
      (progn (xahsite-filepath-to-url φlinkFilePath)))))
;; test
;; (xahsite-filepath-to-href-value "c:/Users/h3/web/xahlee_org/arts/blog.html" "c:/Users/h3/web/ergoemacs_org/emacs/emacs23_features.html")
;; (xahsite-filepath-to-href-value "c:/Users/h3/web/ergoemacs_org/index.html" "c:/Users/h3/web/ergoemacs_org/emacs/emacs23_features.html" )
;; (xahsite-filepath-to-href-value "c:/Users/h3/web/ergoemacs_org/emacs/emacs23_features.html" "c:/Users/h3/web/ergoemacs_org/index.html" )

(defun xahsite-href-value-to-filepath (φhref-value φ-host-file-path)
  "Returns the file path of a link to xah website.

φhref-value is the link string, in 「href=\"…\"」. The value can be a URL to xahsite or relative path.
φ-host-file-path is a full path of the host file name or its dir.

For reverse, see `xahsite-filepath-to-href-value'.
See also: `xahsite-url-to-filepath'
"
  (if (string-match-p "\\`http://" φhref-value)
      (progn (xahsite-url-to-filepath φhref-value "addFileName"))
    (progn
      (expand-file-name φhref-value (file-name-directory φ-host-file-path )))))
;; test
;; (xahsite-href-value-to-filepath "http://xahlee.org/Netiquette_dir/death_of_a_troll.html" "c:/Users/h3/web/xahlee_info/comp/Google_Tech_Talk_Lisp_At_JPL_by_Ron_Garret.html")

(defun xahsite-url-to-filepath (φxahsiteURL &optional φ-add-file-name φredirect)
  "Returns the file path of a xah website URL φxahsiteURL.

If the optional argument φ-add-file-name is true, then append “index.html” if the resulting path is a dir.
If the optional argument φredirect is true, then also consider result of http redirect.

This function does not check input is actually a URL, nor if the result path file exists."
  ;; test cases:
  ;; (xahsite-url-to-filepath "http://xahlee.org/index.html") ; ⇒ "c:/Users/h3/web/xahlee_org/index.html"
  ;; (xahsite-url-to-filepath "http://xahlee.org/") ; ⇒ "c:/Users/h3/web/http://xahlee.org/index.html"
  ;; (xahsite-url-to-filepath "http://abc.org/x.html") ; ⇒ "c:/Users/h3/web/abc_org/x.html"
  ;; (xahsite-url-to-filepath "some water") ; ⇒ "c:/Users/h3/web/some water"

  (let ((ξurl φxahsiteURL) ξfPath)
    (setq ξurl (remove-uri-fragment ξurl)) ; remove HTML fragment, e.g. http://ergoemacs.org/emacs/elisp.html#comment-113416750
    (when φredirect (setq ξurl (xahsite-url-remap ξurl)))
    (when φ-add-file-name (setq ξurl (replace-regexp-in-string "/\\'" "/index.html" ξurl)))
    ;; (replace-regexp-in-string "%27" "'" (remove-uri-fragment ξurl))

    (setq ξfPath
          (format "%s%s" (xahsite-server-root-path)
                  ;; remove www
                  (replace-regexp-in-string
                   "\\`http://\\(www\\.\\)*\\([^.]+\\)\\.\\(info\\|org\\|com\\)/\\(.*\\)"
                   "\\2_\\3/\\4" ξurl)))
    ξfPath
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

;; (defun xahsite-url-remap (φxahsite-url)
;;   "Returns a redirected url for a xah website URL φxahsite-url.

;; This function is not complete. i.e. it not contain complete url redirects as specified in web server."
;;   (let (
;;         ξdomain
;;         ξpath
;;         (ξs φxahsite-url)
;;         (ξstop nil)
;;         )
;;     (string-match "\\`http://\\(www\\.\\)*\\([^.]+\\)\\.\\(info\\|org\\|com\\)/\\(.*\\)" ξs)
;;     (setq ξdomain (match-string "\\2.\\3" ξs ) )
;;     (setq ξpath (match-string "\\4" ξs ) )

;;     (cond
;;      ((string-match-p "xahlee.org" ξdomain)
;;       (let (
;;             (ξi 0)
;;             (ξlen (length xahsite-xahlee-org-redirect))
;;             (ξpathFirstPart (replace-regexp-in-string "\\`\\([^/]+?\\)/" "\\1" ξpath) )
;;             )
;;         (while (and (not ξstop) (< ξi ξlen) )
;;           (if (string= (elt xahsite-xahlee-org-redirect ξi) ξpathFirstPart)
;;               (progn
;;                 ;; ...
;;                 (setq ξstop t ))
;;             (progn )
;;             )
;;           (setq ξi (1+ ξi))
;;           )
;;         )
;;       )
;;      ;; ...
;;      )

;;     (setq ξs (replace-regexp-in-string "/Periodic_dosage_dir/sanga_pemci/" "/music/" ξs "FIXEDCASE" "LITERAL"))
;;     (setq ξs (replace-regexp-in-string "xahlee.org/emacs/" "ergoemacs.org/emacs/" ξs "FIXEDCASE" "LITERAL"))
;;  ) )

(defun xahsite-url-remap (φxahsite-url)
  "Returns a redirected url for a xah website URL φxahsite-url.

This function is not complete. i.e. it not contain complete url redirects as specified in web server."
  (let ((ξs φxahsite-url)
        (case-fold-search nil))
    (setq ξs
          (cond

           ((string-match-p "\\`http://xahlee\\.org/Periodic_dosage_dir/cmaci_girzu.html" ξs) "http://xahlee.info/math/math_index.html" )
           ((string-match-p "\\`http://xahlee\\.org/Periodic_dosage_dir/keyboarding.html" ξs) "http://xahlee.info/kbd/keyboarding.html" )
           ((string-match-p "\\`http://xahlee\\.org/Periodic_dosage_dir/unicode.html" ξs) "http://xahlee.info/comp/unicode_index.html" )
           ((string-match-p "\\`http://xahlee\\.org/Periodic_dosage_dir/skami_prosa.html" ξs) "http://xahlee.info/comp/comp_index.html" )

           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(emacs\\)/\\(.*\\)" ξs) (format "http://%s/%s/%s" "ergoemacs.org" (match-string 2 ξs) (match-string 3 ξs)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(emacs_manual\\)/\\(.*\\)" ξs) (format "http://%s/%s/%s" "ergoemacs.org" (match-string 2 ξs) (match-string 3 ξs)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(M\\)/\\(.*\\)" ξs) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 ξs) (match-string 3 ξs)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(MathGraphicsGallery_dir\\)/\\(.*\\)" ξs) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 ξs) (match-string 3 ξs)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(MathematicaPrograming_dir\\)/\\(.*\\)" ξs) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 ξs) (match-string 3 ξs)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(PerlMathematica_dir\\)/\\(.*\\)" ξs) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 ξs) (match-string 3 ξs)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(SpecialPlaneCurves_dir\\)/\\(.*\\)" ξs) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 ξs) (match-string 3 ξs)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(UnixResource_dir\\)/\\(.*\\)" ξs) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 ξs) (match-string 3 ξs)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(Wallpaper_dir\\)/\\(.*\\)" ξs) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 ξs) (match-string 3 ξs)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(cmaci\\)/\\(.*\\)" ξs) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 ξs) (match-string 3 ξs)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(comp\\)/\\(.*\\)" ξs) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 ξs) (match-string 3 ξs)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(complex\\)/\\(.*\\)" ξs) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 ξs) (match-string 3 ξs)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(coq\\)/\\(.*\\)" ξs) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 ξs) (match-string 3 ξs)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(haskell\\)/\\(.*\\)" ξs) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 ξs) (match-string 3 ξs)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(java-a-day\\)/\\(.*\\)" ξs) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 ξs) (match-string 3 ξs)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(js\\)/\\(.*\\)" ξs) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 ξs) (match-string 3 ξs)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(kbd\\)/\\(.*\\)" ξs) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 ξs) (match-string 3 ξs)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(linux\\)/\\(.*\\)" ξs) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 ξs) (match-string 3 ξs)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(math\\)/\\(.*\\)" ξs) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 ξs) (match-string 3 ξs)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(mswin\\)/\\(.*\\)" ξs) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 ξs) (match-string 3 ξs)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(ocaml\\)/\\(.*\\)" ξs) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 ξs) (match-string 3 ξs)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(pascal\\)/\\(.*\\)" ξs) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 ξs) (match-string 3 ξs)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(perl-python\\)/\\(.*\\)" ξs) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 ξs) (match-string 3 ξs)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(php\\)/\\(.*\\)" ξs) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 ξs) (match-string 3 ξs)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(powershell\\)/\\(.*\\)" ξs) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 ξs) (match-string 3 ξs)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(prog\\)/\\(.*\\)" ξs) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 ξs) (match-string 3 ξs)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(projective_geometry\\)/\\(.*\\)" ξs) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 ξs) (match-string 3 ξs)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(ruby\\)/\\(.*\\)" ξs) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 ξs) (match-string 3 ξs)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(surface\\)/\\(.*\\)" ξs) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 ξs) (match-string 3 ξs)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(tiling\\)/\\(.*\\)" ξs) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 ξs) (match-string 3 ξs)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(tree\\)/\\(.*\\)" ξs) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 ξs) (match-string 3 ξs)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(visual_basic\\)/\\(.*\\)" ξs) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 ξs) (match-string 3 ξs)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(w\\)/\\(.*\\)" ξs) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 ξs) (match-string 3 ξs)))

           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(music\\)/\\(.*\\)" ξs) (format "http://%s/%s/%s" "xahmusic.org" (match-string 2 ξs) (match-string 3 ξs)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(lit\\)/\\(.*\\)" ξs) (format "http://%s/%s/%s" "wordyenglish.com" (match-string 2 ξs) (match-string 3 ξs)))

           (t ξs )))
    ξs
    ))

(defun remove-uri-fragment (φhref-value)
  "remove URL φhref-value fragment, anything after first 「#」 char, including it.
See also `split-uri-hashmark'"
  ;; test
  ;; (remove-uri-fragment "a#b") ; "a"
  ;; (remove-uri-fragment "#3")  ; ""
  ;; (remove-uri-fragment "4")  ; "4"
  ;; (remove-uri-fragment "#")   ; ""
  ;; (remove-uri-fragment "")  ; ""
  (let ((ξx (string-match-p "#" φhref-value )))
    (if ξx
        (substring φhref-value 0 ξx)
      φhref-value )))

(defun split-uri-hashmark (φhref-value)
  "Split a URL φhref-value by 「#」 char, return a vector.
 e.g. \"y.html#z\" ⇒ [\"y.html\", \"#z\"]

Examples:
 「a#b」 ⇒ 「a」 「#b」
 「#」 ⇒ 「」 「#」
 「#3」 ⇒ 「」 「#3」
 「3#」 ⇒ 「3」 「#」
 「4」 ⇒  「4」 「」
 「」 ⇒  「」 「」

See also: `remove-uri-fragment'"
  ;; test
  ;; (split-uri-hashmark "a#b") ; ["a" "#b"]
  ;; (split-uri-hashmark "#3")  ; ["" "#3"]
  ;; (split-uri-hashmark "#")   ; ["" "#"]
  ;; (split-uri-hashmark "4")  ; ["4" ""]
  ;; (split-uri-hashmark "")  ; ["" ""]
  (let ((ξx (string-match-p "#" φhref-value )))
    (if ξx
        (vector (substring φhref-value 0 ξx) (substring φhref-value ξx))
      (vector φhref-value "" ))))



(defun file-moved-p (φfpath φmoved-dirs )
  "Return true if either paths are in φmoved-dirs list or as a subdir.
φfpath is a full path to a file.
φmoved-dirs is a list/sequence of file full paths.
Return true if φfpath is in φmoved-dirs or is a subdir of φmoved-dirs.
Technically, if any string in φmoved-dirs is a prefix of φfpath."
  (let ( ( ξfound nil) ( ξi 0))
    (while (and (not ξfound) (< ξi (length φmoved-dirs)))
      (setq ξfound (string-match-p (concat "\\`" (regexp-quote (elt φmoved-dirs ξi))) φfpath ))
      (setq ξi (1+ ξi)))
    ξfound
    ))
;; test
;; (file-moved-p "abc/d" ["abc/d" "don/" "12/3/"] ) ; true, because “abc/d” equal to one of the moved dir
;; (file-moved-p "abc/d/e" ["abc/d" "don/" "12/3/"] ) ; true, because “abc/d/e” is subdir of “abc/d”
;; (file-moved-p "abc/" ["abc/d" "don/" "12/3/"] ) ; false, because “abc/” isn't in any of the moved dirs

(defun local-url-to-file-path (φlocal-file-url)
  "Turn a localhost file URL LOCALFILEURL into a file full path.

φlocal-file-url must be a full path.

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
    (replace-regexp-pairs-in-string φlocal-file-url
                                    [
                                     ["\\`file://localhost" ""]
                                     ["\\`file://" ""]
                                     ["\\`/\\([A-Za-z]\\):" "\\1:"] ; Windows C:\\
                                     ["\\`C:" "c:"] ; need because a bug in `file-relative-name', it doesn't work when path C: is cap
                                     ["\\\\" "/"]   ; Windows \ → /
                                     ]
                                    "FIXEDCASE"
                                    )))

(defun windows-style-path-to-unix  (φfpath)
  "Turn a MS Windows style full path ΦFPATH to unix style.
Note: This drops the drive letter.

For example:
 C:\\Users\\xah\\web\\emacs\\emacs.html
becomes
 /Users/xah/web/emacs/emacs.html

TODO: The drive letter is removed. Not sure whether that should be part of this function. But emacs 23.2's `file-relative-name' has a bug. It does not work when there's a drive letter is capitalized."
  (replace-regexp-in-string "\\`[A-Za-z]:" ""
                            (replace-regexp-in-string "\\\\" "/" φfpath t t)))

(defun xahsite-web-path-to-filepath (φinput-str &optional φdefault-dir)
  "Returns a file full path of φinput-str.
φinput-str can have any of these form:

 x.html (relative path)
 c:/Users/h3/web/ergoemacs_org/a/x.html (Windows style)
 c:\\Users\\h3\\web\\ergoemacs_org\\emacs\\x.html (Windows style)
 /cygdrive/c/Users/h3/web/ergoemacs_org/a/x.html (Cygwin)
 /Users/xah/web/ergoemacs_org/a/x.html (unix style)
 ~/web/ergoemacs_org/a/x.html
 file://… (file URL. See: `local-url-to-file-path')
 http://ergoemacs.org/a/x.html (URL)

if the φinput-str is a relative path, φdefault-dir is used to resolve to full path."
  (let ( (ξs φinput-str))

    ;; (setq ξs (replace-regexp-in-string "^file:///" "" ξs "FIXEDCASE" "LITERAL" ) )
    ;; (setq ξs (replace-regexp-in-string "^/media/OS/Users/h3" "~" ξs "FIXEDCASE" "LITERAL" ) )

    (if (string-match-p "\\`https?://" ξs)
        (progn (setq ξs (xahsite-url-to-filepath ξs "addFileName")))
      (progn
        (when (string-match-p "\\`file://" ξs) (setq ξs (local-url-to-file-path ξs)))
        (when (string-match-p "\\`[A-Za-z]:\\|\\\\" ξs) (setq ξs (windows-style-path-to-unix ξs)))
        (setq ξs (replace-regexp-in-string "\\`/cygdrive/[a-zA-Z]" "" ξs))
        (setq ξs (expand-file-name ξs φdefault-dir))))
    ξs
    ))

(defun path-ends-in-image-suffix-p (φpath)
  "Returns t if φpath ends in .jpg .png .gif .svg, else nil."
  (string-match-p "\.jpg\\'\\|\.png\\'\\|\.gif\\'\\|\.svg\\'" φpath))

(defun xahsite-generate-sitemap (φ-domain-name)
  "Generate a sitemap.xml.gz file of xahsite at doc root.
φ-domain-name must match a existing one."
  (interactive
   (list (ido-completing-read "choose:" '( "ergoemacs.org" "wordyenglish.com" "xaharts.org" "xahlee.info" "xahlee.org" "xahmusic.org" "xahporn.org" "xahsl.org" ))))
  (let (
        (ξ-sitemapFileName "sitemap" )
        (ξ-websiteDocRootPath (concat (xahsite-server-root-path) (replace-regexp-in-string "\\." "_" φ-domain-name "FIXEDCASE" "LITERAL") "/")))

    (print (concat "begin: " (format-time-string "%Y-%m-%dT%T")))

    ;; rename file to backup ~ if already exist
    (let* (
           (f1 (concat ξ-websiteDocRootPath ξ-sitemapFileName ".xml"))
           (f2 (concat f1 ".gz")))
      (when (file-exists-p f1)
        (rename-file f1 (concat f1 "~") t))
      (when (file-exists-p f2)
        (rename-file f2 (concat f2 "~") t)))

    ;; create sitemap buffer
    (let (
          (ξfilePath (concat ξ-websiteDocRootPath ξ-sitemapFileName ".xml"))
          ξsitemapBuffer
          )
      (setq ξsitemapBuffer (find-file ξfilePath))
      (erase-buffer)
      (set-buffer-file-coding-system 'unix)
      (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">
")

      (require 'find-lisp)
      (mapc
       (lambda (ξf)
         (when (not
                (or
                 (string-match "/xx" ξf) ; ; dir/file starting with xx are not public
                 (string-match "403error.html" ξf)
                 (string-match "404error.html" ξf)))
           (with-temp-buffer
             (insert-file-contents ξf)
             (when (not (search-forward "<meta http-equiv=\"refresh\"" nil "noerror"))
               (with-current-buffer ξsitemapBuffer
                 (insert "<url><loc>")
                 (insert (concat "http://" φ-domain-name "/" (substring ξf (length ξ-websiteDocRootPath))))
                 (insert "</loc></url>\n"))))))
       (find-lisp-find-files ξ-websiteDocRootPath "\\.html$"))

      (insert "</urlset>")

      (save-buffer)
      (kill-buffer ξsitemapBuffer)

      (if "zip it"
          (progn
            (shell-command (concat "gzip " ξfilePath))
            (find-file (concat ξfilePath ".gz")))
        (progn (find-file ξfilePath ))))

    (print (concat "finished: " (format-time-string "%Y-%m-%dT%T")))))

(defun xahsite-remove-ads (φp1 φp2)
  "Remove all ads of in region φp1 φp2.

Remove Google adds, Amazon ads, and other ads, Google Analytics
 Tracker code, Disqus code, …."

  (save-restriction
    (narrow-to-region φp1 φp2)

    (replace-pairs-region 1 (point-max)
 [

  ;; google analytics tracker
["<script>(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)})(window,document,'script','//www.google-analytics.com/analytics.js','ga');ga('create','UA-10884311-1','xahlee.info');ga('send','pageview');</script>" ""]

["<script>(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)})(window,document,'script','//www.google-analytics.com/analytics.js','ga');ga('create','UA-10884311-3','ergoemacs.org');ga('send','pageview');</script>" ""]

  ;; Google Ad
[
"<script async src=\"http://pagead2.googlesyndication.com/pagead/js/adsbygoogle.js\"></script>
<!-- 728x90, created 8/12/09 -->
<ins class=\"adsbygoogle\"
     style=\"display:inline-block;width:728px;height:90px\"
     data-ad-client=\"ca-pub-5125343095650532\"
     data-ad-slot=\"8521101965\"></ins>
<script>
(adsbygoogle = window.adsbygoogle || []).push({});
</script>"
""
]

;; Google Plus
["<div class=\"g-plusone\" data-size=\"medium\" data-annotation=\"none\"></div>" ""]

;; social network links ergoemacs.org
["<a href=\"https://twitter.com/ErgoEmacs\"> </a> <a href=\"https://plus.google.com/113859563190964307534/posts\"> </a> <a href=\"https://www.facebook.com/xahlee\"> </a>"
 ""]

;; social network links xahlee.info
["<a href=\"https://twitter.com/xah_lee\"> </a> <a href=\"https://plus.google.com/112757647855302148298\"> </a> <a href=\"http://www.facebook.com/xahlee\"> </a>" ""]

;; twitter
["<div class=\"twitter\"><a href=\"https://twitter.com/share\" class=\"twitter-share-button\" data-via=\"xah_lee\" data-count=\"none\">Tweet</a></div>" ""]
["<div class=\"twitter\"><a href=\"https://twitter.com/share\" class=\"twitter-share-button\" data-via=\"ErgoEmacs\" data-count=\"none\">Tweet</a></div>" ""]
["<div class=\"twitter\"><a href=\"https://twitter.com/share\" class=\"twitter-share-button\" data-via=\"wordy_english\" data-count=\"none\">Tweet</a></div>" ""]

;; facebook
["<div class=\"fb-like\" data-send=\"false\" data-layout=\"button_count\" data-width=\"90\" data-show-faces=\"false\"></div>" ""]

["<script>(function() { var po=document.createElement('script');po.type='text/javascript';po.async=true;po.src='https://apis.google.com/js/plusone.js';var s=document.getElementsByTagName('script')[0];s.parentNode.insertBefore(po,s);})();</script>
<script>!function(d,s,id){var js,fjs=d.getElementsByTagName(s)[0],p=/^http:/.test(d.location)?'http':'https';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+'://platform.twitter.com/widgets.js';fjs.parentNode.insertBefore(js,fjs);}}(document, 'script', 'twitter-wjs');</script>
<div id=\"fb-root\"></div>
<script>(function(d, s, id) { var js, fjs = d.getElementsByTagName(s)[0]; if (d.getElementById(id)) return; js = d.createElement(s); js.id = id; js.src = \"//connect.facebook.net/en_US/all.js#xfbml=1\"; fjs.parentNode.insertBefore(js, fjs); }(document, 'script', 'facebook-jssdk'));</script>"
""]

;; ["<div class=\"showlove\">
;; Like what you read?
;; <div class=\"ppp8745\"><a href=\"buy_xah_emacs_tutorial.html\">Buy Xah Emacs Tutorial</a>
;; <form action=\"https://www.paypal.com/cgi-bin/webscr\" method=\"post\" target=\"_top\">
;; <input type=\"hidden\" name=\"cmd\" value=\"_xclick\" />
;; <input type=\"hidden\" name=\"business\" value=\"JPHAB7F7QZRPC\" />
;; <input type=\"hidden\" name=\"lc\" value=\"US\" />
;; <input type=\"hidden\" name=\"item_name\" value=\"xah emacs tutorial\" />
;; <input type=\"hidden\" name=\"amount\" value=\"15.00\" />
;; <input type=\"hidden\" name=\"currency_code\" value=\"USD\" />
;; <input type=\"hidden\" name=\"button_subtype\" value=\"services\" />
;; <input type=\"hidden\" name=\"no_note\" value=\"0\" />
;; <input type=\"hidden\" name=\"cn\" value=\"Add special instructions to the seller:\" />
;; <input type=\"hidden\" name=\"no_shipping\" value=\"1\" />
;; <input type=\"hidden\" name=\"shipping\" value=\"0.00\" />
;; <input type=\"hidden\" name=\"bn\" value=\"PP-BuyNowBF:btn_buynowCC_LG.gif:NonHosted\" />
;; <input type=\"image\" src=\"https://www.paypalobjects.com/en_US/i/btn/btn_buynowCC_LG.gif\" border=\"0\" name=\"submit\" alt=\"PayPal\" />
;; </form>
;; </div>
;; or share some <span style=\"color:red;font-size:xx-large\">♥</span>
;; <div id=\"share-buttons-97729\"><div class=\"g-plusone\" data-size=\"medium\" data-annotation=\"none\"></div></div><script defer src=\"http://xahlee.info/share_widgets.js\"></script>
;; </div>"
;; ""]

;; paypal
["<div class=\"paypal-donate-60910\"><form action=\"https://www.paypal.com/cgi-bin/webscr\" method=\"post\"><input type=\"hidden\" name=\"cmd\" value=\"_s-xclick\" /><input type=\"hidden\" name=\"hosted_button_id\" value=\"8127788\" /><input type=\"image\" src=\"https://www.paypal.com/en_US/i/btn/btn_donateCC_LG.gif\" name=\"submit\" /><img src=\"https://www.paypal.com/en_US/i/scr/pixel.gif\" alt=\"\" width=\"1\" height=\"1\" /></form></div>
<p>thank you <a href=\"http://xahlee.org/thanks.html\" rel=\"nofollow\">donors</a></p>
<br />"
""]

  ;; Paypal donate
  [
   "<div class=\"ppp8745\"><form action=\"https://www.paypal.com/cgi-bin/webscr\" method=\"post\"><input type=\"hidden\" name=\"cmd\" value=\"_s-xclick\" /><input type=\"hidden\" name=\"hosted_button_id\" value=\"Y4V2F8TA949M2\" /><input type=\"image\" src=\"https://www.paypal.com/en_US/i/btn/btn_paynowCC_LG.gif\" name=\"submit\" alt=\"PayPal\" /><img src=\"https://www.paypal.com/en_US/i/scr/pixel.gif\" alt=\"\" width=\"1\" height=\"1\" /></form></div>"
   ""]

["<section class=\"buy-book\">Buy <a href=\"buy_xah_emacs_tutorial.html\">Xah Emacs Tutorial</a>. Master emacs benefits for life.
<form action=\"https://www.paypal.com/cgi-bin/webscr\" method=\"post\" target=\"_top\">
<input type=\"hidden\" name=\"cmd\" value=\"_xclick\" />
<input type=\"hidden\" name=\"business\" value=\"JPHAB7F7QZRPC\" />
<input type=\"hidden\" name=\"lc\" value=\"US\" />
<input type=\"hidden\" name=\"item_name\" value=\"xah emacs tutorial\" />
<input type=\"hidden\" name=\"amount\" value=\"15.00\" />
<input type=\"hidden\" name=\"currency_code\" value=\"USD\" />
<input type=\"hidden\" name=\"button_subtype\" value=\"services\" />
<input type=\"hidden\" name=\"no_note\" value=\"0\" />
<input type=\"hidden\" name=\"cn\" value=\"Add special instructions to the seller:\" />
<input type=\"hidden\" name=\"no_shipping\" value=\"1\" />
<input type=\"hidden\" name=\"shipping\" value=\"0.00\" />
<input type=\"hidden\" name=\"bn\" value=\"PP-BuyNowBF:btn_buynowCC_LG.gif:NonHosted\" />
<input type=\"image\" src=\"https://www.paypalobjects.com/en_US/i/btn/btn_buynowCC_LG.gif\" border=\"0\" name=\"submit\" alt=\"PayPal\" />
</form></section>"
""]

["<li><a href=\"buy_xah_emacs_tutorial.html\">Buy</a></li>"
""]

["<li><a href=\"../emacs/buy_xah_emacs_tutorial.html\">Buy</a></li>"
""]

["<li><a href=\"emacs/buy_xah_emacs_tutorial.html\">Buy</a></li>"
""]

  ;; Disqus code
  [
   "<div id=\"disqus_thread\"></div><script>(function(){var dsq=document.createElement('script');dsq.type='text/javascript';dsq.async=true;dsq.src='http://xahlee.disqus.com/embed.js';(document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);})();</script><a href=\"http://disqus.com\" class=\"dsq-brlink\">blog comments powered by <span class=\"logo-disqus\">Disqus</span></a>"
   ""
   ]

  ;; amazon ad
["<iframe src=\"http://rcm-na.amazon-adsystem.com/e/cm?t=xahhome-20&o=1&p=8&l=as1&asins=B00COR29XI&ref=tf_til&fc1=000000&IS2=1&lt1=_blank&m=amazon&lc1=0000FF&bc1=000000&bg1=FFFFFF&f=ifr\" style=\"width:120px;height:240px;\" scrolling=\"no\" marginwidth=\"0\" marginheight=\"0\" frameborder=\"0\"></iframe><br />" ""]

[
"<div class=\"job-α\"><a href=\"http://xahlee.org/PageTwo_dir/Personal_dir/Xah_Lee_Resume.html\">Programer for Hire</a></div>"
""]

  ;; 1and1 web hosting
  [
"<a href=\"http://www.1and1.com/?k_id=10914806\" target=\"_blank\" rel=\"nofollow\"><img src=\"http://adimg.uimserv.net/1und1/Werbemittel_US/wh_an_180x150.gif\" width=\"180\" height=\"150\" border=\"0\"/></a>"
   ""
   ]

[
"<div class=\"¤\"><a href=\"http://ode-math.com/\" rel=\"nofollow\">Differential Equations, Mechanics, and Computation</a></div>"
""]

                           ]
                          )

    (replace-regexp-pairs-region 1 (point-max)
 [

["<div class=\"showlove\">[[:ascii:][:nonascii:]]+<script defer src=\"http://xahlee.info/share_widgets.js\"></script>
</div>"
""]

["<div class=\"ad66704\">[\n ]*</div>" ""]

["<div class=\"ads-bottom-65900\">[\n ]*</div>" ""]

["<aside id=\"id1\">[[:ascii:]]+?</aside>" ""]

["<div class=\"share-buttons\">[\n ]*</div>" ""]

["<div class=\"¤xd\">[^<]+?</div>" ""]

["<div class=\"¤\">[^<]+?</div>" ""]

[ "<script charset=\"utf-8\" src=\"http://ws.amazon.com[^<]+?</script>" ""]

["<div class=\"¤tla\"><a href=\"\\([^\"]+?\\)\">\\([^<]+?\\)</a></div>" ""]

["<script><!--
amazon_ad_tag .+?</script>
<script src=\"http://www.assoc-amazon.com/s/ads.js\"></script>"
 ""]

;; ["<div class=\"showlove\">
;; Like what you read\\?
;; <div class=\"ppp8745\"><a href=\"buy_xah_emacs_tutorial.html\">Buy Xah Emacs Tutorial</a>
;; <form action=\"https://www.paypal.com/cgi-bin/webscr\" method=\"post\" target=\"_top\">
;; <input type=\"hidden\" name=\"cmd\" value=\"_xclick\" />
;; <input type=\"hidden\" name=\"business\" value=\"JPHAB7F7QZRPC\" />
;; <input type=\"hidden\" name=\"lc\" value=\"US\" />
;; <input type=\"hidden\" name=\"item_name\" value=\"xah emacs tutorial\" />
;; <input type=\"hidden\" name=\"amount\" value=\"15.00\" />
;; <input type=\"hidden\" name=\"currency_code\" value=\"USD\" />
;; <input type=\"hidden\" name=\"button_subtype\" value=\"services\" />
;; <input type=\"hidden\" name=\"no_note\" value=\"0\" />
;; <input type=\"hidden\" name=\"cn\" value=\"Add special instructions to the seller:\" />
;; <input type=\"hidden\" name=\"no_shipping\" value=\"1\" />
;; <input type=\"hidden\" name=\"shipping\" value=\"0.00\" />
;; <input type=\"hidden\" name=\"bn\" value=\"PP-BuyNowBF:btn_buynowCC_LG.gif:NonHosted\" />
;; <input type=\"image\" src=\"https://www.paypalobjects.com/en_US/i/btn/btn_buynowCC_LG.gif\" border=\"0\" name=\"submit\" alt=\"PayPal\" />
;; </form>
;; </div>
;; or share some <span style=\"color:red;font-size:xx-large\">♥</span>
;; <div id=\"share-buttons-97729\"><div class=\"g-plusone\" data-size=\"medium\" data-annotation=\"none\"></div></div><script defer src=\"http://xahlee.info/share_widgets.js\"></script>
;; </div>"
;; ""]



;; ["<div id=\"disqus_thread\"></div><script>.+?</script><a href.+Disqus</span></a>" ""]
;; ["<footer>.+?</footer>" ""]

]
"FIXEDCASE" "LITERAL")
    )

;; (replace-pairs-region 1 (point-max)
;;  [
;; ;; paypal
;; ["9158656425"
;; ""]
;; ]
;;  )

)
