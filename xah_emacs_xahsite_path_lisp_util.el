;; -*- coding: utf-8; lexical-binding: t; -*-
;; part of Xah Lee's emacs init file.
;; 2011-05-27
;; Xah Lee,
;; ∑ http://xahlee.org/

(require 'subr-x) ; string-trim

(defun xahsite-server-root-path ()
  "Returns the full path of xah lee website local file web root.
Ends in a slash.
e.g. c:/Users/h3/web/"
(expand-file-name "~/web/")
)

(defvar xahsite-domain-to-path-alist nil "alist of domain and path
2017-09-03")

(setq xahsite-domain-to-path-alist
      '(
        ("ergoemacs.org" . "ergoemacs_org")
        ("wordyenglish.com" . "wordyenglish_com")
        ( "wordyenglish.info" . "wordyenglish_info")
        ("xaharts.org" . "xaharts_org")
        ("xahlee.info" . "xahlee_info")
        ("xahlee.org" . "xahlee_org")
        ("xahmusic.org" . "xahmusic_org")
        ("xahporn.org" . "xahporn_org")
        ("xahsl.org" . "xahsl_org")))

(setq xahsite-domain-names (mapcar (lambda (x) (car x)) xahsite-domain-to-path-alist))


(defun xahsite-local-link-p (@href-value)
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
  ;; (not (string-match-p "\\`https?://\\|\\`mailto:\\|\\`irc:\\|\\`ftp:\\|\\`javascript:" @href-value) )

  (cond
   ((string-match-p "^//" @href-value) nil)
   ((string-match-p "^http://" @href-value) nil)
   ((string-match-p "^https://" @href-value) nil)
   ((string-match-p "^mailto:" @href-value) nil)
   ((string-match-p "^irc:" @href-value) nil)
   ((string-match-p "^ftp:" @href-value) nil)
   ((string-match-p "^javascript:" @href-value) nil)
   (t t)))

(defun xahsite-url-is-xah-website-p (@url)
  "Returns t if @url is a xah website, else nil.

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
            (when (string-match-p (format "\\`http://\\(www\\.\\)*%s\.*/*" (regexp-quote x)) @url)
              (throw 'myloop t)))
          xahsite-domain-names)
    nil
    ))
;; test cases
;; (xahsite-url-is-xah-website-p "yahoo.com")             ; nil. not xah site.
;; (xahsite-url-is-xah-website-p "http://yahoo.com")      ; nil. not xah site.
;; (xahsite-url-is-xah-website-p "http://xahlee.org")     ; t
;; (xahsite-url-is-xah-website-p "xahlee.org")            ; nil. just domain name.
;; (xahsite-url-is-xah-website-p "http://w.xahlee.org") ; nil
;; (xahsite-url-is-xah-website-p "http://www.xahlee.org") ; t
;; (xahsite-url-is-xah-website-p "http://ergoemacs.org/") ; t
;; (xahsite-url-is-xah-website-p "http://www.ergoemacs.org/") ; t

(defun xahsite-is-link-to-xahsite-p (@href-value)
  "Returns true if @href-value points to a xah website, else false.
@href-value is the string in 「<a href=\"…\">」 or 「<img src=\"…\">」 or any such.

Technically, returns true if @href-value is a local link (relative file path) or is URL to xah site 「http://…‹xah domain›/」.

See: `xahsite-local-link-p', `xahsite-url-is-xah-website-p'."
  (if (xahsite-local-link-p @href-value)
      t
    (xahsite-url-is-xah-website-p @href-value)))



(defun xahsite-url-to-domain-name (@url)
  "Returns the domain name of a xah site.
e.g. http://ergoemacs.org/emacs/emacs.html ⇒ ergoemacs.org
"
(replace-regexp-in-string "\\`http://\\(www\\.\\)*\\([^.]+\\)\\.\\(info\\|org\\|com\\)/.+" "\\2.\\3" @url) )

(defun xahsite-get-domain-of-local-file-path (@abs-path)
  "Returns the domain name of full path @abs-path belong to.
e.g. 「c:/Users/h3/web/ergoemacs_org/emacs/xyz.html」
returns 「ergoemacs.org」.

This function depends on `xahsite-server-root-path'."
  (let ((case-fold-search nil)
        $str
        ($pathPart
         (string-remove-prefix
          (downcase (xahsite-server-root-path))
          (downcase @abs-path))))
    (if (string-match "\\`\\([^/]+?\\)/" $pathPart )
        (progn
          (setq $str (match-string 1 $pathPart))
          (replace-regexp-in-string "_" "." $str "FIXEDCASE" "LITERAL"))
      (error "「%s」 is not a full path for xah site." @abs-path ))))

(defun xahsite-get-path-relative-to-domain (@fpath)
  "Returns the path relative to that file's domain's root dir.
e.g. 「c:/Users/h3/web/ergoemacs_org/emacs/xyz.html」
returns 「emacs/xyz.html」"
  (let ((case-fold-search nil))
    (string-match (format "\\`%s[^/]+?/\\(.+\\)" (regexp-quote (xahsite-server-root-path)))
                  (replace-regexp-in-string "\\`C:/" "c:/" @fpath  "FIXEDCASE" "LITERAL"))
    (match-string 1 @fpath)))

(defun xahsite-filepath-to-url (@webpath)
  "Turn my website path @webpath to my site's URL.
For example, the following path:
 C:/Users/xah/web/ergoemacs_org/emacs/emacs.html
or
 /Users/xah/web/ergoemacs_org/emacs/emacs.html
will become:
 http://ergoemacs.org/emacs/emacs.html"
  (format "http://%s/%s" (xahsite-get-domain-of-local-file-path @webpath) (xahsite-get-path-relative-to-domain @webpath)))

(defun xahsite-filepath-to-href-value (@linkFilePath @currentFilePathOrDir)
  "Return a URL or relative path.
All arguments should all be full paths.
If the two paths are in different domain, then return a URL (string starts with “http://”).
Else, return a relative path.

For reverse, see `xahsite-href-value-to-filepath'.
"
  (let ((sameDomain-p (string= (xahsite-get-domain-of-local-file-path @linkFilePath) (xahsite-get-domain-of-local-file-path @currentFilePathOrDir))))
    (if sameDomain-p
        (xah-file-relative-name-emacs24.1.1-fix @linkFilePath (file-name-directory @currentFilePathOrDir))
      (xahsite-filepath-to-url @linkFilePath))))
;; test
;; (xahsite-filepath-to-href-value "c:/Users/h3/web/xahlee_org/arts/blog.html" "c:/Users/h3/web/ergoemacs_org/emacs/emacs23_features.html")
;; (xahsite-filepath-to-href-value "c:/Users/h3/web/ergoemacs_org/index.html" "c:/Users/h3/web/ergoemacs_org/emacs/emacs23_features.html" )
;; (xahsite-filepath-to-href-value "c:/Users/h3/web/ergoemacs_org/emacs/emacs23_features.html" "c:/Users/h3/web/ergoemacs_org/index.html" )

(defun xahsite-href-value-to-filepath (@href-value @host-file-path)
  "Returns the file path of a link to xah website.

@href-value is the link string, in 「href=\"…\"」. The value can be a URL to xahsite or relative path.
@host-file-path is a full path of the host file name or its dir.

For reverse, see `xahsite-filepath-to-href-value'.
See also: `xahsite-url-to-filepath'"
  (if (string-match-p "\\`http://" @href-value)
      (progn (xahsite-url-to-filepath @href-value "addFileName"))
    (progn
      (expand-file-name @href-value (file-name-directory @host-file-path )))))
;; test
;; (xahsite-href-value-to-filepath "http://xahlee.org/Netiquette_dir/death_of_a_troll.html" "c:/Users/h3/web/xahlee_info/comp/Google_Tech_Talk_Lisp_At_JPL_by_Ron_Garret.html")

(defun xahsite-url-to-filepath (@xurl &optional @add-file-name @redirect)
  "Returns the file path of a xah website URL @xurl.

If the optional argument @add-file-name is true, then append “index.html” if the resulting path is a dir.
If the optional argument @redirect is true, then also consider result of http redirect.

This function does not check input is actually a URL, nor if the result path file exists.
Version 2017-09-21"
  ;; test cases:
  ;; (xahsite-url-to-filepath "http://xahlee.org/index.html") ; ⇒ "c:/Users/h3/web/xahlee_org/index.html"
  ;; (xahsite-url-to-filepath "http://xahlee.org/") ; ⇒ "c:/Users/h3/web/http://xahlee.org/index.html"
  ;; (xahsite-url-to-filepath "http://abc.org/x.html") ; ⇒ "c:/Users/h3/web/abc_org/x.html"
  ;; (xahsite-url-to-filepath "some water") ; ⇒ "c:/Users/h3/web/some water"
  (let (
        ($url (xah-html-remove-uri-fragment @xurl))
        $fPath)
    (if (string-match "^file:///"  $url )
        (progn
          (setq $fPath (replace-regexp-in-string "^file:///" "/" $url t t)))
      (progn
        (if (let ((case-fold-search t))
              (string-match "VirtualMathMuseum" $url ))
            (progn
              (setq $fPath (replace-regexp-in-string
                            "http://VirtualMathMuseum.org/"
                            "~/x3dxm/vmm/"
                            $url t t)))
          (progn
            (when @redirect (setq $url (xahsite-url-remap $url)))
            (when @add-file-name (setq $url (replace-regexp-in-string "/\\'" "/index.html" $url)))
            ;; (replace-regexp-in-string "%27" "'" (xah-html-remove-uri-fragment $url))
            (if (xahsite-url-is-xah-website-p $url)
                (setq $fPath
                      (format "%s%s" (xahsite-server-root-path)
                              ;; remove www
                              (replace-regexp-in-string
                               "\\`http://\\(www\\.\\)*\\([^.]+\\)\\.\\(info\\|org\\|com\\)/\\(.*\\)"
                               "\\2_\\3/\\4" $url)))
              $url
              )))))))

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

;; (defun xahsite-url-remap (@xahsite-url)
;;   "Returns a redirected url for a xah website URL @xahsite-url.

;; This function is not complete. i.e. it not contain complete url redirects as specified in web server."
;;   (let (
;;         $domain
;;         $path
;;         ($s @xahsite-url)
;;         ($stop nil)
;;         )
;;     (string-match "\\`http://\\(www\\.\\)*\\([^.]+\\)\\.\\(info\\|org\\|com\\)/\\(.*\\)" $s)
;;     (setq $domain (match-string "\\2.\\3" $s ) )
;;     (setq $path (match-string "\\4" $s ) )

;;     (cond
;;      ((string-match-p "xahlee.org" $domain)
;;       (let (
;;             ($i 0)
;;             ($len (length xahsite-xahlee-org-redirect))
;;             ($pathFirstPart (replace-regexp-in-string "\\`\\([^/]+?\\)/" "\\1" $path) )
;;             )
;;         (while (and (not $stop) (< $i $len) )
;;           (if (string= (elt xahsite-xahlee-org-redirect $i) $pathFirstPart)
;;               (progn
;;                 ;; ...
;;                 (setq $stop t ))
;;             (progn )
;;             )
;;           (setq $i (1+ $i))
;;           )
;;         )
;;       )
;;      ;; ...
;;      )

;;     (setq $s (replace-regexp-in-string "/Periodic_dosage_dir/sanga_pemci/" "/music/" $s "FIXEDCASE" "LITERAL"))
;;     (setq $s (replace-regexp-in-string "xahlee.org/emacs/" "ergoemacs.org/emacs/" $s "FIXEDCASE" "LITERAL"))
;;  ) )

(defun xahsite-url-remap (@xahsite-url)
  "Returns a redirected url for a xah website URL @xahsite-url.

This function is not complete. i.e. it not contain complete url redirects as specified in web server."
  (let (($s @xahsite-url)
        (case-fold-search nil))
    (setq $s
          (cond

           ((string-match-p "\\`http://xahlee\\.org/Periodic_dosage_dir/cmaci_girzu.html" $s) "http://xahlee.info/math/math_index.html" )
           ((string-match-p "\\`http://xahlee\\.org/Periodic_dosage_dir/keyboarding.html" $s) "http://xahlee.info/kbd/keyboarding.html" )
           ((string-match-p "\\`http://xahlee\\.org/Periodic_dosage_dir/unicode.html" $s) "http://xahlee.info/comp/unicode_index.html" )
           ((string-match-p "\\`http://xahlee\\.org/Periodic_dosage_dir/skami_prosa.html" $s) "http://xahlee.info/comp/comp_index.html" )

           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(emacs\\)/\\(.*\\)" $s) (format "http://%s/%s/%s" "ergoemacs.org" (match-string 2 $s) (match-string 3 $s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(emacs_manual\\)/\\(.*\\)" $s) (format "http://%s/%s/%s" "ergoemacs.org" (match-string 2 $s) (match-string 3 $s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(M\\)/\\(.*\\)" $s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 $s) (match-string 3 $s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(MathGraphicsGallery_dir\\)/\\(.*\\)" $s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 $s) (match-string 3 $s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(MathematicaPrograming_dir\\)/\\(.*\\)" $s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 $s) (match-string 3 $s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(PerlMathematica_dir\\)/\\(.*\\)" $s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 $s) (match-string 3 $s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(SpecialPlaneCurves_dir\\)/\\(.*\\)" $s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 $s) (match-string 3 $s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(UnixResource_dir\\)/\\(.*\\)" $s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 $s) (match-string 3 $s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(Wallpaper_dir\\)/\\(.*\\)" $s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 $s) (match-string 3 $s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(cmaci\\)/\\(.*\\)" $s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 $s) (match-string 3 $s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(comp\\)/\\(.*\\)" $s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 $s) (match-string 3 $s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(complex\\)/\\(.*\\)" $s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 $s) (match-string 3 $s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(coq\\)/\\(.*\\)" $s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 $s) (match-string 3 $s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(haskell\\)/\\(.*\\)" $s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 $s) (match-string 3 $s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(java-a-day\\)/\\(.*\\)" $s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 $s) (match-string 3 $s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(js\\)/\\(.*\\)" $s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 $s) (match-string 3 $s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(kbd\\)/\\(.*\\)" $s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 $s) (match-string 3 $s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(linux\\)/\\(.*\\)" $s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 $s) (match-string 3 $s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(math\\)/\\(.*\\)" $s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 $s) (match-string 3 $s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(mswin\\)/\\(.*\\)" $s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 $s) (match-string 3 $s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(ocaml\\)/\\(.*\\)" $s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 $s) (match-string 3 $s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(pascal\\)/\\(.*\\)" $s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 $s) (match-string 3 $s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(perl-python\\)/\\(.*\\)" $s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 $s) (match-string 3 $s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(php\\)/\\(.*\\)" $s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 $s) (match-string 3 $s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(powershell\\)/\\(.*\\)" $s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 $s) (match-string 3 $s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(prog\\)/\\(.*\\)" $s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 $s) (match-string 3 $s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(projective_geometry\\)/\\(.*\\)" $s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 $s) (match-string 3 $s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(ruby\\)/\\(.*\\)" $s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 $s) (match-string 3 $s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(surface\\)/\\(.*\\)" $s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 $s) (match-string 3 $s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(tiling\\)/\\(.*\\)" $s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 $s) (match-string 3 $s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(tree\\)/\\(.*\\)" $s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 $s) (match-string 3 $s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(visual_basic\\)/\\(.*\\)" $s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 $s) (match-string 3 $s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(w\\)/\\(.*\\)" $s) (format "http://%s/%s/%s" "xahlee.info" (match-string 2 $s) (match-string 3 $s)))

           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(music\\)/\\(.*\\)" $s) (format "http://%s/%s/%s" "xahmusic.org" (match-string 2 $s) (match-string 3 $s)))
           ((string-match "\\`http://\\(xahlee\\.org\\)/\\(lit\\)/\\(.*\\)" $s) (format "http://%s/%s/%s" "wordyenglish.com" (match-string 2 $s) (match-string 3 $s)))

           (t $s )))
    $s
    ))



(defun xah-file-moved-p (@fpath @moved-dirs )
  "Return true if either paths are in @moved-dirs list or as a subdir.
@fpath is a full path to a file.
@moved-dirs is a list/sequence of file full paths.
Return true if @fpath is in @moved-dirs or is a subdir of @moved-dirs.
Technically, if any string in @moved-dirs is a prefix of @fpath."
  (let ( ( $found nil) ( $i 0))
    (while (and (not $found) (< $i (length @moved-dirs)))
      (setq $found (string-match-p (concat "\\`" (regexp-quote (elt @moved-dirs $i))) @fpath ))
      (setq $i (1+ $i)))
    $found
    ))
;; test
;; (xah-file-moved-p "abc/d" ["abc/d" "don/" "12/3/"] ) ; true, because “abc/d” equal to one of the moved dir
;; (xah-file-moved-p "abc/d/e" ["abc/d" "don/" "12/3/"] ) ; true, because “abc/d/e” is subdir of “abc/d”
;; (xah-file-moved-p "abc/" ["abc/d" "don/" "12/3/"] ) ; false, because “abc/” isn't in any of the moved dirs

(defun xah-local-url-to-file-path (@local-file-url)
  "Turn a localhost file URL LOCALFILEURL into a file full path.

@local-file-url must be a full path.

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
    (xah-replace-regexp-pairs-in-string @local-file-url
                                    [
                                     ["\\`file://localhost" ""]
                                     ["\\`file://" ""]
                                     ["\\`/\\([A-Za-z]\\):" "\\1:"] ; Windows C:\\
                                     ["\\`C:" "c:"] ; need because a bug in `file-relative-name', it doesn't work when path C: is cap
                                     ["\\\\" "/"]   ; Windows \ → /
                                     ]
                                    "FIXEDCASE"
                                    )))

(defun xahsite-web-path-to-filepath (@input-str &optional @default-dir)
  "Returns a file full path of @input-str.
@input-str can have any of these form:

 x.html (relative path)
 c:/Users/h3/web/ergoemacs_org/a/x.html (Windows style)
 c:\\Users\\h3\\web\\ergoemacs_org\\emacs\\x.html (Windows style)
 /cygdrive/c/Users/h3/web/ergoemacs_org/a/x.html (Cygwin)
 /Users/xah/web/ergoemacs_org/a/x.html (unix style)
 ~/web/ergoemacs_org/a/x.html
 file://… (file URL. See: `xah-local-url-to-file-path')
 http://ergoemacs.org/a/x.html (URL)

if the @input-str is a relative path, @default-dir is used to resolve to full path."
  (let ( ($s @input-str))

    ;; (setq $s (replace-regexp-in-string "^file:///" "" $s "FIXEDCASE" "LITERAL" ) )
    ;; (setq $s (replace-regexp-in-string "^/media/OS/Users/h3" "~" $s "FIXEDCASE" "LITERAL" ) )

    (if (string-match-p "\\`https?://" $s)
        (progn (setq $s (xahsite-url-to-filepath $s "addFileName")))
      (progn
        (when (string-match-p "\\`file://" $s) (setq $s (xah-local-url-to-file-path $s)))
        (when (string-match-p "\\`[A-Za-z]:\\|\\\\" $s) ; change Microsoft Windows style path to unix
          (setq $s (replace-regexp-in-string "\\`[A-Za-z]:" "" (replace-regexp-in-string "\\\\" "/" $s t t))))
        (setq $s (replace-regexp-in-string "\\`/cygdrive/[a-zA-Z]" "" $s))
        (setq $s (expand-file-name $s @default-dir))))
    $s
    ))

(defun xah-find-files-file-predicate-p (@fname _parentdir)
  "return t if @fname is what we want. Else nil.
The arg _parentdir is not used. It is there so that this function can be passed as 2nd arg to `find-lisp-find-files-internal', which requires 2 args.
2016-07-09"
  (interactive)
  (and
   (string-match "\\.html$" @fname)
   (not (string-match "^xx" @fname))
   ))

(defun xah-find-files-dir-predicate-p (@fname @parentdir)
  "return t if @fname is what we want. Else nil.
2016-09-09"
  (and
   (not
    (or
     (string-equal "java8_doc" @fname)
     (string-equal "REC-SVG11-20110816" @fname)
     (string-equal "clojure-doc-1.8" @fname)
     (string-equal "css_2.1_spec" @fname)
     (string-equal "css_transitions" @fname)
     (string-equal "dom-whatwg" @fname)
     (string-equal "html5_whatwg" @fname)
     (string-equal "javascript_ecma-262_5.1_2011" @fname)
     (string-equal "javascript_ecma-262_6_2015" @fname)
     (string-equal "ocaml_doc" @fname)
     (string-equal "javascript_es6" @fname)
     (string-equal "jquery_doc" @fname)
     (string-equal "node_api" @fname)
     (string-equal "php-doc" @fname)
     (string-equal "python_doc_2.7.6" @fname)
     (string-equal "python_doc_3.3.3" @fname)
     (string-match "^xx" @fname)))
   (find-lisp-default-directory-predicate @fname @parentdir)))

(defun xahsite-traverse-dir-file-list (@dirpath)
  "a list of full paths to process"
  (require 'find-lisp)
  (find-lisp-find-files-internal
    @dirpath
    'xah-find-files-file-predicate-p
    'xah-find-files-dir-predicate-p))

(defun xahsite-generate-sitemap-all ()
  "do all
2016-08-15"
  (interactive)
  (require 'find-lisp)
  (xahsite-generate-sitemap "ergoemacs.org" )
  (xahsite-generate-sitemap "wordyenglish.com" )
  (xahsite-generate-sitemap "xaharts.org" )
  (xahsite-generate-sitemap "xahlee.info" )
  (xahsite-generate-sitemap "xahlee.org" )
  (xahsite-generate-sitemap "xahmusic.org" )
  (xahsite-generate-sitemap "xahporn.org" )
  (xahsite-generate-sitemap "xahsl.org"  ))



