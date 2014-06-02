;-*- coding: utf-8 -*-
; Xah Lee's emacs customization related to google earth, and also a Geogebra function that work similarly.

; 2007-10, 2011-10-21
;   Xah Lee
; ∑ http://xahlee.org/



;; ✈ 🌎
;; ⊕ 🌐

(defun grab-lines (n)
"Delete the next n lines and return a list
where each element is a line."
(interactive)
(beginning-of-line)
(let (t1 t2 cl (lines '()))
  (dotimes (x n)
    (progn
      (setq t1 (point))
      (move-end-of-line nil)
      (setq t2 (point))
      (setq cl (buffer-substring-no-properties t1 t2))
      (delete-region t1 t2)
      (delete-char 1)
      (push cl lines)
      )
    )
  (setq lines (reverse lines))
;  (prin1 lines (current-buffer))
))

(defun insert-google-map-link (&optional φtitle φcoord)
  "Insert HTML link to Google Map.

φtitle is the φtitle for the HTML link.
φcoord is a vector [x y] where x is longitude and y is latitude. Each must be a decimal number.

Example of inserted text:
 <a href=\"http://maps.google.com/maps?q=25.269536%2C82.990723\" title=\"Petrified Forest National Park\">🌐</a>"
  (interactive)
  (let ( ξtitle ξy ξx)
    (setq ξtitle (if φtitle φtitle ""))
    (if φcoord
        (progn
          (setq ξx (elt φcoord 0))
          (setq ξy (elt φcoord 1))
          )
      (progn
        (setq ξx "x�")
        (setq ξy "y�") ) )
    (insert "<a href=\"http://maps.google.com/maps?q=" (number-to-string ξy) "%2C" (number-to-string ξx) "\" title=\"" ξtitle "\" target=\"_blank\">🌐</a>\n")))

(defun insert-google-earth-link (&optional φtitle φfilePath)
  "Insert a HTML markup for link to a local Goole Earth file.
 “φtitle” is the “title” attribute in the anchor link.
 “file-path” is the full path to the KML file.
Here's a sample inserted text:
<a href=\"../kml/las_vegas.kmz\" title=\"Las Vegas\">🌎</a>"
  (interactive)
  (insert (format "<a href=\"%s\" title=\"%s\">🌎</a>\n" (if φfilePath (xahsite-filepath-to-url φfilePath) "�") (if φtitle φtitle "�") )) )

(defun insert-kml (&optional φ-kml-title φ-lon-lat φ-source-fpath)
  "Insert a simple Google Earth KML markup template.
 ΞKMLTITLE is the name to use for the <name> tag.
φ-lon-lat is a vector [longitude latitude]. They must be real numbers.
 SOURCEFILEPATH is the file that links to this kml file,
used in the <description> tag."
  (interactive)
  (let (coord-x coord-y)
    (when (not φ-kml-title) (setq φ-kml-title "�"))
    (if φ-lon-lat
        (progn
          (setq coord-x (elt φ-lon-lat 0))
          (setq coord-y (elt φ-lon-lat 1))
          )
      (progn
        (setq coord-x 0)
        (setq coord-y 0) ) )

    (insert

     (format "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<kml xmlns=\"http://www.opengis.net/kml/2.2\">
<Placemark>
<name>%s</name>
<description>
%s
</description>
<Point><coordinates>%s,%s</coordinates></Point>
</Placemark>
</kml>
"
             φ-kml-title
             (if φ-source-fpath (format "See: %s" (xahsite-filepath-to-url φ-source-fpath)) "")
             (number-to-string coord-x)
             (number-to-string coord-y) ) )))

(defun latitude-longitude-decimalize (φlatlon)
  "Convert latitude longitude string ΦLATLON in minutes second format to decimal.

For example: 「\"37°26′36.42″N 06°15′14.28″W\"」
becomes 「[37.44345 -6.253966666666667]」"
  (interactive)
  (let (ξtmpPair (tt2 φlatlon) ξlatStr ξlatNum ξlonStr ξlonNum
 ξdeg ξmin ξsec ξsign (ξc (/ 1.0 60.0))
)

    (setq tt2 (replace-regexp-in-string "''" "″" tt2 t t) )
    (setq tt2 (replace-regexp-in-string "\"" "″" tt2 t t) )
    (setq tt2 (replace-regexp-in-string "'" "′" tt2 t t) )

    (setq ξtmpPair (split-string tt2 " +"))
    (when (not (equal (length ξtmpPair) 2)) (user-error "Error: input can contain only one space"))

    (setq ξlatStr (elt ξtmpPair 0))
    (setq ξlonStr (elt ξtmpPair 1))
    (if (string-match "\\`\\([0-9]+\\)°\\([0-9]+\\)′\\([.0-9]+\\)″\\(.?\\)\\'" ξlatStr )
        (progn
            (setq ξdeg  (string-to-number (match-string 1 ξlatStr) ))
            (setq ξmin  (string-to-number (match-string 2 ξlatStr) ))
            (setq ξsec  (string-to-number (match-string 3 ξlatStr) ))
            (setq ξsign (match-string 4 ξlatStr))
            (setq ξlatNum (+ ξdeg (* (+ ξmin (* ξsec ξc)) ξc)))
            (cond
                 ((string= (downcase ξsign) "n") nil)
                 ((string= ξsign "") nil)
                 ((string= (downcase ξsign) "s") (setq ξlatNum (* -1 ξlatNum) ))
                 (t (user-error "Your input is malformed. Your latitude ends with a char that's not N or S"))
                 )
            )
      (progn (user-error "Your latitude is malformed") )
      )

(if (string-match "\\`\\([0-9]+\\)°\\([0-9]+\\)′\\([.0-9]+\\)″\\(.?\\)\\'" ξlonStr )
        (progn
            (setq ξdeg  (string-to-number (match-string 1 ξlonStr) ))
            (setq ξmin  (string-to-number (match-string 2 ξlonStr) ))
            (setq ξsec  (string-to-number (match-string 3 ξlonStr) ))
            (setq ξsign (match-string 4 ξlonStr))
            (setq ξlonNum (+ ξdeg (* (+ ξmin (* ξsec ξc)) ξc)))
            (cond
                 ((string= (downcase ξsign) "e") nil)
                 ((string= ξsign "") nil)
                 ((string= (downcase ξsign) "w") (setq ξlonNum (* -1 ξlonNum) ))
                 (t (user-error "Your input is malformed. Your longitude ends with a char that's not E or W"))
                 )
            )
      (progn (user-error "Your longitude is malformed") )
      )
(vector ξlatNum ξlonNum) ) )

;; (defun minsec-to-dec (φlatlong)
;;   "Convert latitude longitude string ΦLATLONG in minutes second format to decimal.

;; For example: 「37°26′36.42″N 06°15′14.28″W」
;; becomes

;; "
;;   (interactive)
;;   (let (var1)
;;     (setq var1 some)

;;     ))

(defun make-google-earth ()
"Create a KML file and replace the current line as a link to it.

The current line must have data of this format:
‹title›/‹latitude›/‹longitude›

Example:
Las Vegas/36.1027/-115.1730

The line will be replaced to like this:
<a href=\"…/las_vegas.kml\" title=\"Las Vegas\">🌎</a>

 (Note: latitude is y-axis, longitude is x-axis)

The KML file will be created at:
 〔~/web/xahlee_org/kml/‹title›.kml〕."
(interactive)
(let* (
       (p1 (line-beginning-position))
       (p2 (line-end-position))
       (inputStr (buffer-substring-no-properties p1 p2 ))
       (userBufferPath (buffer-file-name))
       (kmlDirRoot (concat (xahsite-server-root-path) "xaharts_org/kml/"))
       (titleCoordList (split-string inputStr "/"))
       (kmlFileTitle (elt titleCoordList 0))
       (coord-y (trim-string (replace-regexp-in-string "°" "" (elt titleCoordList 1))))
       (coord-x (trim-string (replace-regexp-in-string "°" "" (elt titleCoordList 2))))
       (kmlFilePath (concat kmlDirRoot (asciify-text (replace-pairs-in-string (trim-string (elt (split-string kmlFileTitle ",") 0)) [[" " "_"] ["," "_"]])) ".kml"))
       doit-p
       )

  (message "kmlFilePath 「%s」" kmlFilePath)
  (setq doit-p t)
  (when (file-exists-p kmlFilePath)
    (setq doit-p nil)
    (setq doit-p (y-or-n-p (format "File exist at 「%s」\nDo you want to replace it?" kmlFilePath)))
    )

  (when doit-p
    (delete-region p1 p2)
    (insert-google-map-link kmlFileTitle (vector (string-to-number coord-x) (string-to-number coord-y)))
    (insert-google-earth-link kmlFileTitle kmlFilePath)
    (find-file kmlFilePath)
    (erase-buffer)
    (insert-kml kmlFileTitle (vector (string-to-number coord-x) (string-to-number coord-y)) userBufferPath)
    (search-backward "<description>") (forward-char 14)
    (nxml-mode)
    (save-buffer)
    )
  ))



(defun insert-ggb-link (φ-file-core-name φ-file-title)
  "Insert HTML link to GeoGebra (“.ggb”) file."
  (interactive)
  (insert "<a class=\"ggb\" href=\"../ggb/" φ-file-core-name ".html\">" φ-file-title "</a>"))

(defun make-ggb ()
"Create a Geogebra file set and link.

This function will take 2 lines the cursor is on as input,
create a Geogebra file (.ggb),
create a HTML file that embed the ggb applet,
and insert a link to the HTML file in the current buffer.

The cursor must be on 2 lines separated by empty lines.
The lines are:

fileCoreName
fileTitle

For Example:

ellipse_trace
Ellisp Tracing

make-ggb will then create the files at:

~/web/SpecialPlaneCurves_dir/ggb/ellipse_trace.html
~/web/SpecialPlaneCurves_dir/ggb/ellipse_trace.ggb

and the HTML file's <title> tag content will be “Ellisp Tracing”.
The ggb file is copied from a template file at
~/web/SpecialPlaneCurves_dir/ggb/x-template.ggb
The HTML file is copied from a template file at
~/web/SpecialPlaneCurves_dir/ggb/x-template.html

Finally, the 2 input lines will be replaced by this link:
<p><img src=\"../../ics/ggb.gif\" alt=\"ggb icon\"> <a href=\"../ggb/ellipse_trace.html\">Ellisp Tracing</a></p>"
(interactive)
(let (fileCoreName fileTitle sl vl ggbFileName htmlFileName dirPath linkBackRelativePath linkBackTitle)

  (search-backward "\n\n")
  (search-forward "\n\n")
  (setq sl '(fileCoreName fileTitle)) ;; sl = symbol list; vl = value list
  (setq vl (grab-lines 2))
  (while sl (set (pop sl) (pop vl)))

  ;; returns this "../Ellipse_dir/ellipse.html"
  (setq linkBackRelativePath (concat ".." (substring (buffer-file-name) 37)))
  (setq linkBackTitle (xhm-get-html-file-title (buffer-file-name)))
  (setq dirPath "~/web/SpecialPlaneCurves_dir/ggb/")
  (setq ggbFileName (concat dirPath fileCoreName ".ggb"))
  (setq htmlFileName (concat dirPath fileCoreName ".html"))

  (insert-ggb-link fileCoreName fileTitle)
  (insert "\n")
  (copy-file (concat dirPath "x-template.ggb") ggbFileName)
  (copy-file (concat dirPath "x-template.html") htmlFileName)

  (let (mybuff (case-replace nil) (case-fold-search nil))
    (setq mybuff (find-file htmlFileName))
    (goto-char (point-min))
    (while (search-forward "「fileTitle」" nil t) (replace-match fileTitle nil t))
    (goto-char (point-min))
    (while (search-forward "「fileCoreName」" nil t) (replace-match fileCoreName nil t))
    (goto-char (point-min))
    (while (search-forward "「linkBackRelativePath」" nil t) (replace-match linkBackRelativePath nil t))
    (goto-char (point-min))
    (while (search-forward "「linkBackTitle」" nil t) (replace-match linkBackTitle nil t))
    (save-buffer)
    (kill-buffer mybuff)
)

 (shell-command (concat "open " ggbFileName))
))
