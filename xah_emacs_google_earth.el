;-*- coding: utf-8 -*-
; Xah Lee's emacs customization related to google earth, and also a Geogebra function that work similarly.

; 2007-10, 2011-10-21
;   Xah Lee
; ∑ http://xahlee.org/



;; ✈ 🌎
;; ⊕ 🌐

(defun xah-grab-lines (*n)
  "Delete the next n lines and return a list
Where each element is a line.
each line does not include the newline character."
  (interactive "p")
  (beginning-of-line)
  (let ((p1 (point))
        p2
        ($lines '()))
    (dotimes ($i (- *n 1)) (forward-line))
    (end-of-line)
    (setq p2 (point))
    (setq $lines (split-string (buffer-substring-no-properties p1 p2) "\n" ))
    (delete-region p1 p2)
    (delete-char 1)
    ;; (print $lines)
    $lines))

(defun xah-insert-google-map-link (&optional *title *latlon)
  "Insert HTML link to Google Map.

*title is the title attribute for the HTML link.
*latlon is a vector [y x] where y is latitude, x is longitude. Each must be a decimal number. See also: `xah-latitude-longitude-decimalize'

Example of inserted text:
 <a href=\"http://maps.google.com/maps?q=40.71277777777778%2C-74.00583333333333\" title=\"▮\" target=\"_blank\">Google Map</a>

URL `http://ergoemacs.org/emacs/elisp_make_google-map_link.html'
Version 2015-05-12"
  (interactive)
  (let ($title $y $x)
    (setq $title (if *title *title ""))
    (if *latlon
        (progn
          (setq $y (elt *latlon 0))
          (setq $x (elt *latlon 1)))
      (progn
        (setq $y "y▮")
        (setq $x "x▮")))
    (insert "<a href=\"http://maps.google.com/maps?q=" (number-to-string $y) "%2C" (number-to-string $x) "\" title=\"" $title "\" target=\"_blank\">Google Map</a>\n")))

(defun xah-insert-google-earth-link (&optional *title *filePath)
  "Insert a HTML markup for link to a local Goole Earth file.
 “*title” is the “title” attribute in the anchor link.
 “file-path” is the full path to the KML file.
Here's a sample inserted text:
<a href=\"../kml/las_vegas.kmz\" title=\"Las Vegas\">🌎</a>"
  (interactive)
  (insert (format "<a href=\"%s\" title=\"%s\">🌎</a>\n" (if *filePath (xahsite-filepath-to-url *filePath) "▮") (if *title *title "▮") )) )

(defun xah-insert-kml (&optional *kml-title *lon-lat *source-fpath)
  "Insert a simple Google Earth KML markup template.
 *kml-title is the name to use for the <name> tag.
*lon-lat is a vector [longitude latitude]. They must be real numbers.
 *source-fpath is the file that links to this kml file,
used in the <description> tag."
  (interactive)
  (let (coord-x coord-y)
    (when (not *kml-title) (setq *kml-title "▮"))
    (if *lon-lat
        (progn
          (setq coord-x (elt *lon-lat 0))
          (setq coord-y (elt *lon-lat 1)))
      (progn
        (setq coord-x 0)
        (setq coord-y 0)))

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
             *kml-title
             (if *source-fpath (format "See: %s" (xahsite-filepath-to-url *source-fpath)) "")
             (number-to-string coord-x)
             (number-to-string coord-y)))))

(defun xah-latitude-longitude-decimalize (*latlon)
  "Convert latitude longitude string *latlon in minutes second format to decimal.
Returns a vector.
For example: 「\"37°26′36.42″N 06°15′14.28″W\"」
becomes 「[37.44345 -6.253966666666667]」
(Note: latitude is “horizontal lines”, longitude is “vertical lines”)

URL `http://ergoemacs.org/emacs/elisp_make_google-map_link.html'
Version 2015-02-08"
  (interactive)
  (let (($tmpPair
         (split-string
          (replace-regexp-in-string "'" "′"
                                    (replace-regexp-in-string "\"" "″"
                                                              (replace-regexp-in-string "''" "″" *latlon t t) t t) t t)
          " +"))
        $latStr $latNum $lonStr $lonNum $deg $min $sec $sign ($c (/ 1.0 60.0)))

    (when (not (equal (length $tmpPair) 2)) (user-error "Error: input can contain only one space"))

    (setq $latStr (elt $tmpPair 0))
    (setq $lonStr (elt $tmpPair 1))
    (if (string-match "\\`\\([0-9]+\\)°\\([0-9]+\\)′\\([.0-9]+\\)″\\(.?\\)\\'" $latStr )
        (progn
          (setq $deg  (string-to-number (match-string 1 $latStr)))
          (setq $min  (string-to-number (match-string 2 $latStr)))
          (setq $sec  (string-to-number (match-string 3 $latStr)))
          (setq $sign (match-string 4 $latStr))
          (setq $latNum (+ $deg (* (+ $min (* $sec $c)) $c)))
          (cond
           ((string= (downcase $sign) "n") nil)
           ((string= $sign "") nil)
           ((string= (downcase $sign) "s") (setq $latNum (* -1 $latNum)))
           (t (user-error "Your input is malformed. Your latitude ends with a char that's not N or S"))))
      (progn (user-error "Your latitude is malformed")))

    (if (string-match "\\`\\([0-9]+\\)°\\([0-9]+\\)′\\([.0-9]+\\)″\\(.?\\)\\'" $lonStr )
        (progn
          (setq $deg  (string-to-number (match-string 1 $lonStr)))
          (setq $min  (string-to-number (match-string 2 $lonStr)))
          (setq $sec  (string-to-number (match-string 3 $lonStr)))
          (setq $sign (match-string 4 $lonStr))
          (setq $lonNum (+ $deg (* (+ $min (* $sec $c)) $c)))
          (cond
           ((string= (downcase $sign) "e") nil)
           ((string= $sign "") nil)
           ((string= (downcase $sign) "w") (setq $lonNum (* -1 $lonNum)))
           (t (user-error "Your input is malformed. Your longitude ends with a char that's not E or W"))))
      (progn (user-error "Your longitude is malformed")))
    (vector $latNum $lonNum)))

;; (defun minsec-to-dec (*latlong)
;;   "Convert latitude longitude string *latlong in minutes second format to decimal.

;; For example: 「37°26′36.42″N 06°15′14.28″W」
;; becomes

;; "
;;   (interactive)
;;   (let (var1)
;;     (setq var1 some)

;;     ))

(require 'subr-x) ; for string-trim

(defun xah-make-google-earth ()
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
         (coord-y (string-trim (replace-regexp-in-string "°" "" (elt titleCoordList 1))))
         (coord-x (string-trim (replace-regexp-in-string "°" "" (elt titleCoordList 2))))
         (kmlFilePath (concat kmlDirRoot (xah-asciify-string (xah-replace-pairs-in-string (string-trim (elt (split-string kmlFileTitle ",") 0)) [[" " "_"] ["," "_"]])) ".kml"))
         doit-p
         )

    (message "kmlFilePath 「%s」" kmlFilePath)
    (setq doit-p t)
    (when (file-exists-p kmlFilePath)
      (setq doit-p nil)
      (setq doit-p (y-or-n-p (format "File exist at 「%s」\nDo you want to replace it?" kmlFilePath))))

    (when doit-p
      (delete-region p1 p2)
      (xah-insert-google-map-link kmlFileTitle (vector (string-to-number coord-y) (string-to-number coord-x)))
      (xah-insert-google-earth-link kmlFileTitle kmlFilePath)
      (find-file kmlFilePath)
      (erase-buffer)
      (xah-insert-kml kmlFileTitle (vector (string-to-number coord-x) (string-to-number coord-y)) userBufferPath)
      (search-backward "<description>") (forward-char 14)
      (nxml-mode)
      (save-buffer))))

(defun xah-google-map-linkify ()
  "Change coordinate under cursor into a Google Map link.

The current line must be one of the following format:
 40°42′46″N 74°00′21″W
 40.71277777777778 -74.00583333333333

The above is ‹latitude› ‹longitude›
 (That's New York City)
 (Note: latitude is y-axis, longitude is x-axis)

If there's a text selection, use that as input.

Sample result:
 <a href=\"http://maps.google.com/maps?q=40.71277777777778%2C-74.00583333333333\" title=\"▮\" target=\"_blank\">Google Map</a>

URL `http://ergoemacs.org/emacs/elisp_make_google-map_link.html'
Version 2016-07-12"
  (interactive)
  (let ($p1 $p2 $input
           $coord-x
           $coord-y
           $coord-y-x
           )
    (if (use-region-p)
        (progn
          (setq $p1 (region-beginning))
          (setq $p2 (region-end)))
      (progn
        (setq $p1 (line-beginning-position))
        (setq $p2 (line-end-position))))

    (setq $input (buffer-substring-no-properties $p1 $p2))
    (if (string-match-p "°" $input)
        (progn
          (setq $coord-y-x (xah-latitude-longitude-decimalize $input))
          (setq $coord-y (aref $coord-y-x 0))
          (setq $coord-x (aref $coord-y-x 1)))
      (progn
        (let ( ($xx (split-string $input  " " "OMIT-NULLS")))
          (setq $coord-y (string-to-number (nth 0 $xx)))
          (setq $coord-x (string-to-number (nth 1 $xx)))
          (setq $coord-y-x (vector $coord-y $coord-x)))))
    (delete-region $p1 $p2)
    (xah-insert-google-map-link "▮" $coord-y-x)))



(defun xah-insert-ggb-link (*file-core-name *file-title)
  "Insert HTML link to GeoGebra (“.ggb”) file."
  (interactive)
  (insert "<a class=\"ggb\" href=\"../ggb/" *file-core-name ".html\">" *file-title "</a>"))

(defun xah-make-ggb ()
  "Create a Geogebra file set and link.

This function will take 2 lines the cursor is on as input,
create a Geogebra file (.ggb),
create a HTML file that embed the ggb applet,
and insert a link to the HTML file in the current buffer.

The cursor must be on 2 lines separated by blank lines.
The lines are:

fileCoreName
fileTitle

For Example:

ellipse_trace
Ellisp Tracing

xah-make-ggb will then create the files at:

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
    (setq vl (xah-grab-lines 2))
    (while sl (set (pop sl) (pop vl)))

    ;; returns this "../Ellipse_dir/ellipse.html"
    (setq linkBackRelativePath (concat ".." (substring (buffer-file-name) 37)))
    (setq linkBackTitle (xah-html-get-html-file-title (buffer-file-name)))
    (setq dirPath "~/web/SpecialPlaneCurves_dir/ggb/")
    (setq ggbFileName (concat dirPath fileCoreName ".ggb"))
    (setq htmlFileName (concat dirPath fileCoreName ".html"))

    (xah-insert-ggb-link fileCoreName fileTitle)
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
      (kill-buffer mybuff))

    (shell-command (concat "open " ggbFileName))))
