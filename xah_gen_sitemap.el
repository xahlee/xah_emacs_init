;; -*- coding: utf-8; lexical-binding: t; -*-
;; 2018-09-04

(require 'seq)

(setq xah-web-root-path "/Users/xah/web/" )

(defvar xahsite-external-docs nil "A vector of dir paths.")
(setq  xahsite-external-docs
 [
  "ergoemacs_org/emacs_manual/"
  "wordyenglish_com/arabian_nights/xx_full_2017-05-13/"
  "xahlee_info/REC-SVG11-20110816/"
  "xahlee_info/clojure-doc-1.8/"
  "xahlee_info/css_2.1_spec/"
  "xahlee_info/css_transitions/"
  "xahlee_info/javascript_ecma-262_5.1_2011/"
  "xahlee_info/javascript_ecma-262_6_2015/"
  "xahlee_info/javascript_es2016/"
  "xahlee_info/javascript_es6/"
  "xahlee_info/jquery_doc/"
  "xahlee_info/node_api/"
  "xahlee_info/ocaml_doc/"
  "xahlee_info/python_doc_2.7.6/"
  "xahlee_info/python_doc_3.3.3/"
  ])

(defun xahsite-generate-sitemap (@domain-name)
  "Generate a sitemap.xml.gz file of xahsite at doc root.
@domain-name must match a existing one.
Version 2018-09-04"
  (interactive
   (list (ido-completing-read "choose:" '( "ergoemacs.org" "wordyenglish.com" "xaharts.org" "xahlee.info" "xahlee.org" "xahmusic.org" "xahporn.org" "xahsl.org" ))))
  (let (
        ($sitemapFileName "sitemap.xml" )
        ($websiteDocRootPath (concat xah-web-root-path (replace-regexp-in-string "\\." "_" @domain-name "FIXEDCASE" "LITERAL") "/")))
    ;; (print (concat "begin: " (format-time-string "%Y-%m-%dT%T")))
    (let (
          ($filePath (concat $websiteDocRootPath $sitemapFileName ))
          ($sitemapBuffer (generate-new-buffer "sitemapbuff")))
      (with-current-buffer $sitemapBuffer
        (set-buffer-file-coding-system 'unix)
        (insert "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">
"))
      (mapc
       (lambda ($f)
         (setq $pageMoved-p nil)
         (when (not (or
                     (string-match "/xx" $f) ; ; dir/file starting with xx are not public
                     (string-match "403error.html" $f)
                     (string-match "404error.html" $f)))
           (with-temp-buffer
             (insert-file-contents $f nil 0 100)
             (when (search-forward "page_moved_64598" nil t)
               (setq $pageMoved-p t)))
           (when (not $pageMoved-p)
             (with-current-buffer $sitemapBuffer
               (insert "<url><loc>"
                       "http://" @domain-name "/" (substring $f (length $websiteDocRootPath))
                       "</loc></url>\n"
                       )))))
       (seq-filter
        (lambda (path)
          (not (seq-some
                (lambda (x) (string-match x path))
                xahsite-external-docs
                )))
        (directory-files-recursively $websiteDocRootPath "\\.html$" )))
      (with-current-buffer $sitemapBuffer
        (insert "</urlset>")
        (write-region (point-min) (point-max) $filePath nil 3)
        (kill-buffer ))
      (find-file $filePath)
      )
    ;; (print (concat "done: " (format-time-string "%Y-%m-%dT%T")))
    ))

(defun xahsite-generate-sitemap-all ()
  "do all
2016-08-15"
  (interactive)
  (xahsite-generate-sitemap "ergoemacs.org" )
  (xahsite-generate-sitemap "wordyenglish.com" )
  (xahsite-generate-sitemap "xaharts.org" )
  (xahsite-generate-sitemap "xahlee.info" )
  (xahsite-generate-sitemap "xahlee.org" )
  (xahsite-generate-sitemap "xahmusic.org" )
  (xahsite-generate-sitemap "xahporn.org" )
  (xahsite-generate-sitemap "xahsl.org"  ))

