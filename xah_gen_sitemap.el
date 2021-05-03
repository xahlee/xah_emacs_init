;; -*- coding: utf-8; lexical-binding: t; -*-
;; version: 2019-06-11
;; home page: /Users/xah/web/ergoemacs_org/emacs/make_sitemap.html

(require 'seq)

(setq xah-web-root-path "/Users/xah/web/" )

(defvar xahsite-external-docs nil "A vector of dir paths. Version 2019-06-11")
(setq  xahsite-external-docs
       [
        "ergoemacs_org/emacs_manual/"
        "xahlee_info/clojure-doc-1.8/"
        "xahlee_info/css_2.1_spec/"
        "xahlee_info/css_transitions/"
        "xahlee_info/js_es2011/"
        "xahlee_info/js_es2015/"
        "xahlee_info/js_es2015_orig/"
        "xahlee_info/js_es2016/"
        "xahlee_info/js_es2018/"
        "xahlee_info/node_api/"
        ])

(defun xahsite-generate-sitemap (@domain-name)
  "Generate a sitemap.xml.gz file of xahsite at doc root.
@domain-name must match a existing one.
Version 2018-09-17"
  (interactive
   (list (ido-completing-read "choose:" '( "ergoemacs.org" "wordyenglish.com" "xaharts.org" "xahlee.info" "xahlee.org" "xahmusic.org" "xahsl.org" ))))
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
        (insert "</urlset>\n")
        (write-region (point-min) (point-max) $filePath nil 3)
        (kill-buffer ))
      (find-file $filePath)
      )
    ;; (print (concat "done: " (format-time-string "%Y-%m-%dT%T")))
    ))

