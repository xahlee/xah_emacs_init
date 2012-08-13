;; -*- coding: utf-8 -*-
;; part of Xah Lee's emacs init file.
;; 2011-05-28
;; Xah Lee,
;; ∑ http://xahlee.org/

(defun make-blogger-entry ()
  "Make a blogger entry.

Copy the current buffer.
create a new buffer. Make it html-mode.
paste it in.
remove the header and footer.
fix all relative links to http://xahlee.org/ links.
add a “Perm URL with updates: ‹link›” sentence at top.

This new content is ready to be posted to blogger."
  (interactive)
  (let* (
    (bds (get-selection-or-unit 'buffer))
    (mainText (elt bds 0) )
    (p1 (elt bds 1) )
    (p2 (elt bds 2) )
    (thisFilePath (or (buffer-file-name) default-directory))
    (currentDir (file-name-directory thisFilePath))
    (newBuff (generate-new-buffer "*blogger temp*"))
    cssStr permLink)

    (switch-to-buffer newBuff)
    (html-mode)

    (insert mainText)

    ;; get css declaration, if any
    (let (p3 p4 )
      (goto-char 1)
      (if (search-forward "<style type=\"text/css\">" nil t)
          (progn
            (search-backward "<style" )
            (setq p3 (point) )
            (search-forward "</style>")
            (setq p4 (point) )
            (setq cssStr (buffer-substring-no-properties p3 p4))
            )
        (progn
          (setq cssStr "")
          ) ) )

    ;; remove header
    (goto-char 1)
    (when (search-forward "<div class=\"nav\">" nil t)
      (search-forward "</div>")
      (delete-region (point) (point-min) )
      )

    ;; remove fixed strings. ads, banners, donation box.
    (replace-pairs-region 1 (point-max)
 '(

["<script><!--
google_ad_client = \"pub-5125343095650532\";
/* 728x90, created 8/12/09 */
google_ad_slot = \"8521101965\";
google_ad_width = 728;
google_ad_height = 90;
//-->
</script>
<script type=\"text/javascript\"
src=\"http://pagead2.googlesyndication.com/pagead/show_ads.js\">
</script>" ""]

["<script charset=\"utf-8\" src=\"http://ws.amazon.com/widgets/q?rt=tf_sw&amp;ServiceVersion=20070822&amp;MarketPlace=US&amp;ID=V20070822/US/xahhome-20/8002/97c57146-4835-472a-a8d9-d43977e801f5\"></script>" ""]

["<div id=\"disqus_thread\"></div><script>(function(){var dsq=document.createElement('script');dsq.type='text/javascript';dsq.async=true;dsq.src='http://xahlee.disqus.com/embed.js';(document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);})();</script><a href=\"http://disqus.com\" class=\"dsq-brlink\">blog comments powered by <span class=\"logo-disqus\">Disqus</span></a>" ""]

["<script><!--
amazon_ad_tag = \"xahh-20\"; amazon_ad_width = \"728\"; amazon_ad_height = \"90\"; amazon_ad_logo = \"hide\"; amazon_color_border = \"7E0202\";//--></script>
<script src=\"http://www.assoc-amazon.com/s/ads.js\"></script>" ""]

["?tag=xahh-20" "?tag=xahblg-20"]
 ))

;; remove header, author, misc ads boxes, etc.
   (replace-regexp-pairs-region 1 (point-max) '(
["<h1>[^<]+?</h1>" ""]
["<p><span class=\"b3\">[^<]+?</span></p>" ""]
["<p class=\"αuth\">Xah Lee, [-<>/ ,…time0-9]+?</p>" ""]
["<div class=\"¤xd\"><a href=\".+?/ads.html\">Advertise Here</a></div>" ""]
["<div class=\"βds\">[ \n]+</div>" ""]
["<div class=\"βshare\">[[:ascii:]]+?<img src=\"http://www.reddit.com/static/spreddit1.gif\" /></a>
</div>" ""]
))

    ;; remove blogger link
    (goto-char 1)
    (when
        (search-forward "<div class=\"blgcmt\"><a href=" nil t)
      (let (pt1 pt2 )
        (beginning-of-line)
        (setq pt1 (point) )
        (end-of-line)
        (setq pt2 (point) )
        (delete-region pt1 pt2 ) ) )

    ;; remove footer
    (when (progn
            (goto-char (point-max))
            (search-backward "<footer>" nil t))
      (delete-region (line-beginning-position) (point-max) )
      )

    ;; change relative links to full http://… URL
    (goto-char 1)
    (while (search-forward "href=\"" nil t)
      (when (not (looking-at "http"))
        (let (bds myLink fPath)
          (setq bds (bounds-of-thing-at-point 'filename))
          (setq myLink (buffer-substring-no-properties (car bds) (cdr bds)))
          (setq fPath (expand-file-name (concat currentDir myLink)) )
          (delete-region (car bds) (cdr bds))
          (insert (xahsite-filepath-to-url fPath))
          ) ))

    ;; change inline image links from relative to http://xahlee.org/ URL
    (goto-char 1)
    (while (search-forward "src=\"" nil t)
      (when (not (looking-at "http"))
        (let (bds myLink fPath)
          (setq bds (bounds-of-thing-at-point 'filename))
          (setq myLink (buffer-substring-no-properties (car bds) (cdr bds)))
          (setq fPath (expand-file-name (concat currentDir myLink)) )
          (delete-region (car bds) (cdr bds))
          (insert (xahsite-filepath-to-url fPath))
          ) ) )

    ;; (compute-url-from-relative-link thisFilePath)

    ;; insert perm link at top
    (when (and
           (not (string-match "blog\.html$" thisFilePath))
           (not (string-match "Vocabulary_dir" thisFilePath))
           )
      (goto-char 1)
      (setq permLink (xahsite-filepath-to-url thisFilePath))
      (insert "<p>Perm URL with updates: " "<a href=\"" permLink "\">" permLink "</a>" "</p>\n\n")
      )

    ;; insert css string
    (goto-char 1)
    (insert cssStr "\n\n")

(replace-regexp-pairs-region 1 (point-max) '(
["\n\n+" "\n\n"]
))

    (goto-char 1)

    (kill-new (buffer-string))
    ))