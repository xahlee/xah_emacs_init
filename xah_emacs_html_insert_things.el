;; -*- coding: utf-8; lexical-binding: t; -*-
;; some custome string functions for working with HTML
;; 2011-05-28
;;   Xah Lee
;; ∑ http://xahlee.org/





(defun xah-html-insert-date-tag ()
  "Insert a date tag."
  (interactive)
  (when (use-region-p)
    (delete-region (region-beginning) (region-end) )
    )
  (insert (concat "<div class=\"date-xl\"><time>" (format-time-string "%Y-%m-%d") "</time></div>\n\n\n" ))
  (backward-char 1)
  )



(defun xah-html-insert-midi ()
  "Insert a midi audio markup."
  (interactive)
  (insert "<div class=\"obj\">
<object type=\"application/x-midi\" data=\"../../ClassicalMusic_dir/midi/liszt/Transcendetal_Etudes_dir/12_chasse.mid\" width=\"300\" height=\"20\">
<param name=\"src\" value=\"../../ClassicalMusic_dir/midi/liszt/Transcendetal_Etudes_dir/12_chasse.mid\">
<param name=\"autoStart\" value=\"0\">
</object>
<p class=\"cpt\">Liszt's transcendental etude #12.
<a href=\"../ClassicalMusic_dir/midi/liszt/Transcendetal_Etudes_dir/12_chasse.mid\">midi file ♪</a>.</p>
</div>
")
  )

