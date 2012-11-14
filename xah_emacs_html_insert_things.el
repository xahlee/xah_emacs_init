;; -*- coding: utf-8 -*-
;; some custome string functions for working with HTML
;; 2011-05-28
;;   Xah Lee
;; ∑ http://xahlee.org/


;; inserts generic fixed strings

(defun insert-javascript-tag ()
  "Insert a javascript tag."
  (interactive)
  (insert "<script src=\".js\"></script>")
  (backward-char 14)
  )


(defun insert-keywords-tag ()
"Insert the HTML keywords meta tag."
(interactive)
(insert "<meta name=\"keywords\" content=\"ttt\" />\n")
(insert "<meta name=\"description\" content=\"ttt\" />")
(backward-char 2)
)


;; inserts fixed strings, particular to xahsite

(defun insert-lyrics-header ()
  "Insert a XahLee.org customized lyrics meta data tag."
  (interactive)
  (insert "<pre class=\"lycif\">
Title: 
Date: 
Singer: 
Lyrics: 
Music: 
English translation: 李杀 (Xah Lee)
</pre>")
  (search-backward "Singer")
  (backward-char)
  )

(defun insert-lyrics-table ()
  "Insert a html 2-columns table for holding english chinese lyrics."
  (interactive)
  (insert 
"<table>
<tr>
<td>
<pre class=\"lyc\">
�
</pre>
</td>
<td>
<pre class=\"lyc\">
�
</pre>
</td>
</tr>
</table>"
)
  (search-backward "�")
  (search-backward "�")
  )

(defun insert-screen-filler ()
"Insert a custome HTML <div> tag."
(interactive)
(insert "<div class=\"filler\">↓</div>")
)



(defun insert-date-tag ()
  "Insert a date tag, e.g. <pre class=\"date\">2006-10-09</pre>."
  (interactive)
  (insert (concat "<div class=\"date-α\"><time>" (format-time-string "%Y-%m-%d") "</time></div>\n\n\n\n" ))
  (backward-char 2)
)



(defun insert-midi ()
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


