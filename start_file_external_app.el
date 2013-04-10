; 2013-04-03
; from Fabrice Popineau
; https://plus.google.com/b/113859563190964307534/113859563190964307534/posts/PQtjojZ3F91


(setq dired-guess-shell-alist-user
      '(("\\.pdf\\'" "start")
        ("\\.zip\\'" "start")
        ("\\.docx?\\'" "start")
        ("\\.xlsx?\\'" "start")
        ("\\.pptx?\\'" "start")
        ("\\.jpe?g\\'" "start")
        ("\\.png\\'" "start")
        ("\\.bmp\\'" "start")
        ("\\.html\\'" "start")
        ))

(defadvice dired-shell-stuff-it (around dired-execute-start-command)
  "Use w32-shell-execute to execute `start' commands."
  (if (string= (ad-get-arg 0) "start")
      (dolist (file (ad-get-arg 1))
        (w32-shell-execute nil (expand-file-name file default-directory)))
    ad-do-it))

(ad-activate 'dired-shell-stuff-it)

;; Dired invokes dired-run-shell-command anyway, even when
;; the command as already been processed by advising
;; dired-shell-stuff-it. In this case the command is null.
;; We want to avoid any error message.

(defadvice dired-run-shell-command (around dired-ignore-null-command)
  "Ignore null command in dired-run-shell-command."
  (when (ad-get-arg 0)
    ad-do-it))

(ad-activate 'dired-run-shell-command)
