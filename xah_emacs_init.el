;; -*- coding: utf-8; lexical-binding: t; -*-
;; 2007-06, 2012-09-24
;; http://ergoemacs.org/emacs/xah_emacs_init.html

(require 'package)
(package-initialize)

(require 'ido)


; loading custom commands and functions

(defun xah-get-fullpath (@file-relative-path)
  "Return the full path of @file-relative-path, relative to caller's file location.

Example: If you have this line
 (xah-get-fullpath \"../xyz.el\")
in the file at
 /home/mary/emacs/emacs_lib.el
then the return value is
 /home/mary/xyz.el
Regardless how or where emacs_lib.el is called.

This function solves 2 problems.

① If you have file A, that calls the `load' on a file at B, and B calls `load' on file C using a relative path, then Emacs will complain about unable to find C. Because, emacs does not switch current directory with `load'.

To solve this problem, when your code only knows the relative path of another file C, you can use the variable `load-file-name' to get the current file's full path, then use that with the relative path to get a full path of the file you are interested.

② To know the current file's full path, emacs has 2 ways: `load-file-name' and `buffer-file-name'. If the file is loaded by `load', then `load-file-name' works but `buffer-file-name' doesn't. If the file is called by `eval-buffer', then `load-file-name' is nil. You want to be able to get the current file's full path regardless the file is run by `load' or interactively by `eval-buffer'.

Version 2017-07-19"

  (concat (file-name-directory (or load-file-name buffer-file-name)) @file-relative-path)
)



(load (xah-get-fullpath "xah_emacs_settings"))



; add the dir of this file to load path
(add-to-list 'load-path (xah-get-fullpath ""))

(add-to-list 'load-path "~/git/xah-get-thing-or-selection/")
(require 'xah-get-thing)

(add-to-list 'load-path "~/git/xah-replace-pairs/")
(require 'xah-replace-pairs)

(add-to-list 'load-path "~/git/xeu_elisp_util.el/")
(require 'xeu_elisp_util)

(add-to-list 'load-path "~/git/xah-fly-keys/")
(require 'xah-fly-keys)
(add-hook 'xah-fly-command-mode-activate-hook 'xah-fly-save-buffer-if-file)


(progn
  (require 'xah-text-mode)
  (setq initial-major-mode 'xah-text-mode))

(progn
  (add-to-list 'load-path "~/git/xah-elisp-mode/")
  (require 'xah-elisp-mode)
  (when (boundp 'xah-elisp-mode-map)
    (define-key xah-elisp-mode-map
      (kbd "<delete>")
      xah-elisp-mode-no-chord-map)))

(add-to-list 'load-path "~/git/xah-find/")
(require 'xah-find)

(add-to-list 'load-path "~/git/xah-insert-random-id.el/")
(require 'xah-insert-random-id)

;; (add-to-list 'load-path "~/git/xah-comment.el/")

;; (add-to-list 'load-path "~/git/xah-html6-mode.el/")

(progn
  (add-to-list 'load-path "~/git/xah-css-mode/")
  (require 'xah-css-mode)
  (when (boundp 'xah-css-mode-no-chord-map)
    (define-key xah-css-mode-no-chord-map (kbd "s") 'xah-sync-css)
    (define-key xah-css-mode-map (kbd "<delete>") xah-css-mode-no-chord-map)))

(progn
  (add-to-list 'load-path "~/git/xah-html-mode.el/")
  (require 'xah-html-mode)
  (when (fboundp 'xah-html-mode)

    (define-key xah-html-mode-map (kbd "<delete>") xah-html-mode-no-chord-map)

    (defun xah-html-mode-keys ()
      "Modify keymaps used by `html-mode'."

      (define-key xah-html-mode-map (kbd "<f5>") 'xah-browse-url-of-buffer)

      (define-key xah-html-mode-no-chord-map (kbd "e") 'xah-make-atom-entry)
      (define-key xah-html-mode-no-chord-map (kbd "u") 'xahsite-update-article-timestamp)
      (define-key xah-html-mode-no-chord-map (kbd "n") 'xah-insert-reference-span-tag)
      (define-key xah-html-mode-no-chord-map (kbd "i") 'xah-html-insert-date-tag)

      (define-key xah-html-mode-no-chord-map (kbd "<delete>") 'xah-browse-url-of-buffer)

      (define-key xah-html-mode-no-chord-map (kbd "SPC") nil)
      (define-key xah-html-mode-no-chord-map (kbd "SPC b") 'xah-make-blogger-entry)
      (define-key xah-html-mode-no-chord-map (kbd "SPC c") 'xah-angle-brackets-to-html)
      (define-key xah-html-mode-no-chord-map (kbd "SPC e") 'xah-html-full-size-img-linkify)
      (define-key xah-html-mode-no-chord-map (kbd "SPC p") 'xah-copy-url-current-file)
      (define-key xah-html-mode-no-chord-map (kbd "SPC r d") 'xah-html-perldoc-ref-linkify)
      (define-key xah-html-mode-no-chord-map (kbd "SPC r e") 'xah-html-emacs-ref-linkify)
      (define-key xah-html-mode-no-chord-map (kbd "SPC r g") 'xah-clojure-word-ref-linkify)
      (define-key xah-html-mode-no-chord-map (kbd "SPC r j") 'xah-html-image-figure-linkify)
      
      (define-key xah-html-mode-no-chord-map (kbd "SPC r r") 'xah-add-to-related-links)
      

      (define-key xah-html-mode-no-chord-map (kbd "j") nil)

      (define-key xah-html-mode-no-chord-map (kbd "j a") 'xah-words-annotate)
      (define-key xah-html-mode-no-chord-map (kbd "j e") 'xah-words-bold-word)
      (define-key xah-html-mode-no-chord-map (kbd "j c") 'xah-words-chinese-linkify)
      (define-key xah-html-mode-no-chord-map (kbd "j m") 'xah-words-move-word-to-page)
      (define-key xah-html-mode-no-chord-map (kbd "j t") 'xah-words-word-etymology-linkify)

      (define-key xah-html-mode-no-chord-map (kbd "j n") 'xah-words-new-word-entry )
      (define-key xah-html-mode-no-chord-map (kbd "j i") 'xah-words-insert-word-entry )
      (define-key xah-html-mode-no-chord-map (kbd "j d") 'xah-words-add-definition )
      (define-key xah-html-mode-no-chord-map (kbd "j s") 'xah-words-add-source )
      (define-key xah-html-mode-no-chord-map (kbd "j c") 'xah-words-add-comment )
      (define-key xah-html-mode-no-chord-map (kbd "j g") 'xah-words-search-next-unbold )
      (define-key xah-html-mode-no-chord-map (kbd "j p") 'xah-words-query-find-then-bold )

      (define-key xah-html-mode-no-chord-map (kbd "SPC z b") 'xah-html-insert-lyrics-header)
      (define-key xah-html-mode-no-chord-map (kbd "SPC z f") 'xah-html-insert-midi))

    (add-hook 'xah-html-mode-hook 'xah-html-mode-keys)))

(add-to-list 'load-path "~/git/xah-js-mode.el/")
;; (require 'xah-js-mode)
(autoload 'xah-js-mode "xah-js-mode" )

(add-to-list 'load-path "~/git/xah-typescript-mode/")
(require 'xah-ts-mode nil t)
(autoload 'xah-ts-mode "xah-ts-mode" )

(progn
  (add-to-list 'load-path "~/git/xah-clojure-mode/")
  (require 'xah-clojure-mode)
  (autoload 'xah-clojure-mode "xah-clojure-mode" "autoload the mode." t)
  (when (boundp 'xah-clojure-mode-map)
    (define-key xah-clojure-mode-map (kbd "<delete>") xah-clojure-mode-no-chord-map)))

(add-to-list 'load-path "~/git/xah-php-mode.el/")
;; (require 'xah-php-mode)
(autoload 'xah-php-mode "xah-php-mode" "autoload the mode." t)

(add-to-list 'load-path "~/git/xbbcode-mode.el/")
;; (require 'xbbcode-mode)
(autoload 'xbbcode-mode "xbbcode-mode" "autoload the mode." t)

(add-to-list 'load-path "~/git/xub-mode.el/")
;; (require 'xub-mode)
(autoload 'xub-mode "xub-mode" "autoload the mode." t)

(add-to-list 'load-path "~/git/lookup-word-on-internet/")
(require 'xah-lookup)

(progn
  (add-to-list 'load-path "~/git/xah-math-input/")
  (require 'xah-math-input)
  (global-xah-math-input-mode 1)
  )

(when (file-exists-p "~/Dropbox/")
  (progn
    (add-to-list 'load-path "~/Dropbox/")
    ;; (require 'xlsl-mode)
    (autoload 'xlsl-mode "xlsl-mode" "autoload the mode." t)))

(load (xah-get-fullpath "xah_emacs_xahsite_path_lisp_util"))

(add-to-list 'load-path "~/git/xahk-mode.el/")
(autoload 'xahk-mode "xahk-mode" "Load xahk-mode for editing AutoHotkey scripts." t)

(when (string-equal system-type "windows-nt") (load (xah-get-fullpath "xah_emacs_ms_windows")))

(when (fboundp 'xah-fly-keys)
    (setq xah-fly-swapped-1-8-and-2-7-p t)
    ;; (xah-fly-keys-set-layout "qwerty") ; required if you use qwerty
    (xah-fly-keys 1)
    (load (xah-get-fullpath "xah_emacs_keybinding"))
    (load (xah-get-fullpath "xah_emacs_keybinding_mode_specific"))
    ;; (load (xah-get-fullpath "xah_emacs_keybinding_number_pad"))
    ;; (load (xah-get-fullpath "xah_emacs_keybinding_number_pad_number"))
    (load (xah-get-fullpath "xah_emacs_mouse_binding")))

(load (xah-get-fullpath "xah_emacs_dired_commands"))

(load (xah-get-fullpath "xah_emacs_abbr"))

(load (xah-get-fullpath "xah_emacs_mouse_commands"))

(load (xah-get-fullpath "xah_emacs_html"))
(load (xah-get-fullpath "xah_emacs_html_insert_things"))
(load (xah-get-fullpath "xah_emacs_html_linkify"))
(load (xah-get-fullpath "xah_emacs_ref_linkify"))
(load (xah-get-fullpath "xah_emacs_google_earth"))

;; (load (xah-get-fullpath "xah_emacs_font"))
(load (xah-get-fullpath "xah_emacs_misc"))

(load (xah-get-fullpath "xah_emacs_atom_rss_util"))
(load (xah-get-fullpath "xah_emacs_blogger_util"))
(load (xah-get-fullpath "xah_emacs_xahsite_fix_util"))

(load (xah-get-fullpath "xah_emacs_wordyenglish"))



(load (xah-get-fullpath "xah_emacs_settings_external_packages"))
(load (xah-get-fullpath "xah_emacs_file_association"))

(load (xah-get-fullpath "xah_emacs_replace_quotes"))

;; (server-force-delete)
;; ;; (server-start "LEAVE-DEAD" "INHIBIT-PROMPT")
;; (server-start )

(when (file-exists-p "~/Dropbox/")
  (progn
    (load "~/Dropbox/xah-emacs-private_b53d8d39")))

(setq describe-char-unicodedata-file "~/.emacs.d/UnicodeData.txt")
