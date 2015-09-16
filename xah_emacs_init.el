;; -*- coding: utf-8 -*-
;; 2007-06, 2012-09-24
;; http://ergoemacs.org/emacs/xah_emacs_init.html

(require 'package)
(package-initialize)


; loading custom commands and functions

(defun xah-get-fullpath (φfile-relative-path)
  "Return the full path of φfile-relative-path, relative to caller's file location.

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

② To know the current file's full path, emacs has 2 ways: `load-file-name' and `buffer-file-name'. If the file is loaded by `load', then `load-file-name' works but `buffer-file-name' doesn't. If the file is called by `eval-buffer', then `load-file-name' is nil. You want to be able to get the current file's full path regardless the file is run by `load' or interactively by `eval-buffer'."

  (concat (file-name-directory (or load-file-name buffer-file-name)) φfile-relative-path)
)



(load (xah-get-fullpath "xah_emacs_settings"))



; add the dir of this file to load path
(add-to-list 'load-path (xah-get-fullpath ""))

(add-to-list 'load-path "~/git/xah-fly-keys/")
(add-to-list 'load-path "~/git/xah-elisp-mode/")
(add-to-list 'load-path "~/git/xah-clojure-mode/")
(add-to-list 'load-path "~/git/xah-comment.el/")
(add-to-list 'load-path "~/git/xah-css-mode/")
(add-to-list 'load-path "~/git/xah-find/")
(add-to-list 'load-path "~/git/xah-html6-mode.el/")
(add-to-list 'load-path "~/git/xah-html-mode.el/")
(add-to-list 'load-path "~/git/xah-insert-random-id.el/")
(add-to-list 'load-path "~/git/xah-js-mode.el/")
(add-to-list 'load-path "~/git/xah-misc-commands.el/")
(add-to-list 'load-path "~/git/xah-php-mode.el/")
(add-to-list 'load-path "~/git/xbbcode-mode.el/")
(add-to-list 'load-path "~/git/xeu_elisp_util.el/")
(add-to-list 'load-path "~/git/xah-replace-pairs/")
(add-to-list 'load-path "~/git/xub-mode.el/")
(add-to-list 'load-path "~/git/lookup-word-on-internet/")
(add-to-list 'load-path "~/git/xahk-mode.el/")
(add-to-list 'load-path "~/git/xah-math-input/")

(add-to-list 'load-path "~/git/xah-get-thing-or-selection/")

(autoload 'xahk-mode "xahk-mode" "Load xahk-mode for editing AutoHotkey scripts." t)

(load "xah-get-thing")
(load "xah-find")
(load "xah-html-mode")
(load "xah-comment")
(load "xah-elisp-mode")
(load "xah-clojure-mode")
(load "xah-js-mode")
(load "xah-php-mode")
(load "xah-css-mode")
(load "xah-insert-random-id")
(load "xah-lookup")
(load "xah-replace-pairs")
(load "xeu_elisp_util")
(load "xah-misc-commands")
(load "xbbcode-mode")
(load "xah-math-input")

(defalias 'bbcode-mode 'xbbcode-mode)

(when (string-equal system-type "windows-nt") (load (xah-get-fullpath "xah_emacs_ms_windows")))

(setq xah-load-xahkeys-q t)

(if (boundp 'xah-load-xahkeys-q)
    (when xah-load-xahkeys-q
      (setq xah-fly-swapped-1827-p t)
      (require 'xah-fly-keys)

      (load (xah-get-fullpath "xah_emacs_keybinding"))
      (load (xah-get-fullpath "xah_emacs_keybinding_mode_specific"))
      ;; (load (xah-get-fullpath "xah_emacs_keybinding_number_pad"))
      ;; (load (xah-get-fullpath "xah_emacs_keybinding_number_pad_number"))
      (load (xah-get-fullpath "xah_emacs_mouse_binding"))
      )
  (progn
    nil
    ))

(load (xah-get-fullpath "xah_emacs_keybinding_functions"))
(load (xah-get-fullpath "xah_emacs_dired_commands"))

(load (xah-get-fullpath "xah_emacs_alias"))
(load (xah-get-fullpath "xah_emacs_abbr"))

(load (xah-get-fullpath "xah_emacs_xahsite_path_lisp_util"))

(load (xah-get-fullpath "xah_emacs_generic"))
(load (xah-get-fullpath "xah_emacs_mouse_commands"))

(load (xah-get-fullpath "xah_emacs_html"))
(load (xah-get-fullpath "xah_emacs_html_insert_things"))
(load (xah-get-fullpath "xah_emacs_html_linkify"))
(load (xah-get-fullpath "xah_emacs_ref_linkify"))
(load (xah-get-fullpath "xah_emacs_vid_linkify"))
(load (xah-get-fullpath "xah_emacs_google_earth"))

(load (xah-get-fullpath "xah_emacs_str_rep_func"))

(load (xah-get-fullpath "xah_emacs_font"))
(load (xah-get-fullpath "xah_emacs_misc"))
(load (xah-get-fullpath "xah_emacs_browse_url"))

(load (xah-get-fullpath "xah_emacs_atom_rss_util"))
(load (xah-get-fullpath "xah_emacs_blogger_util"))
(load (xah-get-fullpath "xah_emacs_xahsite_fix_util"))

(load (xah-get-fullpath "xah_emacs_wordyenglish"))

(load (xah-get-fullpath "xah_emacs_settings_font"))

(load (xah-get-fullpath "xah_emacs_settings_external_packages"))
(load (xah-get-fullpath "xah_emacs_file_association"))

(when (string-equal system-type "darwin")
  (load (xah-get-fullpath "xah_emacs_mac_os_x"))
  )

(load (xah-get-fullpath "xah_emacs_replace_quotes"))

;; (server-force-delete)
;; ;; (server-start "LEAVE-DEAD" "INHIBIT-PROMPT")
;; (server-start )

(load (xah-get-fullpath "xah_emacs_syntax_parse"))
(load (xah-get-fullpath "xah_emacs_overlay_and_char_properties"))
(load "~/Dropbox/xah-emacs-private_b53d8d39")
