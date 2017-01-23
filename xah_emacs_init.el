;; -*- coding: utf-8; lexical-binding: t; -*-
;; 2007-06, 2012-09-24
;; http://ergoemacs.org/emacs/xah_emacs_init.html

(require 'package)
(package-initialize)


(require 'ido)


; loading custom commands and functions

(defun xah-get-fullpath (*file-relative-path)
  "Return the full path of *file-relative-path, relative to caller's file location.

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

  (concat (file-name-directory (or load-file-name buffer-file-name)) *file-relative-path)
)



(load (xah-get-fullpath "xah_emacs_settings"))



; add the dir of this file to load path
(add-to-list 'load-path (xah-get-fullpath ""))


;;; smex.el --- M-x interface with Ido-style fuzzy matching. -*- lexical-binding: t; -*-

;; (load "~/git/xah-get-thing-or-selection/xah-get-thing")
;; (load "~/git/xah-replace-pairs/xah-replace-pairs")
;; (load "~/git/xeu_elisp_util.el/xeu_elisp_util")
;; (load "~/git/xah-fly-keys/xah-fly-keys.el")
;; (load "~/git/xah-elisp-mode/xah-elisp-mode")
;; (load "~/git/xah-clojure-mode/xah-clojure-mode")
;; (load "~/git/xah-find/xah-find")
;; (load "~/git/xah-insert-random-id.el/xah-insert-random-id")

(add-to-list 'load-path "~/git/xah-get-thing-or-selection/")
(require 'xah-get-thing)
(add-to-list 'load-path "~/git/xah-replace-pairs/")
(require 'xah-replace-pairs)
(add-to-list 'load-path "~/git/xeu_elisp_util.el/")
(require 'xeu_elisp_util)
(add-to-list 'load-path "~/git/xah-fly-keys/")
(require 'xah-fly-keys)
(add-to-list 'load-path "~/git/xah-elisp-mode/")
(require 'xah-elisp-mode)
(add-to-list 'load-path "~/git/xah-clojure-mode/")
(require 'xah-clojure-mode)
(add-to-list 'load-path "~/git/xah-find/")
(require 'xah-find)
(add-to-list 'load-path "~/git/xah-insert-random-id.el/")
(require 'xah-insert-random-id)

;; (add-to-list 'load-path "~/git/xah-comment.el/")

;; (add-to-list 'load-path "~/git/xah-html6-mode.el/")

(add-to-list 'load-path "~/git/xah-css-mode/")
(require 'xah-css-mode)
(add-to-list 'load-path "~/git/xah-html-mode.el/")
(require 'xah-html-mode)
(add-to-list 'load-path "~/git/xah-js-mode.el/")
(require 'xah-js-mode)
(add-to-list 'load-path "~/git/xah-php-mode.el/")
(require 'xah-php-mode)
(add-to-list 'load-path "~/git/xbbcode-mode.el/")
(require 'xbbcode-mode)
(add-to-list 'load-path "~/git/xub-mode.el/")
(require 'xub-mode)
(add-to-list 'load-path "~/git/lookup-word-on-internet/")
(require 'xah-lookup)
(add-to-list 'load-path "~/git/xah-math-input/")
(require 'xah-math-input)

(add-to-list 'load-path "~/Dropbox/")
(require 'xlsl-mode)

(load (xah-get-fullpath "xah_emacs_xahsite_path_lisp_util"))

(add-to-list 'load-path "~/git/xahk-mode.el/")
(autoload 'xahk-mode "xahk-mode" "Load xahk-mode for editing AutoHotkey scripts." t)

(when (string-equal system-type "windows-nt") (load (xah-get-fullpath "xah_emacs_ms_windows")))

(setq xah-load-xahkeys-q t)

(if (boundp 'xah-load-xahkeys-q)
    (when xah-load-xahkeys-q
      (setq xah-fly-swapped-1-8-and-2-7-p t)
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

(load (xah-get-fullpath "xah_emacs_abbr"))

(load (xah-get-fullpath "xah_emacs_generic"))
(load (xah-get-fullpath "xah_emacs_mouse_commands"))

(load (xah-get-fullpath "xah_emacs_html"))
(load (xah-get-fullpath "xah_emacs_html_insert_things"))
(load (xah-get-fullpath "xah_emacs_html_linkify"))
(load (xah-get-fullpath "xah_emacs_ref_linkify"))
(load (xah-get-fullpath "xah_emacs_vid_linkify"))
(load (xah-get-fullpath "xah_emacs_google_earth"))

;; (load (xah-get-fullpath "xah_emacs_font"))
(load (xah-get-fullpath "xah_emacs_misc"))

(load (xah-get-fullpath "xah_emacs_atom_rss_util"))
(load (xah-get-fullpath "xah_emacs_blogger_util"))
(load (xah-get-fullpath "xah_emacs_xahsite_fix_util"))

(load (xah-get-fullpath "xah_emacs_wordyenglish"))

(load (xah-get-fullpath "xah_emacs_settings_font"))

(load (xah-get-fullpath "xah_emacs_settings_external_packages"))
(load (xah-get-fullpath "xah_emacs_file_association"))

(load (xah-get-fullpath "xah_emacs_replace_quotes"))

;; (server-force-delete)
;; ;; (server-start "LEAVE-DEAD" "INHIBIT-PROMPT")
;; (server-start )

(load (xah-get-fullpath "xah_emacs_syntax_parse"))
(load (xah-get-fullpath "xah_emacs_overlay_and_char_properties"))
(load "~/Dropbox/xah-emacs-private_b53d8d39")
