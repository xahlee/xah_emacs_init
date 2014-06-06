;; -*- coding: utf-8 -*-
;; 2007-06, 2012-09-24
;; http://ergoemacs.org/emacs/xah_emacs_init.html


; loading custom commands and functions

(defun xah-fullpath-relative-to-caller (φfile-relative-path)
  "Return the full path of ΦFILE-RELATIVE-PATH, relative to caller's file location.

Example: If you have this line
 (xah-fullpath-relative-to-caller \"../xyz.el\")
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



(setenv "LANG" "en_US.UTF-8" )
(setenv "LC_ALL" "en_US.UTF-8" )
(setenv "ERGOEMACS_KEYBOARD_LAYOUT" "dv") ; US Dvorak (Ergonomic)



; add the dir of this file to load path
(add-to-list 'load-path (xah-fullpath-relative-to-caller ""))

;; (require 'package)
;; (package-initialize)

(when (string-equal system-type "windows-nt") (load (xah-fullpath-relative-to-caller "xah_emacs_ms_windows")))

(if (boundp 'xah-load-xahkeys-q)
    (when xah-load-xahkeys-q
      ;; (load (xah-fullpath-relative-to-caller "xah_emacs_keybinding_unset_keys.el"))
      (load (xah-fullpath-relative-to-caller "xah_emacs_keybinding_ergoemacs_xah"))
      (load (xah-fullpath-relative-to-caller "xah_emacs_keybinding_ergoemacs_raw"))
      (load (xah-fullpath-relative-to-caller "xah_emacs_keybinding_special_keys"))
      (load (xah-fullpath-relative-to-caller "xah_emacs_keybinding"))
      (load (xah-fullpath-relative-to-caller "xah_emacs_keybinding_control_key"))
      (load (xah-fullpath-relative-to-caller "xah_emacs_keybinding_mode_specific"))
      (load (xah-fullpath-relative-to-caller "xah_emacs_unicode_input"))
      ;; (load (xah-fullpath-relative-to-caller "xah_emacs_keybinding_shift_switch"))
      ;; (load (xah-fullpath-relative-to-caller "xah_emacs_keybinding_number_pad"))
      (load (xah-fullpath-relative-to-caller "xah_emacs_keybinding_number_pad_number"))
      (load (xah-fullpath-relative-to-caller "xah_emacs_mouse_binding"))
      (load (xah-fullpath-relative-to-caller "xah_emacs_hyper_super_setup"))
      ;; (load (xah-fullpath-relative-to-caller "xah_emacs_keybinding_truly_ergonomic"))
      )
  (progn
    nil
    )
  )

(load (xah-fullpath-relative-to-caller "xah_emacs_keybinding_functions"))
(load (xah-fullpath-relative-to-caller "xah_emacs_cursor_movement"))

(load (xah-fullpath-relative-to-caller "xah_emacs_alias"))
(load (xah-fullpath-relative-to-caller "xah_emacs_abbr"))

(load (xah-fullpath-relative-to-caller "xah_elisp_util"))
(load (xah-fullpath-relative-to-caller "xah_emacs_xahsite_path_lisp_util"))
(load (xah-fullpath-relative-to-caller "xah_emacs_insert_brackets"))

(load "~/Dropbox/xah-emacs-private_b53d8d39")

(load (xah-fullpath-relative-to-caller "xah_emacs_editing_commands"))
(load (xah-fullpath-relative-to-caller "xah_emacs_dired_commands"))
(load (xah-fullpath-relative-to-caller "xah_emacs_generic"))

(load (xah-fullpath-relative-to-caller "xah_emacs_html"))
(load (xah-fullpath-relative-to-caller "xah_emacs_html_insert_things"))
(load (xah-fullpath-relative-to-caller "xah_emacs_html_syntax_color_util"))
(load (xah-fullpath-relative-to-caller "xah_emacs_linkify"))
(load (xah-fullpath-relative-to-caller "xah_emacs_ref_linkify"))
(load (xah-fullpath-relative-to-caller "xah_emacs_vid_linkify"))
(load (xah-fullpath-relative-to-caller "xah_emacs_google_earth"))

(load (xah-fullpath-relative-to-caller "xah_emacs_str_insertion"))

(load (xah-fullpath-relative-to-caller "xah_emacs_str_replacement"))
(require 'xah-misc-commands)

(load (xah-fullpath-relative-to-caller "xah_emacs_str_rep_func"))

(load (xah-fullpath-relative-to-caller "xah_emacs_font"))
(load (xah-fullpath-relative-to-caller "xah_emacs_misc"))
(load (xah-fullpath-relative-to-caller "xah_emacs_browse_url"))

(load (xah-fullpath-relative-to-caller "xah_emacs_atom_rss_util"))
(load (xah-fullpath-relative-to-caller "xah_emacs_blogger_util"))
(load (xah-fullpath-relative-to-caller "xah_emacs_xahsite_fix_util"))

(load (xah-fullpath-relative-to-caller "xah_emacs_wordyenglish"))

(load (xah-fullpath-relative-to-caller "xah_emacs_settings"))
(load (xah-fullpath-relative-to-caller "xah_emacs_file_association"))

(load (xah-fullpath-relative-to-caller "xah_emacs_load_misc"))

(when (string-equal system-type "darwin")
  (load (xah-fullpath-relative-to-caller "xah_emacs_mac_os_x"))
  )

(load (xah-fullpath-relative-to-caller "xah_emacs_replace_quotes"))

;; (server-force-delete)
;; ;; (server-start "LEAVE-DEAD" "INHIBIT-PROMPT")
;; (server-start )

(load (xah-fullpath-relative-to-caller "xah_emacs_syntax_parse"))
(load (xah-fullpath-relative-to-caller "xah_emacs_overlay_and_char_properties"))
