;; -*- coding: utf-8 -*-
;; 2007-06, 2012-09-24
;; http://ergoemacs.org/emacs/xah_emacs_init.html


; loading custom commands and functions

(defun fullpath-relative-to-current-file (file-relative-path)
  "Returns the full path of FILE-RELATIVE-PATH, relative to file location where this function is called.

Example: If you have this line
 (fullpath-relative-to-current-file \"../xyz.el\")
in the file at
 /home/mary/emacs/emacs_lib.el
then the return value is
 /home/mary/xyz.el
Regardless how or where emacs_lib.el is called.

This function solves 2 problems.

 ① If you have file A, that calls the `load' on a file at B, and
B calls “load” on file C using a relative path, then Emacs will
complain about unable to find C. Because, emacs does not switch
current directory with “load”.

 To solve this problem, when your code only knows the relative
path of another file C, you can use the variable `load-file-name'
to get the current file's full path, then use that with the
relative path to get a full path of the file you are interested.

 ② To know the current file's full path, emacs has 2 ways:
`load-file-name' and `buffer-file-name'.  If the file is loaded
by “load”, then load-file-name works but buffer-file-name
doesn't.  If the file is called by `eval-buffer', then
load-file-name is nil. You want to be able to get the current
file's full path regardless the file is run by “load” or
interactively by “eval-buffer”."
  (concat (file-name-directory (or load-file-name buffer-file-name)) file-relative-path)
)



(setenv "LANG" "en_US.UTF-8" )
(setenv "LC_ALL" "en_US.UTF-8" )
(setenv "ERGOEMACS_KEYBOARD_LAYOUT" "dv") ; US Dvorak (Ergonomic)

;; 2012-05-07
(when (string-equal system-type "windows-nt")
  (let (
        (myPathList
         [
           "C:/Python27"
           "C:/Users/h3/apps/php-5.4.6-nts-Win32-VC9-x86/"
           ;; "C:/Python32"
           "C:/strawberry/c/bin"
           "C:/strawberry/perl/site/bin"
           "C:/strawberry/perl/bin"

           "C:/Users/h3/AppData/Roaming/npm"
           "C:/Program Files (x86)/nodejs/"

           "C:/cygwin/usr/local/bin"
           "C:/cygwin/usr/bin"
           "C:/cygwin/bin"

           "C:/Program Files (x86)/ErgoEmacs/msys/bin"
           "C:/Program Files (x86)/Mozilla Firefox/"
           "C:/Program Files (x86)/Opera"
           "C:/Program Files (x86)/Safari"
           "C:/Users/h3/AppData/Local/Google/Chrome/Application"
           ] )
        )

    (setenv "PATH" (mapconcat 'identity myPathList ";") )

    (setq exec-path (append myPathList (list "." exec-directory)) )
    ) )



; add the dir of this file to load path
(add-to-list 'load-path (fullpath-relative-to-current-file ""))

(package-initialize)

(load (fullpath-relative-to-current-file "xah_emacs_keybinding"))
(load (fullpath-relative-to-current-file "xah_emacs_mouse_binding"))
(load (fullpath-relative-to-current-file "xah_emacs_hyper_super_setup"))
(load (fullpath-relative-to-current-file "xah_emacs_unicode_input"))
(load (fullpath-relative-to-current-file "xah_emacs_alias"))
(load (fullpath-relative-to-current-file "xah_emacs_abbr"))

(load (fullpath-relative-to-current-file "xah_elisp_util"))
(load (fullpath-relative-to-current-file "xah_emacs_xahsite_path_lisp_util"))
(load (fullpath-relative-to-current-file "xah_emacs_insert_pairs"))

(load (fullpath-relative-to-current-file "xah_emacs_cursor_movement"))
(load (fullpath-relative-to-current-file "xah_emacs_editing_commands"))
(load (fullpath-relative-to-current-file "xah_emacs_generic"))

(load (fullpath-relative-to-current-file "xah_emacs_html"))
(load (fullpath-relative-to-current-file "xah_emacs_html_table"))
(load (fullpath-relative-to-current-file "xah_emacs_html_insert_things"))
(load (fullpath-relative-to-current-file "xah_emacs_html_syntax_color_util"))
(load (fullpath-relative-to-current-file "xah_emacs_linkify"))
(load (fullpath-relative-to-current-file "xah_emacs_ref_linkify"))
(load (fullpath-relative-to-current-file "xah_emacs_vid_linkify"))
(load (fullpath-relative-to-current-file "xah_emacs_google_earth"))

(load (fullpath-relative-to-current-file "xah_emacs_str_insertion"))
(load (fullpath-relative-to-current-file "xah_emacs_str_replacement"))
(load (fullpath-relative-to-current-file "xah_emacs_str_rep_func"))

(load (fullpath-relative-to-current-file "xah_emacs_font"))
(load (fullpath-relative-to-current-file "xah_emacs_misc"))

(load (fullpath-relative-to-current-file "xah_emacs_atom_rss_util"))
(load (fullpath-relative-to-current-file "xah_emacs_blogger_util"))
(load (fullpath-relative-to-current-file "xah_emacs_xahsite_fix_util"))

(load (fullpath-relative-to-current-file "xah_emacs_wordyenglish"))

(load (fullpath-relative-to-current-file "xah_emacs_settings"))

(load (fullpath-relative-to-current-file "xah_emacs_load_misc"))

(when (string-equal system-type "darwin")
  (load (fullpath-relative-to-current-file "xah_emacs_mac_os_x"))
  )

(load (fullpath-relative-to-current-file "xah_emacs_replace_quotes"))
