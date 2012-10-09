(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(setenv "ERGOEMACS_KEYBOARD_LAYOUT" "dv") ; US Dvorak (Ergonomic)

(load "~/git/ergoemacs_src/ergoemacs/ergoemacs/init")

(when
 (file-exists-p "~/git/xah_emacs_init/xah_emacs_init.el")
 (load "~/git/xah_emacs_init/xah_emacs_init.el")
)
