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
  )

(add-to-list 'load-path "~/git/xah-find/")
(require 'xah-find)

(add-to-list 'load-path "~/git/xah-insert-random-id.el/")
(require 'xah-insert-random-id)

;; (add-to-list 'load-path "~/git/xah-comment.el/")

;; (add-to-list 'load-path "~/git/xah-html6-mode.el/")

(progn
  (add-to-list 'load-path "~/git/xah-css-mode/")
  (require 'xah-css-mode)
  )

(require 'htmlize)

(progn
  (add-to-list 'load-path "~/git/xah-html-mode.el/")
  (require 'xah-html-mode))

(progn
  (add-to-list 'load-path "~/git/xah-js-mode.el/")
  ;; (require 'xah-js-mode)
  (autoload 'xah-js-mode "xah-js-mode" ))

(progn
  (add-to-list 'load-path "~/git/xah-typescript-mode/")
  (require 'xah-ts-mode nil t)
  (autoload 'xah-ts-mode "xah-ts-mode" ))

(progn
  (add-to-list 'load-path "~/git/xah-clojure-mode/")
  (require 'xah-clojure-mode)
  (autoload 'xah-clojure-mode "xah-clojure-mode" "autoload the mode." t)
  )

(progn
  (add-to-list 'load-path "~/git/xah-php-mode.el/")
  ;; (require 'xah-php-mode)
  (autoload 'xah-php-mode "xah-php-mode" "autoload the mode." t))

(progn
  (add-to-list 'load-path "~/git/xbbcode-mode.el/")
  ;; (require 'xbbcode-mode)
  (autoload 'xbbcode-mode "xbbcode-mode" "autoload the mode." t))

(progn
  (add-to-list 'load-path "~/git/xub-mode.el/")
  ;; (require 'xub-mode)
  (autoload 'xub-mode "xub-mode" "autoload the mode." t))

(progn
  (add-to-list 'load-path "~/git/lookup-word-on-internet/")
  ;; (setq xah-lookup-browser-function 'eww )
  (require 'xah-lookup)
;; (put 'xah-lookup-word-definition 'xah-lookup-browser-function 'eww)
;; (put 'xah-lookup-wikipedia 'xah-lookup-browser-function 'eww)

)

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

(progn
  (add-to-list 'load-path "~/git/xahk-mode.el/")
  (autoload 'xahk-mode "xahk-mode" "Load xahk-mode for editing AutoHotkey scripts." t))

(when (string-equal system-type "windows-nt")
  (load (xah-get-fullpath "xah_emacs_ms_windows")))

(when (fboundp 'xah-fly-keys)
    (setq xah-fly-swapped-1-8-and-2-7-p t)
    ;; (xah-fly-keys-set-layout "qwerty") ; required if you use qwerty
    (xah-fly-keys 1)
    (load (xah-get-fullpath "xah_emacs_keybinding"))
    ;; (load (xah-get-fullpath "xah_emacs_keybinding_number_pad"))
    ;; (load (xah-get-fullpath "xah_emacs_keybinding_number_pad_number"))
    (load (xah-get-fullpath "xah_emacs_mouse_setup")))

(load (xah-get-fullpath "xah_emacs_dired_commands"))

(load (xah-get-fullpath "xah_emacs_abbr"))

(load (xah-get-fullpath "xah_emacs_mouse_commands"))

(load (xah-get-fullpath "xah_emacs_html"))
(load (xah-get-fullpath "xah_emacs_html_insert_things"))
(load (xah-get-fullpath "xah_emacs_html_linkify"))
(load (xah-get-fullpath "xah_emacs_ref_linkify"))
(load (xah-get-fullpath "xah_emacs_google_earth"))

(load (xah-get-fullpath "xah_emacs_font"))
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

;; (setq describe-char-unicodedata-file "~/.emacs.d/UnicodeData.txt")
