;; -*- coding: utf-8; lexical-binding: t; -*-
;; 2007-06
;; http://ergoemacs.org/emacs/xah_emacs_init.html

(when (< emacs-major-version 27) (package-initialize))

(require 'ido)

;; HHH___________________________________________________________________
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

;; HHH___________________________________________________________________

(load (xah-get-fullpath "xah_emacs_settings"))
(load (xah-get-fullpath "xah_emacs_abbr"))

;; HHH___________________________________________________________________

; add the dir of this file to load path
(add-to-list 'load-path (xah-get-fullpath ""))

(add-to-list 'load-path "~/git/xah-get-thing-or-selection/")
(require 'xah-get-thing)

(add-to-list 'load-path "~/git/xah-replace-pairs/")
(require 'xah-replace-pairs)

(add-to-list 'load-path "~/git/xeu_elisp_util.el/")
(require 'xeu_elisp_util)

(progn
  (add-to-list 'load-path "~/git/xah-fly-keys/")
  (require 'xah-fly-keys)
  (xah-fly-keys-set-layout "dvorak")
  (xah-fly-keys 1)
  (setq xah-fly-M-x-command 'smex)
  (setq
   xah-run-current-file-map
   '(
     ("php" . "php")
     ("pl" . "d:/Strawberry/perl/bin/perl.exe")
     ("py" . "python")
     ("py2" . "python")
     ("py3" . "C:/Python39/python.exe")
     ("rb" . "ruby")
     ("go" . "go run")
     ("hs" . "runhaskell")
     ("js" . "deno run")
     ("ts" . "deno run") ; TypeScript
     ("tsx" . "tsc")
     ("mjs" . "node --experimental-modules ")
     ("sh" . "bash")
     ("clj" . "clj")
     ("rkt" . "racket")
     ("ml" . "ocaml")
     ("vbs" . "cscript")
     ("tex" . "pdflatex")
     ("latex" . "pdflatex")
     ("java" . "javac")
     ;; ("pov" . "/usr/local/bin/povray +R2 +A0.1 +J1.2 +Am2 +Q9 +H480 +W640")
     )))

(progn
  (require 'xah-text-mode)
  )

(progn
  (add-to-list 'load-path "~/git/xah-elisp-mode/")
  (require 'xah-elisp-mode)
  )

(add-to-list 'load-path "~/git/xah-insert-random-id.el/")
(require 'xah-insert-random-id)

;; (add-to-list 'load-path "~/git/xah-comment.el/")

;; (add-to-list 'load-path "~/git/xah-html6-mode.el/")

(progn
  (add-to-list 'load-path "~/git/xah-css-mode/")
  (require 'xah-css-mode))

(progn
  (load "~/git/xah_emacs_init/htmlize")
  (when (fboundp 'htmlize-region)
    (setq htmlize-convert-nonascii-to-entities nil)
    (setq htmlize-html-charset "utf-8")
    (setq htmlize-untabify nil)
    (setq htmlize-generate-hyperlinks nil)))

(progn
  (load "~/git/xah_emacs_init/smex"))

;; (progn
;;   (require 'command-log-mode)
;;   )

(progn
  (add-to-list 'load-path "~/git/xah-html-mode.el/")
  (require 'xah-html-mode)
  (setq initial-major-mode 'xah-html-mode))

(progn
  (add-to-list 'load-path "~/git/xah-js-mode.el/")
  (require 'xah-js-mode))

(progn
  (add-to-list 'load-path "~/git/xah-find/")
  (require 'xah-find))

(progn
  (add-to-list 'load-path "~/git/xah-dired/")
  (require 'xah-dired)
  )

(progn
  (add-to-list 'load-path "~/git/xah-clojure-mode/")
  ;; (require 'xah-clojure-mode)
  (autoload 'xah-clojure-mode "xah-clojure-mode" "autoload the mode." t))

(progn
  (add-to-list 'load-path "~/git/xah-php-mode.el/")
  ;; (require 'xah-php-mode)
  (autoload 'xah-php-mode "xah-php-mode" "autoload the mode." t))

(progn
  (add-to-list 'load-path "~/git/xbbcode-mode.el/")
  ;; (require 'xbbcode-mode)
  (autoload 'xbbcode-mode "xbbcode-mode" "autoload the mode." t))

(progn
  (add-to-list 'load-path "~/git/lookup-word-on-internet/")
  ;; (setq xah-lookup-browser-function 'eww )
  (require 'xah-lookup)
  ;; (put 'xah-lookup-word-definition 'xah-lookup-browser-function 'eww)
  (put 'xah-lookup-word-definition 'xah-lookup-url "http://www.thefreedictionary.com/word02051")

  ;; (put 'xah-lookup-word-definition 'xah-lookup-url "https://www.ahdictionary.com/word/search.html?q=word02051")

  ;; (put 'xah-lookup-wikipedia 'xah-lookup-browser-function 'eww)

  )

(progn
  (add-to-list 'load-path "~/git/xah-math-input/")
  (require 'xah-math-input)
  (global-xah-math-input-mode 1)
  )

(progn
  (add-to-list 'load-path "~/git/xub-mode.el/")
  ;; (require 'xub-mode)
  (autoload 'xub-mode "xub-mode" "autoload the mode." t))

(when (file-exists-p "~/no_Dropbox/")
  (progn
    (add-to-list 'load-path "~/no_Dropbox/")
    ;; (require 'xlsl-mode)
    (autoload 'xlsl-mode "xlsl-mode" "autoload the mode." t)))

(load (xah-get-fullpath "xah_emacs_xahsite_path_lisp_util"))
;; (load (xah-get-fullpath "xah_gen_sitemap"))

(load (xah-get-fullpath "xah_emacs_keybinding"))
(load (xah-get-fullpath "xah_emacs_mouse_setup"))

(progn
;; AutoHotkey
  (add-to-list 'load-path "~/git/xahk-mode.el/")
  (require 'xahk-mode)
  ;; (autoload 'xahk-mode "xahk-mode" "Load xahk-mode for editing AutoHotkey scripts." t)
  )

;; (when (string-equal system-type "windows-nt") (load (xah-get-fullpath "xah_emacs_ms_windows")))

(load (xah-get-fullpath "xah_emacs_kmacro"))

(load (xah-get-fullpath "xah_emacs_mouse_commands"))

(load (xah-get-fullpath "xah_emacs_html"))
(load (xah-get-fullpath "xah_emacs_html_linkify"))
(load (xah-get-fullpath "xah_emacs_ref_linkify"))
(load (xah-get-fullpath "xah_emacs_google_earth"))

(load (xah-get-fullpath "xah_emacs_misc"))

(load (xah-get-fullpath "xah_emacs_atom_rss_util"))

(load (xah-get-fullpath "xah_emacs_xahsite_fix_util"))

(load (xah-get-fullpath "xah_emacs_wordyenglish"))

(load (xah-get-fullpath "xah_emacs_settings_external_packages"))
(load (xah-get-fullpath "xah_emacs_file_association"))

(load (xah-get-fullpath "xah_emacs_replace_quotes"))

(load (xah-get-fullpath "xah_gen_sitemap"))

(load "~/xdoc/xah_emacs_private_HYfgS.el" "NOERROR" "NOMESSAGE" "NOSUFFIX" "MUST-SUFFIX")

;; (when (file-exists-p "~/.emacs.d/UnicodeData.txt") (setq describe-char-unicodedata-file "~/.emacs.d/UnicodeData.txt"))

(progn
  (when (fboundp 'xah-find-text)
    (defvar xahsite-external-docs nil "A vector of dir paths. Version 2019-06-11")
    (setq  xahsite-external-docs
           [
            "ergoemacs_org/emacs_manual/"
            "xahlee_info/clojure-doc-1.8/"
            "xahlee_info/css_2.1_spec/"
            "xahlee_info/css_transitions/"
            "xahlee_info/js_es2011/"
            "xahlee_info/js_es2015/"
            "xahlee_info/js_es2015_orig/"
            "xahlee_info/js_es2016/"
            "xahlee_info/js_es2018/"
            "xahlee_info/node_api/"
            ])
    (setq xah-find-dir-ignore-regex-list (append xahsite-external-docs [ "\\.git/" "xahlee_info/js/ex/" ] nil))))
