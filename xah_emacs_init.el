;; -*- coding: utf-8; lexical-binding: t; -*-

; add the dir of this file to load path
(add-to-list 'load-path (xah-get-fullpath ""))

;; HHH___________________________________________________________________
;; xah fly keys

(xah-fly-keys-set-layout "dvorak")
(setq xah-fly-M-x-command 'smex)
(setq xah-run-current-file-map
      '(("php" . "php")
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
        ))

;; HHH___________________________________________________________________

(load (xah-get-fullpath "xah_emacs_abbr"))

(require 'xah-text-mode)

;; (add-to-list 'load-path "~/git/xah-comment.el/")

;; (add-to-list 'load-path "~/git/xah-html6-mode.el/")

(setq initial-major-mode 'xah-html-mode)

(progn
  (add-to-list 'load-path "~/git/xah-clojure-mode/")
  (autoload 'xah-clojure-mode "xah-clojure-mode" "autoload the mode." t))

(progn
  (add-to-list 'load-path "~/git/xah-php-mode.el/")
  (autoload 'xah-php-mode "xah-php-mode" "autoload the mode." t))

(progn
  (add-to-list 'load-path "~/git/xbbcode-mode.el/")
  (autoload 'xbbcode-mode "xbbcode-mode" "autoload the mode." t))

(progn
  (add-to-list 'load-path "~/git/xub-mode.el/")
  (autoload 'xub-mode "xub-mode" "autoload the mode." t))

(when (file-exists-p "~/no_Dropbox/")
  (progn
    (add-to-list 'load-path "~/no_Dropbox/")
    (autoload 'xlsl-mode "xlsl-mode" "autoload the mode." t)))

(load (xah-get-fullpath "xah_emacs_xahsite_path_lisp_util"))

(load (xah-get-fullpath "xah_emacs_keybinding"))
(load (xah-get-fullpath "xah_emacs_mouse_setup"))

(progn
  ;; AutoHotkey
  (add-to-list 'load-path "~/git/xahk-mode.el/")
  (autoload 'xahk-mode "xahk-mode" "Load xahk-mode for editing AutoHotkey scripts." t))

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

(defalias 'xnp 'xahsite-new-page)
(defalias 'xytd 'xah-html-fix-youtube-description)
(defalias 'xytt 'xah-html-youtube-to-text)
(defalias 'xil 'xah-icon-linkify)
(defalias 'xchbl 'xah-copy-html-by-link)
(defalias 'ms 'magit-status)

;; no want tpu-edt
(defalias 'tpu-edt 'forward-char)
(defalias 'tpu-edt-on 'forward-char)

