;; -*- coding: utf-8 -*-
;; settings for packages that's not bundled with emacs
;;   Xah Lee
;; ∑ http://xahlee.org/

(when (fboundp 'global-undo-tree-mode)
  (global-undo-tree-mode 1)
  (defalias 'redo 'undo-tree-redo))

(when (fboundp 'xah-lookup-google)
  (when (or
         (string-equal system-type "gnu/linux")
         (string-equal system-type "darwin"))
    (require 'eww)
    (setq xah-lookup-dictionary-browser-function 'eww)))

(when (fboundp 'smex)
  ;; enhanced execute-extended-command
  (require 'smex)
  (smex-initialize))

(when (fboundp 'xah-math-input-mode)
  (xah-math-input-mode 1))

(when (fboundp 'keyfreq-mode)
  ;; record command call statistics
  (require 'keyfreq)
  (setq keyfreq-file "~/.emacs.d/.emacs.keyfreq")
  (setq keyfreq-file-lock "~/.emacs.d/.emacs.keyfreq.lock")
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(when (fboundp 'global-page-break-lines-mode)
  ;; make the formfeed char (^L) display as a line
  (global-page-break-lines-mode 1)
  (setq
   page-break-lines-modes
   '(
     emacs-lisp-mode lisp-mode scheme-mode compilation-mode outline-mode help-mode
     fundamental-mode text-mode org-mode ruby-mode python-mode html-mode nxml-mode
     xah-html-mode xah-elisp-mode
     )))

(when (fboundp 'htmlize-region)
  ;; htmlize.el
  ;; make htmlize generate unicode directly instead of html entities
  (setq htmlize-convert-nonascii-to-entities nil)
  ; make the output html use utf-8 charset
  (setq htmlize-html-charset "utf-8"))

(when (fboundp 'xah-find-text)
  (setq
   xah-find-dir-ignore-regex-list
   [
    "\\.git/"
    "xahlee_info/php-doc/"
    "xahlee_info/node_api/"
    "xahlee_info/java8_doc/"
    "xahlee_info/css_transitions/"
    "xahlee_info/css3_spec_bg/"
    "xahlee_info/css_3_color_spec/"
    "xahlee_info/REC-SVG11-20110816/"
    "xahlee_info/python_doc_3.3.3/"
    "xahlee_info/python_doc_2.7.6/"
    "xahlee_info/jquery_doc/"
    "xahlee_info/javascript_ecma-262_5.1_2011/"
    "xahlee_info/javascript_ecma-262_6_2015/"
    "xahlee_info/javascript_es6/"
    "xahlee_info/git-bottomup/"
    "xahlee_info/dom-whatwg/"
    "xahlee_info/css_2.1_spec/"
    "xahlee_info/clojure-doc-1.6/"
    ]))
