;; -*- coding: utf-8; lexical-binding: t; -*-
;; settings for packages that's not bundled with emacs
;;   Xah Lee
;; ∑ http://xahlee.org/

;; (when (fboundp 'global-undo-tree-mode)
;;   (global-undo-tree-mode 1)
;;   )

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
  (global-xah-math-input-mode 1))

(when (fboundp 'keyfreq-mode)
  ;; record command call statistics
  (require 'keyfreq)
  (setq keyfreq-file "~/.emacs.d/.emacs.keyfreq")
  (setq keyfreq-file-lock "~/.emacs.d/.emacs.keyfreq.lock")
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

;; (when (fboundp 'global-page-break-lines-mode)
;;   ;; make the formfeed char (^L) display as a line
;;   (global-page-break-lines-mode 1)
;;   (setq
;;    page-break-lines-modes
;;    '(
;;      emacs-lisp-mode lisp-mode scheme-mode compilation-mode outline-mode help-mode
;;      fundamental-mode text-mode org-mode ruby-mode python-mode html-mode nxml-mode
;;      xah-html-mode xah-elisp-mode
;;      )))

(when (fboundp 'htmlize-region)
  ;; htmlize.el
  ;; make htmlize generate unicode directly instead of html entities
  (setq htmlize-convert-nonascii-to-entities nil)
  ; make the output html use utf-8 charset
  (setq htmlize-html-charset "utf-8"))

;; (when (and (fboundp 'which-key-mode)
;;            which-key-mode
;;            )
;;   ;; (which-key-setup-side-window-right)
;;   ;; (setq which-key-popup-type 'side-window)
;;   )

;; (when (featurep 'guide-key)
;;   (setq guide-key/guide-key-sequence '("C-x" "x" "<menu>" "<end>" "<delete>"))
;;   (guide-key-mode 1))

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
    "xahlee_info/html5_whatwg/"
    "xahlee_info/css_2.1_spec/"
    "xahlee_info/clojure-doc-1.8/"
    "ergoemacs_org/emacs_manual/"
    ]))

;; (when (fboundp 'elfeed)
;;   (setq elfeed-feeds
;;         '("http://nullprogram.com/feed/"
;;           "http://sachachua.com/blog/category/emacs-news/feed/"
;;           "http://irreal.org/blog/?feed=rss2"
;;           "http://www.terminally-incoherent.com/blog/feed/"
;;           "http://ejohn.org/subscribe/"
;;           "http://ergoemacs.org/emacs/blog.xml"
;;           "http://xahlee.info/comp/blog.xml"
;;           "http://xahlee.info/js/blog.xml"
;;           "http://xahlee.info/math/blog.xml"
;;           "http://wordyenglish.com/lit/blog.xml"
;;           "http://xahmusic.org/music/blog.xml"
;;           "http://xaharts.org/arts/blog.xml"
;;           "http://xahlee.org/sex/blog.xml"
;;           "http://xahsl.org/sl/blog.xml"
;;           "http://wordyenglish.com/chinese/blog.xml"
;;           )))

;; (when (fboundp 'xah-find-replace-text)
;; (setq xah-find-context-char-count-before 0)
;; (setq xah-find-context-char-count-after 99) )

(when (fboundp 'global-company-mode)
  (global-company-mode 1))
