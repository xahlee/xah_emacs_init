;; -*- coding: utf-8; lexical-binding: t; -*-
;; settings for packages that's not bundled with emacs
;;   Xah Lee
;; ∑ http://xahlee.org/

;; (when (fboundp 'global-undo-tree-mode)
;;   (global-undo-tree-mode 1)
;;   )

;; (when (fboundp 'smex)
;;   ;; enhanced execute-extended-command
;;   (require 'smex)
;;   (smex-initialize))

;; (when (fboundp 'magit-status)
;;   (define-key magit-file-section-map (kbd "u") nil)
;;   (define-key magit-file-section-map (kbd "a") nil))

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
  (setq xah-find-dir-ignore-regex-list (append xahsite-external-docs [ "\\.git/" "xahlee_info/js/ex/" ] nil)))

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

(when (fboundp 'go-mode)
  (add-hook 'before-save-hook #'gofmt-before-save))
