;; -*- coding: utf-8 -*-
;; 2015-01-15 Xah Lee


;; Emacs: How to List ＆ Set Font
;; http://ergoemacs.org/emacs/emacs_list_and_set_font.html

;; set a default font
(when (member "DejaVu Sans Mono" (font-family-list))
  (set-face-attribute 'default nil :font "DejaVu Sans Mono"))

;; specify font for all unicode characters
(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))

;; specify font for chinese characters using default chinese font on linux
(when (member "WenQuanYi Micro Hei" (font-family-list))
  (set-fontset-font t '(#x4e00 . #x9fff) "WenQuanYi Micro Hei" ))

;; 西游记，第一回：灵根育孕源流出，心性修持大道生
;; http://wordyenglish.com/monkey_king/x001-1.html

;; range of Chinese chars in Unicode BMP plane
;; '(#x4e00 . #x9fff)

;; Unicode Emoticons, Faces 😃 😄 😱 😸 👸 👽 👍
;; http://xahlee.info/comp/unicode_6_emoticons_list.html

;; list-fontsets
;; Fontset: -unknown-DejaVu Sans Mono-normal-normal-normal-*-13-*-*-*-m-0-fontset-auto1
;; Fontset: -unknown-DejaVu Sans Mono-normal-normal-normal-*-13-*-*-*-m-0-fontset-auto2
;; Fontset: -*-*-*-*-*-*-*-*-*-*-*-*-fontset-default
;; Fontset: -*-fixed-medium-r-normal-*-16-*-*-*-*-*-fontset-standard
;; Fontset: -unknown-Ubuntu Mono-normal-normal-normal-*-17-*-*-*-m-0-fontset-startup

;; call describe-fontset then press tab to list fontset
;; -*-*-*-*-*-*-*-*-*-*-*-*-fontset-default
;; -*-fixed-medium-r-normal-*-16-*-*-*-*-*-fontset-standard
;; -unknown-DejaVu Sans Mono-normal-normal-normal-*-13-*-*-*-m-0-fontset-auto1
;; -unknown-DejaVu Sans Mono-normal-normal-normal-*-13-*-*-*-m-0-fontset-auto2
;; -unknown-Ubuntu Mono-normal-normal-normal-*-17-*-*-*-m-0-fontset-startup
;; -unknown-dejavu sans mono-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1
;; -unknown-ubuntu mono-normal-normal-normal-*-17-*-*-*-m-0-iso10646-1
;; fontset-auto1 	fontset-auto2
;; fontset-default 	fontset-standard
;; fontset-startup

;; call describe-character-set then press tab to list all charsets

;; Emacs Lisp: Determine OS, Emacs Version, Machine Host Name
;; http://ergoemacs.org/emacs/elisp_determine_OS_version.html

