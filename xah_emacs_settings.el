;; -*- coding: utf-8 -*-
;; Emacs settings for packages bundled with emacs only

;; 2007-06
;;   Xah Lee
;; ∑ http://xahlee.org/


;; fundamental

(set-default-coding-systems 'utf-8-unix)

(set-background-color "honeydew")
(setq inhibit-splash-screen t)

(setq x-select-enable-clipboard-manager nil)

;; (setq ediff-window-setup-function 'ediff-setup-windows-plain)﻿
;; ;; (setq ediff-split-window-function 'split-window-horizontally)


;; initial window and default window

;123456789;123456789;123456789;123456789;123456789;123456789;123456789;123456789;123456789

(setq initial-frame-alist
      '(
        (width . 92)
        (height . 54)
        (background-color . "honeydew")
        ) )

(setq default-frame-alist
      '((menu-bar-lines . 1)
        (left-fringe)
        (right-fringe)
        (tool-bar-lines . 0)
        (width . 92)
        (height . 52)
        (background-color . "#eeeedd")
        ))



;; Emacs: How to List ＆ Set Font
;; http://ergoemacs.org/emacs/emacs_list_and_set_font.html

;; set default font
(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (when (member "DejaVu Sans Mono" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "DejaVu Sans Mono-12"))
    (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-12"))
    )
  )
 ((string-equal system-type "darwin")   ; Mac OS X
  (when (member "DejaVu Sans Mono" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "DejaVu Sans Mono-10"))
    (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10")))
  )
 ((string-equal system-type "gnu/linux") ; linux
  (when (member "DejaVu Sans Mono" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "DejaVu Sans Mono-10"))
    (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10")))
  )
 )

;; specify font for all unicode characters
(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola"))

(when (member "WenQuanYi Micro Hei" (font-family-list))
  (set-fontset-font nil '(#x4e00 . #x9fff) "WenQuanYi Micro Hei" ))

;; 提笔写忧伤，落笔映惆怅！无奈花落时，唯有独哀殇！

;; ("newspaper"
;; "gothic"
;; "mincho"
;; "fangsong ti"
;; "song ti"
;; "fixed"
;; "nil"
;; "clearlyu alternate glyphs"
;; "clearlyu arabic extra"
;; "clearlyu arabic"
;; "clearlyu devanagari"
;; "clearlyu devangari extra"
;; "clearlyu ligature"
;; "clearlyu pua"
;; "clearlyu"
;; "clean"
;; "fixed"
;; "open look cursor"
;; "open look glyph"
;; "newspaper"
;; "gothic"
;; "mincho"
;; "fangsong ti"
;; "song ti"
;; "fixed"
;; "nil"
;; "clearlyu alternate glyphs"
;; "clearlyu arabic extra"
;; "clearlyu arabic"
;; "clearlyu devanagari"
;; "clearlyu devangari extra"
;; "clearlyu ligature"
;; "clearlyu pua"
;; "clearlyu"
;; "clean"
;; "fixed"
;; "open look cursor"
;; "open look glyph"
;; "bitstream charter"
;; "courier 10 pitch"
;; "latin modern roman"
;; "latin modern sans"
;; "latin modern sansquotation"
;; "latin modern typewriter variable width"
;; "latin modern typewriter"
;; "standard symbols l"
;; "fixed"
;; "utopia"
;; "bitstream charter"
;; "swiss 721"
;; "courier"
;; "mathematica1"
;; "mathematica1mono"
;; "mathematica2"
;; "mathematica2mono"
;; "mathematica3"
;; "mathematica3mono"
;; "mathematica4"
;; "mathematica4mono"
;; "mathematica5"
;; "mathematica5mono"
;; "mathematica6"
;; "mathematica6mono"
;; "mathematica7"
;; "mathematica7mono"
;; "helvetica"
;; "times"
;; "UnGraphic"
;; "wasy10"
;; "Ume Gothic C4"
;; "Ume Gothic C5"
;; "Ume Gothic O5"
;; "Ume Gothic S4"
;; "Ume Gothic S5"
;; "UnDotum"
;; "LMMonoLt10"
;; "Century Schoolbook L"
;; "OpenSymbol"
;; "Khmer OS System"
;; "LMSansQuot8"
;; "msam10"
;; "UnDinaru"
;; "Andale Mono"
;; "UnGungseo"
;; "Trebuchet MS"
;; "Mukti Narrow"
;; "Meera"
;; "Symbola"
;; "Droid Sans Mono"
;; "Ume Mincho S3"
;; "Webdings"
;; "Vemana2000"
;; "KacstQurn"
;; "Ume P Gothic C4"
;; "Ume P Gothic C5"
;; "Ume P Gothic O5"
;; "Ume P Gothic S4"
;; "Ume P Gothic S5"
;; "Ume UI Gothic"
;; "LMMonoSlant10"
;; "Umpush"
;; "DejaVu Sans Mono"
;; "Arial Black"
;; "Purisa"
;; "esint10"
;; "Pothana2000"
;; "msbm10"
;; "KacstBook"
;; "KacstLetter"
;; "cmr10"
;; "Norasi"
;; "Loma"
;; "Verdana"
;; "KacstDigital"
;; "KacstTitleL"
;; "mry_KacstQurn"
;; "URW Palladio L"
;; "Untitled1"
;; "Phetsarath OT"
;; "Sawasdee"
;; "UnPilgi"
;; "Ume P Mincho S3"
;; "Tlwg Typist"
;; "URW Gothic L"
;; "Dingbats"
;; "URW Chancery L"
;; "Ubuntu"
;; "Ume Gothic"
;; "FreeSerif"
;; "Times New Roman"
;; "Ume P Mincho"
;; "Ubuntu Condensed"
;; "ori1Uni"
;; "KacstOffice"
;; "WenQuanYi Micro Hei Mono"
;; "Kedage"
;; "DejaVu Sans"
;; "Kinnari"
;; "KacstArt"
;; "Ume Mincho"
;; "TlwgMono"
;; "NanumMyeongjo"
;; "LMSans10"
;; "Lohit Punjabi"
;; "LMRoman10"
;; "rsfs10"
;; "Symbol"
;; "LMRomanDunh10"
;; "Bitstream Charter"
;; "NanumGothic"
;; "KacstOne"
;; "Comic Sans MS"
;; "Khmer OS"
;; "Courier 10 Pitch"
;; "cmmi10"
;; "Liberation Sans Narrow"
;; "Liberation Mono"
;; "Nimbus Sans L"
;; "TlwgTypewriter"
;; "TakaoPGothic"
;; "LMRomanDemi10"
;; "Rachana"
;; "WenQuanYi Micro Hei"
;; "Droid Serif"
;; "LMMonoCaps10"
;; "LMMonoLtCond10"
;; "Standard Symbols L"
;; "Lohit Gujarati"
;; "KacstPen"
;; "KacstDecorative"
;; "Nimbus Mono L"
;; "Ume UI Gothic O5"
;; "Liberation Serif"
;; "Mallige"
;; "Nimbus Roman No9 L"
;; "eufm10"
;; "LMRomanUnsl10"
;; "Ubuntu"
;; "KacstPoster"
;; "Liberation Sans"
;; "LMMono10"
;; "Mukti Narrow"
;; "FreeSans"
;; "cmex10"
;; "Georgia"
;; "KacstNaskh"
;; "Lohit Tamil"
;; "Tlwg Typo"
;; "LMRomanCaps10"
;; "UnBatang"
;; "KacstFarsi"
;; "Lohit Bengali"
;; "LMSansDemiCond10"
;; "Arial"
;; "LMRomanSlant10"
;; "Courier New"
;; "Waree"
;; "KacstTitle"
;; "gargi"
;; "Lohit Hindi"
;; "DejaVu Serif"
;; "Saab"
;; "LMMonoProp10"
;; "Garuda"
;; "Rekha"
;; "KacstScreen"
;; "Impact"
;; "FreeMono"
;; "Ubuntu Mono"
;; "URW Bookman L"
;; "Ume P Gothic"
;; "LMMonoPropLt10"
;; "cmsy10"
;; "Droid Sans")

;; Emacs Lisp: Determine OS, Emacs Version, Machine Host Name
;; http://ergoemacs.org/emacs/elisp_determine_OS_version.html



(winner-mode 0)
(electric-pair-mode 0)
(blink-cursor-mode 0 )
(setq sentence-end-double-space nil )
(electric-indent-mode 0) ; default is on in emacs 24.4
(global-auto-revert-mode 1)

(setq scroll-error-top-bottom t )

(setq tab-width 1)   ; width for display tabs. emacs 23.1 default is 8
(set-default 'abbrev-mode t)

(setq shift-select-mode nil)

(setq org-startup-folded nil)
(setq org-return-follows-link t)

(add-to-list 'auto-mode-alist '("\\.visi\\'" . clojure-mode))

(when (fboundp 'eww)
  (progn
    (defun xah-rename-eww-hook ()
      "Rename eww browser's buffer so sites open in new page."
      (rename-buffer "eww" t))
    (add-hook 'eww-mode-hook 'xah-rename-eww-hook)))



;; (which-function-mode 1) ; show current function in mode line

;; emacs slows down when you open a file with tens thousands lines
;; emacs freezes
(global-linum-mode 0)

;; (setq auto-save-default t)
;; (setq auto-save-visited-file-name t )

;; set the fallback input method to Chinese for toggle-input-method
(setq default-input-method 'chinese-py) ; as of emacs 24, default is nil anyway.


;; 2009-09-29 see http://groups.google.com/group/ergoemacs/msg/9eec3b455cab3ff1 and http://stackoverflow.com/questions/885793/emacs-error-when-calling-server-start
; (and (= emacs-major-version 23) (defun server-ensure-safe-dir (dir) "Noop" t))



;; (setcdr (assq 'continuation fringe-indicator-alist) '(nil right-curly-arrow))


;; (custom-set-variables
;;   ;; custom-set-variables was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  '(abbrev-mode t)
;;  ;; '(auto-save-default nil)
;;  ;; '(initial-major-mode (quote text-mode))
;;  ;; '(initial-scratch-message "")
;;  ;; '(line-number-display-limit-width 500)
;;  ;; '(mouse-buffer-menu-mode-mult 4)
;;  ;; '(pov-run-high "+R2 +A0.1 +J1.2 +Am2 +Q9 +H480 +W640 +i%s")
;;  ;; '(recentf-exclude (quote ("/ftp")))
;;  ;; '(recentf-max-menu-items 11)
;;  ;; '(recentf-max-saved-items 31)
;;  ;; '(report-emacs-bug-no-confirmation t)
;;  ;; '(report-emacs-bug-no-explanations t)
;;  ;; '(scalable-fonts-allowed t)
;;  ;; '(user-full-name "Xah Lee")
;;  ;; '(user-mail-address "xah@xahlee.org")
;;  ;; '(w32shell-add-emacs-to-path t)
;;  ;; '(w32shell-cygwin-bin "C:\\cygwin\\bin")
;;  ;; '(w32shell-msys-bin "C:\\msys\\1.0\\bin")
;;  ;; '(w32shell-shell (quote cygwin))
;;  ;; '(xlsl-mode-format-style 1)
;;  ;; '(xlsl-reference-url "http://lslwiki.net/lslwiki/wakka.php?wakka=")
;; )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(completions-common-part ((t (:inherit default :foreground "red"))))
 '(diredp-compressed-file-suffix ((t (:foreground "#7b68ee"))))
 '(diredp-ignored-file-name ((t (:foreground "#aaaaaa"))))

 '(rainbow-delimiters-depth-1-face ((t (:foreground "black"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "black"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "blue"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "blue"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "red"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "red"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "gray"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "gray"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "gray"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "green"))))

 '(show-paren-match ((((class color) (background light)) (:background "azure2")))))