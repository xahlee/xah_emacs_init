;; -*- coding: utf-8 -*-
;; 2015-01-15 Xah Lee


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
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))

;; specify font for chinese characters using default chinese font on linux
(when (member "WenQuanYi Micro Hei" (font-family-list))
  (set-fontset-font t '(#x4e00 . #x9fff) "WenQuanYi Micro Hei" ))

;; 西游记，第一回：灵根育孕源流出，心性修持大道生 (页1)
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
;; adobe-standard-encoding
;; alternativnyj
;; arabic-1-column
;; arabic-2-column
;; arabic-digit
;; arabic-iso8859-6
;; ascii
;; assamese-cdac
;; bengali-akruti
;; bengali-cdac
;; big5
;; big5-hkscs
;; chinese-big5-1
;; chinese-big5-2
;; chinese-cns11643-1
;; chinese-cns11643-15
;; chinese-cns11643-2
;; chinese-cns11643-3
;; chinese-cns11643-4
;; chinese-cns11643-5
;; chinese-cns11643-6
;; chinese-cns11643-7
;; chinese-gb2312
;; chinese-gbk
;; chinese-sisheng
;; control-1
;; cp00858
;; cp1047
;; cp1125
;; cp1250
;; cp1251
;; cp1252
;; cp1253
;; cp1254
;; cp1255
;; cp1256
;; cp1257
;; cp1258
;; cp154
;; cp437
;; cp720
;; cp737
;; cp775
;; cp850
;; cp851
;; cp852
;; cp855
;; cp857
;; cp858
;; cp860
;; cp861
;; cp862
;; cp863
;; cp864
;; cp865
;; cp866
;; cp866u
;; cp869
;; cp874
;; cp932
;; cp932-2-byte
;; cp936
;; cp949
;; cp949-2-byte
;; cyrillic-iso8859-5
;; devanagari-akruti
;; devanagari-cdac
;; ebcdic-uk
;; ebcdic-us
;; eight-bit
;; eight-bit-control
;; eight-bit-graphic
;; emacs
;; ethiopic
;; gb18030
;; gb18030-2-byte
;; gb18030-4-byte-bmp
;; gb18030-4-byte-ext-1
;; gb18030-4-byte-ext-2
;; gb18030-4-byte-smp
;; georgian-academy
;; georgian-ps
;; greek-iso8859-7
;; gujarati-akruti
;; gujarati-cdac
;; hebrew-iso8859-8
;; hp-roman8
;; ibm1047
;; ibm850
;; ibm866
;; indian-1-column
;; indian-2-column
;; indian-glyph
;; indian-is13194
;; ipa
;; iso-8859-1
;; iso-8859-10
;; iso-8859-11
;; iso-8859-13
;; iso-8859-14
;; iso-8859-15
;; iso-8859-16
;; iso-8859-2
;; iso-8859-3
;; iso-8859-4
;; iso-8859-5
;; iso-8859-6
;; iso-8859-7
;; iso-8859-8
;; iso-8859-9
;; japanese-jisx0208
;; japanese-jisx0208-1978
;; japanese-jisx0212
;; japanese-jisx0213-1
;; japanese-jisx0213-2
;; japanese-jisx0213-a
;; japanese-jisx0213.2004-1
;; jisx0201
;; kannada-akruti
;; kannada-cdac
;; katakana-jisx0201
;; katakana-sjis
;; koi8
;; koi8-r
;; koi8-t
;; koi8-u
;; korean-ksc5601
;; lao
;; latin-iso8859-1
;; latin-iso8859-10
;; latin-iso8859-13
;; latin-iso8859-14
;; latin-iso8859-15
;; latin-iso8859-16
;; latin-iso8859-2
;; latin-iso8859-3
;; latin-iso8859-4
;; latin-iso8859-9
;; latin-jisx0201
;; mac-roman
;; malayalam-akruti
;; malayalam-cdac
;; mik
;; mule-lao
;; mule-unicode-0100-24ff
;; mule-unicode-2500-33ff
;; mule-unicode-e000-ffff
;; next
;; oriya-akruti
;; oriya-cdac
;; pt154
;; ptcp154
;; punjabi-akruti
;; punjabi-cdac
;; ruscii
;; sanskrit-cdac
;; symbol
;; tamil-akruti
;; tamil-cdac
;; tcvn-5712
;; telugu-akruti
;; telugu-cdac
;; thai-iso8859-11
;; thai-tis620
;; tibetan
;; tibetan-1-column
;; tis620-2533
;; ucs
;; unicode
;; unicode-bmp
;; unicode-sip
;; unicode-smp
;; unicode-ssp
;; vietnamese-viscii-lower
;; vietnamese-viscii-upper
;; viscii
;; vscii
;; vscii-2
;; windows-1250
;; windows-1251
;; windows-1252
;; windows-1253
;; windows-1254
;; windows-1255
;; windows-1256
;; windows-1257
;; windows-1258
;; windows-936

;; fonts
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

