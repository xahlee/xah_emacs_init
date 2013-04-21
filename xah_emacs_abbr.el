
;; -*- coding: utf-8 -*-
;; emacs abbrev def
;; Xah Lee
;; 2007-06, …, 2013-01-13
;; ∑ http://xahlee.org/

; load my abbreviations file
;(read-abbrev-file "emacs_abbrev")


;; clear existing value; when changing a abbrev, it is convenient for user to not have previous abbrev around
(clear-abbrev-table global-abbrev-table)

(define-abbrev-table 'global-abbrev-table '(

    ("eg" "⁖")
    ("mac" "Mac")
    ("osx" "OS X")
    ("macosx" "Mac OS X")
    ("ipv4" "IPv4")
    ("ipv6" "IPv6")
    ("msw" "Microsoft Windows")
    ("ms" "Microsoft")
    ("ex" "example:")
    ("fex" "for example:")

;
;; http://oxforddictionaries.com/words/the-oec-facts-about-the-language
;("t" "the") ; 1
;;; ("t" "be") ; 2
;("2" "to")
;;; ("t" "of") ; 4
;("n" "and")
;;; ("t" "a") ; 6
;;; ("t" "in") ; 7
;("h" "that")
;("v" "have")
;;; ("t" "I") ; 10
;;; ("t" "it") ; 11
;("4" "for")
;("8" "not")
;; ("t" "on") ; 14
;("w" "with") ; 15
;; ("t" "he") ; 16
;; ("t" "as") ; 17
;("u" "you") ; 18
;; ("t" "do") ; 19
;; ("t" "at") ; 20
;; ("t" "this") ; 21
;("b" "but") ; 22
;; ("t" "his") ; 23
;; ("t" "by") ; 24
;; ("t" "from") ; 25
;; ("t" "they") ; 26
;; ("t" "we") ; 27
;; ("t" "say") ; 28
;; ("t" "her") ; 29
;; ("t" "she") ; 30
;; ("t" "or") ; 31
;; ("t" "an") ; 32
;; ("t" "will") ; 33
;; ("t" "my") ; 34
;; ("t" "one") ; 35
;; ("t" "all") ; 36
;; ("t" "would") ; 37
;; ("t" "there") ; 38
;; ("t" "their") ; 39
;; ("t" "what") ; 40
;; ("t" "so") ; 41
;; ("t" "up") ; 42
;; ("t" "out") ; 43
;; ("t" "if") ; 44
;; ("t" "about") ; 45
;; ("t" "who") ; 46
;; ("t" "get") ; 47
;; ("t" "which") ; 48
;; ("t" "go") ; 49
;; ("t" "me") ; 50
;; ("t" "when") ; 51
;; ("t" "make") ; 52
;; ("t" "can") ; 53
;; ("t" "like") ; 54
;; ("t" "time") ; 55
;; ("t" "no") ; 56
;; ("t" "just") ; 57
;; ("t" "him") ; 58
;; ("t" "know") ; 59
;; ("t" "take") ; 60
;("pp" "people") ; 61
;; ("t" "into") ; 62
;; ("t" "year") ; 63
;; ("t" "your") ; 64
;; ("t" "good") ; 65
;; ("t" "some") ; 66
;; ("t" "could") ; 67
;; ("t" "them") ; 68
;; ("t" "see") ; 69
;; ("t" "other") ; 70
;; ("t" "than") ; 71
;; ("t" "then") ; 72
;; ("t" "now") ; 73
;; ("t" "look") ; 74
;; ("t" "only") ; 75
;; ("t" "come") ; 76
;; ("t" "its") ; 77
;; ("t" "over") ; 78
;; ("t" "think") ; 79
;; ("t" "also") ; 80
;; ("t" "back") ; 81
;; ("t" "after") ; 82
;; ("t" "use") ; 83
;; ("t" "two") ; 84
;; ("t" "how") ; 85
;; ("t" "our") ; 86
;; ("t" "work") ; 87
;; ("t" "first") ; 88
;; ("t" "well") ; 89
;; ("t" "way") ; 90
;; ("t" "even") ; 91
;; ("t" "new") ; 92
;; ("t" "want") ; 93
;("bc" "because")
;; ("t" "any") ; 95
;; ("t" "these") ; 96
; ("t" "give") ; 97
; ("t" "day") ; 98
; ("t" "most") ; 99
; ("t" "us") ; 100

; ("tm" "time")
; ("g" "good")
; ("ipt" "important")

    ("8trad" "traditional")
    ("8addr" "address")
    ("8dict" "dictionary")
    ("8dicts" "dictionaries")
    ("8desc" "description")
    ("8ty" "thank you")
    ("8btw" "by the way")
    ("8afaik" "As far as i know")
    ("8intro" "introduction")
    ("8b" "because")
    ("8pls" "please")
    ("8atm" "at the moment")
    ("8q" "question")
    ("8i18n" "international")
    ("8org" "organization")
    ("8ia" "interactive")
    ("8ann" "announcement")
    ("8ref" "reference")
    ("8temp" "temperature")
    ("8gvn" "government")
    ("8auto" "automatically")
    ("8math" "mathematics")
    ("8diff" "difference")
    ("8comp" "computer")
    ("8l" "language")
    ("8ls" "languages")
    ("8p" "programing")
    ("8exp" "experience")
    ("8tech" "technology")
    ("8cs" "computer science")
    ("8ai" "artificial intelligence")
    ("8fs" "fullscreen")

    ("8ch" "Chinese")
    ("8eng" "English")
    ("8jp" "Japanese")

    ;; tech
    ("8ipa" "IP address")
    ("8wp" "Wikipedia")
    ("8ms" "Microsoft")
    ("8qt" "QuickTime")
    ("8it" "IntelliType")
    ("8win" "window")
    ("8ie" "Internet Explorer")
    ("8yt" "YouTube")
    ("8g" "Google")
    ("8ge" "Google Earth")
    ("8gm" "Google Map")
    ("8gc" "Google Chrome")
    ("8gcd" "googlecode")
    ("8ff" "Firefox")
    ("8sl" "Second Life")
    ("8ll" "Linden Labs")
    ("8pv" "Phoenix Viewer")
    ("8fb" "Facebook")
    ("8ahk" "AutoHotkey")
    ("8pr" "POV-Ray")
    ("8ps" "PowerShell")
    ("8m" "Mathematica")
    ("8cl" "Common Lisp")
    ("8e" "emacs")
    ("8js" "JavaScript")
    ("8vb" "Visual Basic")
    ("8pp" "PayPal")
    ("8ahd" "American Heritage Dictionary")
    ("8wm" "Window Manager")
    ("8el" "emacs lisp")
    ("8os" "Operating System")

;; for programers
    ("8hex" "hexadecimal")
    ("8ui" "user interface")
    ("8alt" "alternative")
    ("8p" "program")
    ("8pl" "programing language")
    ("8paren" "parenthesis")
    ("8dir" "directory")
    ("8dirs" "directories")
    ("8subdir" "sub-directory")
    ("8subdirs" "sub-directories")
    ("8doc" "documentation")
    ("8dl" "download")
    ("8char" "character")
    ("8chars" "characters")
    ("8def" "definition")
    ("8bg" "background")
    ("8kw" "keyword")
    ("8kb" "keyboard")
    ("8kbs" "keyboards")
    ("8kbd" "keybinding")
    ("8env" "environment")
    ("8var" "variable")
    ("8vars" "variables")
    ("8ev" "environment variable")
    ("8evs" "environment variables")
    ("8eg" "ergonomic")
    ("8fp" "functional programing")
    ("8fl" "functional language")
    ("8ca" "cellular automata")
    ("8f" "function")
    ("8para" "parameter")
    ("8prog" "programing")
    ("8db" "database")
    ("8oop" "object oriented programing")

 ;; emacs lisp
    ("8date" "\\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\)")
    ("8d" "\\([0-9]+\\)")
    ("8dot" "\\(.\\)")
    ("8str" "\\([^\"]+?\\)")
    ("8curly" "“\\([^”]+?\\)”")
    ("8bracket" "\\[\\([^]]+?\\)\\]")
    ("8tag" "\\([</>=\" A-za-z0-9]+\\)")

 ;; ruby comment
    ("8rc" "=begin\n\n=end")

    ("8tla" "<div class=\"¤tla\"><a href=\"url\">text</a></div>")
    ("8t" "〔☛ 〕")
    ("8menu" "〖a ▸ b ▸ c〗")
    ("8key" "【Alt+f】")

    ("8li" "────────── ────────── ────────── ────────── ──────────")
    ("8u" "-*- coding: utf-8 -*-")
    ("8ascii" "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")

;; css
("8white" "#ffffff")
("8silver" "#c0c0c0")
("8gray" "#808080")
("8black" "#000000")
("8red" "#ff0000")
("8maroon" "#800000")
("8yellow" "#ffff00")
("8olive" "#808000")
("8lime" "#00ff00")
("8green" "#008000")
("8aqua" "#00ffff")
("8teal" "#008080")
("8blue" "#0000ff")
("8navy" "#000080")
("8fuchsia" "#ff00ff")
("8purple" "#800080")
("8orange" "#ffa500")
("8hsl" "hsl(0,100%,50%)")

;; HTML
("8cmt" "<!-- \n -->")
("8html5" "<!DOCTYPE html>")
("8html4s" "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">")
("8html4t" "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\" \"http://www.w3.org/TR/html4/loose.dtd\">")
("8xhtml" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">")
("8html" "<!doctype html><html><head><meta charset=\"utf-8\" />
<title>ttt</title>
</head>
<body>

</body>
</html>")

("8php" "<?php\n\n?>")
("8sb" "<div class=\"section\">⁂</div>")

("8c" "class=\"\"")
("8bq" "blockquote")
("8w" "width")
("8h" "height")
("8bgc" "background-color")

("8fg" "<figure>\n<figcaption>\n</figcaption>\n</figure>")
("8fc" "<figcaption>\n</figcaption>")
("8faq" "<p class=\"q\">How to do this?</p>
<div class=\"a\">
<p>this way</p>
</div>

")
("8css" "<link rel=\"stylesheet\" href=\"../../lbasic.css\" />")
("8css2" "<style type=\"text/css\">\np {line-height:130%}\n</style>")


;;; unix shell

    ("3s" "rsync -z -r -v -t --chmod=Dugo+x --chmod=ugo+r --delete --exclude='*~' --exclude='.bash_history' --exclude='logs/'  --rsh='ssh -l u40651120' ~/web/ u40651120@s168753655.onlinehome.us:~/")
    ("3rsn2" "rsync -z -r -v -t  --delete  ~/web/  /media/OS/Users/h3/web/")

    ("3ssh" "ssh -l u40651120 xahlee.org ")
    ("3im" "convert -quality 85% ")
    ("3ims" "convert -scale 50% -quality 85% ")
    ("3im256" "convert +dither -colors 256 ")
    ("3imf" "find . -name \"*png\" | xargs -l -i basename \"{}\" \".png\" | xargs -l -i  convert -quality 85% \"{}.png\" \"{}.jpg\"")
    ("3bmp2png" "find . -name \"*bmp\" | xargs -l -i basename \"{}\" \".bmp\" | xargs -l -i  convert \"{}.bmp\" \"{}.png\"")

    ("3grep" "grep -r -F 'xxx' --include='*html' ~/web")

    ("3f0" "find . -type f -empty")
    ("3f0rm" "find . -type f -size 0 -exec rm {} ';'")
    ("3frm" "find . -name \"*~\" -exec rm {} ';'")
    ("3f0d" "find . -depth -empty -type d")
    ("3f0drm" "find . -depth -empty -type d -exec rmdir {} ';'")
    ("3chmod" "find . -type f -exec chmod 644 {} ';'")
    ("3chmod2" "find . -type d -exec chmod 755 {} ';'")
    ("3lynx" "lynx -dump -assume_local_charset=utf-8 -display_charset=utf-8 -width=100")
    ("3feh" "feh --randomize --recursive --auto-zoom --action \"gvfs-trash '%f'\" --geometry 1600x1000 ~/Pictures/cinse_pixra3/ &")

; rsync -r -v -t --chmod=Dugo+x --chmod=ugo+r --delete --exclude="**/My *" --rsh="ssh -l xah" /media/HP/Users/xah/Documents /media/HP/Users/xah/web /media/HP/Users/xah/Pictures /media/HP/Users/xah/Shared /media/HP/Users/xah/cinse_pixra3 xah@192.168.1.6:~/

; rsync -r -v -t --delete --rsh="ssh -l xah" ~/web/ xah@169.254.153.147:~/web/

; rsync -r -v -t --delete --exclude="**/My *" --rsh="ssh -l xah" ~/Documents/ xah@169.254.153.147:~/Documents/

; unison -servercmd /usr/bin/unison c:/Users/xah/web ssh://xah@169.254.145.104//Users/xah/web
; sftp u40651120@s168753655.onlinehome.us
;


;;; xah personal
    ("8ee" "ErgoEmacs")
    ("8vdspc" "Visual Dictionary of Special Plane Curves")
    ("8xl" "Xah Lee")
    ("8xim" "Twitter: @xah_lee
Facebook: https://www.facebook.com/xahlee
g+: https://plus.google.com/112757647855302148298/posts
Google talk: xahlee@gmail.com
Skype: XahLee
AIM: xahlee
Yahoo: P0lyglut
MSN: p0lyglut@yahoo.com or xahlee
Second Life: Xah Toll
")

;; xah url
("8xs" " Xah ∑ http://xahlee.org/")

("8uee" "http://ergoemacs.org/")
("8uwe" "http://wordyenglish.com/")
("8uxa" "http://xaharts.org/")
("8uxl" "http://xahlee.org/")
("8uxli" "http://xahlee.info/")
("8uxm" "http://xahmusic.org/")
("8uxp" "http://xahporn.org/")
("8uxsl" "http://xahsl.org/")
("8wec" "wordyenglish_com")

("8uvmm" "http://VirtualMathMuseum.org/")
("8u3dxm" "http://3D-XplorMath.org/")

("8uvi" "http://ergoemacs.org/emacs/emergency_vi.html")
("8uemacs" "http://ergoemacs.org/emacs/emacs.html")
("8uelisp" "http://ergoemacs.org/emacs/elisp.html")
("8uahk" "http://xahlee.info/mswin/autohotkey.html")
("8ueek" "http://ergoemacs.org/emacs/ergonomic_emacs_keybinding.html")
("8uhtml" "http://xahlee.info/js/index.html")

("8uperl" "http://xahlee.info/perl-python/perl_basics.html")
("8upython" "http://xahlee.info/python/python3_basics.html")
("8uruby" "http://xahlee.info/ruby/ruby_basics.html")
("8ujs" "http://xahlee.info/js/javascript_basics.html")
("8uphp" "http://xahlee.info/php/php_basics.html")
("8ucss" "http://xahlee.info/js/css_index.html")

("8uocaml" "http://xahlee.info/ocaml/ocaml.html")
("8ups" "http://xahlee.info/powershell/index.html")
("8umma" "http://xahlee.info/M/index.html")
("8ujava" "http://xahlee.info/java-a-day/java.html")
("8utg" "http://xahlee.info/UnixResource_dir/writ/tech_geeker.html")

    ))


;;; stop asking whether to save newly added abbrev when quitting emacs
(setq save-abbrevs nil)

