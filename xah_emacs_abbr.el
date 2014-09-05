;; -*- coding: utf-8 -*-
;; emacs abbrev def
;; 〈Using Emacs Abbrev Mode for Abbreviation〉 http://ergoemacs.org/emacs/emacs_abbrev_mode.html

; load my abbreviations file
;(read-abbrev-file "emacs_abbrev")


(clear-abbrev-table global-abbrev-table)

(define-abbrev-table 'global-abbrev-table
  '(

    ("eg" "⁖")
    ("mac" "Mac")
    ("osx" "OS X")
    ("macosx" "Mac OS X")
    ("ipv4" "IPv4")
    ("ipv6" "IPv6")
    ("msw" "Microsoft Windows")
    ("ms" "Microsoft")
    ("ex" "example")
    ("fex" "for example")
    ("jq" "jQuery")

    ("arrows""➵➙►▻☛☞👉⇰➛➜➝➞➟➠➢➣➤➥➦➧➨➲➺➻➼➽➾")

    ("hearts""♥💕💓💔💖💗💘💝💞💟💙💚💛💜")

    ;; http://oxforddictionaries.com/words/the-oec-facts-about-the-language

    ("dsnt" "doesn't")
    ("dnt" "don't")
    ("isnt" "isn't")
    ("cnt" "can't")
    ("wnt" "won't")
    ("hnt" "haven't")
    ("shnt" "shouldn't")
    ("r" "are")
    ("ur" "you are")

    ("t" "the")
    ;; ("t" "be") ; 2
    ;; ("2" "to")                          ; problem with just enter a number
    ;; ("t" "of") ; 4
    ("n" "and") ;3
;;; ("t" "a") ; 6
;;; ("t" "in") ; 7

  ;("h" "that") ; 8
    ("h" "have") ; 9
;;; ("t" "I") ; 10
;;; ("t" "it") ; 11
    ("f" "for")  ; 12
  ;("8" "not")
    ;; ("t" "on") ; 14
    ("w" "with") ; 15
    ;; ("t" "he") ; 16
    ;; ("t" "as") ; 17
    ("u" "you") ; 18

    ;; ("t" "do") ; 19
    ;; ("t" "at") ; 20
    ;; ("t" "this") ; 21
    ("b" "but")      ; 22
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
    ;; ("w" "when") ; 51
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
    ("bc" "because") ; 94
    ;; ("t" "any") ; 95
    ;; ("t" "these") ; 96
  ; ("t" "give") ; 97
  ; ("t" "day") ; 98
  ; ("t" "most") ; 99
  ; ("t" "us") ; 100

  ; ("tm" "time")
  ; ("g" "good")
  ; ("ipt" "important")

    ("bt" "between")
    ("uns" "understand")
    ("comm" "communication")
    ("rsi" "Repetitive Strain Injury")
    ("disc" "discussion")
    ("trad" "traditional")
    ("addr" "address")
    ("dict" "dictionary")
    ("dicts" "dictionaries")
    ("desc" "description")
    ("ty" "thank you")
    ("btw" "by the way")
    ("afaik" "As far as i know")
    ("intro" "introduction")
    ("pls" "please")
    ("atm" "at the moment")
    ("q" "question")
    ("i18n" "international")
    ("org" "organization")
    ("ia" "interactive")
    ("ann" "announcement")
    ("ref" "reference")
    ("temp" "temperature")
    ("gvn" "government")
    ("auto" "automatically")
    ("maths" "mathematics")
    ("wtdb" "what's the difference between")
    ("dfb" "difference between")
    ("dfc" "difference")
    ("dft" "different")
    ("comp" "computer")
    ("l" "language")
    ("ls" "languages")
    ("exp" "experience")
    ("tech" "technology")
    ("cs" "computer science")
    ("ai" "artificial intelligence")
    ("fs" "fullscreen")
    ("vid" "video")

    ("ch" "Chinese")
    ("eng" "English")
    ("jp" "Japanese")

    ;; tech
    ("ipa" "IP address")
    ("wp" "Wikipedia")
    ("ms" "Microsoft")
    ("qt" "QuickTime")
    ("8it" "IntelliType")
    ("win" "window")
    ("ie" "Internet Explorer")
    ("yt" "YouTube")
    ("8g" "Google")
    ("ge" "Google Earth")
    ("gh" "Google Hangout")
    ("gm" "Google Map")
    ("gc" "Google Chrome")
    ("gp" "Google Plus")
    ("8so" "StackOverflow")
    ("gcd" "googlecode")
    ("ff" "Firefox")
    ("sl" "Second Life")
    ("bb" "Backbone")
    ("lsl" "Linden Scripting Language")
    ("8ll" "Linden Labs")
    ("fb" "Facebook")
    ("ahk" "AutoHotkey")
    ("pr" "POV-Ray")
    ("ps" "PowerShell")
    ("mma" "Mathematica")
    ("wl" "Wolfram Language")
    ("cl" "Common Lisp")
    ("8e" "emacs")
    ("js" "JavaScript")
    ("vb" "Visual Basic")
    ("pp" "PayPal")
    ("ahd" "American Heritage Dictionary")
    ("wm" "Window Manager")
    ("el" "emacs lisp")
    ("os" "Operating System")
    ("cli" "command line interface")

    ;; for programers
    ("hex" "hexadecimal")
    ("ui" "user interface")
    ("gui" "graphical user interface")
    ("alt" "alternative")

    ("wd" "web development")
    ("dev" "develop")
    ("p" "program")
    ("pl" "programing language")
    ("paren" "parenthesis")
    ("dir" "directory")
    ("dirs" "directories")
    ("subdir" "sub-directory")
    ("subdirs" "sub-directories")
    ("doc" "documentation")
    ("dl" "download")
    ("char" "character")
    ("chars" "characters")
    ("def" "definition")
    ("bg" "background")
    ("kw" "keyword")
    ("kb" "keyboard")
    ("kbs" "keyboards")
    ("kbd" "keybinding")
    ("env" "environment")
    ("v" "variable")
    ("vars" "variables")
    ("ev" "environment variable")
    ("evs" "environment variables")
    ("egn" "ergonomic")
    ("fp" "functional programing")
    ("fl" "functional language")
    ("ca" "cellular automata")
    ("fu" "function")
    ("pm" "parameter")
    ("prog" "programing")
    ("db" "database")
    ("oop" "object oriented programing")

    ;; emacs lisp
    ("8date" "\\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\)")
    ("8d" "\\([0-9]+\\)")
    ("8dot" "\\(.\\)")
    ("8str" "\\([^\"]+?\\)")
    ("8curly" "“\\([^”]+?\\)”")
    ("8bracket" "\\[\\([^]]+?\\)\\]")
    ("8tag" "\\([</>=\" A-Za-z0-9]+\\)")
    ("8az" "\\([A-Za-z0-9]+\\)")

    ("8l" "────────── ────────── ────────── ────────── ──────────")
    ("utf8" "-*- coding: utf-8 -*-")
    ("ascii" "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")

    ("php" "<?php\n\n?>")

    
;;; unix shell

  ; rsync -r -v -t --chmod=Dugo+x --chmod=ugo+r --delete --exclude="**/My *" --rsh="ssh -l xah" /media/HP/Users/xah/Documents /media/HP/Users/xah/web /media/HP/Users/xah/Pictures /media/HP/Users/xah/Shared /media/HP/Users/xah/cinse_pixra3 xah@192.168.1.6:~/

  ; rsync -r -v -t --delete --rsh="ssh -l xah" ~/web/ xah@169.254.153.147:~/web/

  ; rsync -r -v -t --delete --exclude="**/My *" --rsh="ssh -l xah" ~/Documents/ xah@169.254.153.147:~/Documents/

  ; unison -servercmd /usr/bin/unison c:/Users/xah/web ssh://xah@169.254.145.104//Users/xah/web
  ; sftp u40651120@s168753655.onlinehome.us
  ;

    
;;; xah personal
    ("ee" "ErgoEmacs")
    ("em" "ergoemacs-mode")
    ("vdspc" "Visual Dictionary of Special Plane Curves")
    ("xl" "Xah Lee")
    ("xim" "Twitter: @xah_lee
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
    ("xs" " Xah Lee
 xahlee@gmail.com
 408 470 0213
 http://xahlee.org/
 US citizen.
 Mountain View, CA.")

    ("wec" "wordyenglish_com")

    ("uwe" "http://wordyenglish.com/")
    ("uxa" "http://xaharts.org/")
    ("uxl" "http://xahlee.org/")
    ("uxli" "http://xahlee.info/")
    ("uxm" "http://xahmusic.org/")
    ("uxp" "http://xahporn.org/")
    ("uxsl" "http://xahsl.org/")
    ("uunicode" "http://xahlee.info/comp/unicode_index.html")

    ("utek" "http://xahlee.info/kbd/Truly_Ergonomic_keyboard.html")
    ("uergodox" "http://xahlee.info/kbd/ergodox_keyboard.html")
    ("umaltron" "http://xahlee.info/kbd/Maltron_keyboard.html")
    ("ukinesis" "http://xahlee.info/kbd/keyboard_Kinesis.html")
    ("uutron" "http://xahlee.info/kbd/uTRON_keyboard.html")

    ("uvmm" "http://VirtualMathMuseum.org/")
    ("u3dxm" "http://3D-XplorMath.org/")

    ("uvi" "http://ergoemacs.org/emacs/emergency_vi.html")
    ("uemacs" "http://ergoemacs.org/emacs/emacs.html")
    ("uelisp" "http://ergoemacs.org/emacs/elisp.html")
    ("uahk" "http://xahlee.info/mswin/autohotkey.html")
    ("ueek" "http://ergoemacs.org/emacs/ergonomic_emacs_keybinding.html")
    ("uhtml" "http://xahlee.info/js/html_index.html")
    ("uwd" "http://xahlee.info/js/index.html")
    ("uem" "http://ergoemacs.github.io/ergoemacs-mode/")

    ("ulang" "<a href=\"../java-a-day/java.html\">Java</a>
<a href=\"../python/python3_basics.html\">Python</a>
<a href=\"../ruby/ruby_basics.html\">Ruby</a>
<a href=\"../perl-python/perl_basics.html\">Perl</a>
<a href=\"../php/php_basics.html\">PHP</a>
<a href=\"http://ergoemacs.org/emacs/elisp.html\">Emacs Lisp</a>")

    ("uapache" "http://xahlee.info/linux/apache_tutorial.html")
    ("uperl" "http://xahlee.info/perl-python/perl_basics.html")
    ("upython" "http://xahlee.info/python/python3_basics.html")
    ("uruby" "http://xahlee.info/ruby/ruby_basics.html")
    ("ujs" "http://xahlee.info/js/javascript_basics.html")
    ("uphp" "http://xahlee.info/php/php_basics.html")
    ("ucss" "http://xahlee.info/js/css_index.html")
    ("udvorak" "http://xahlee.info/comp/dvorak_keyboard_layout.html")

    ("ute" "http://xahlee.info/kbd/Truly_Ergonomic_keyboard.html")

    ("uocaml" "http://xahlee.info/ocaml/ocaml.html")
    ("ups" "http://xahlee.info/powershell/index.html")
    ("umma" "http://xahlee.info/M/index.html")
    ("ujava" "http://xahlee.info/java-a-day/java.html")
    ("utg" "http://xahlee.info/UnixResource_dir/writ/tech_geeker.html")))

;; 
;; ;;; stop asking whether to save newly added abbrev when quitting emacs
(setq save-abbrevs nil)

