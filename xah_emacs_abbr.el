;; -*- coding: utf-8 -*-
;; emacs abbrev def
;; Xah Lee
;; 2007-06, …, 2013-01-13
;; ∑ http://xahlee.org/

; load my abbreviations file
;(read-abbrev-file "emacs_abbrev")


(clear-abbrev-table global-abbrev-table)

(define-abbrev-table 'global-abbrev-table 
  '(

    ("eg" "⁖" nil :system t)
    ("mac" "Mac" nil :system t)
    ("osx" "OS X" nil :system t)
    ("macosx" "Mac OS X" nil :system t)
    ("ipv4" "IPv4" nil :system t)
    ("ipv6" "IPv6" nil :system t)
    ("msw" "Microsoft Windows" nil :system t)
    ("ms" "Microsoft" nil :system t)
    ("ex" "example" nil :system t)
    ("fex" "for example" nil :system t)
    ("jq" "jQuery" nil :system t)


    ("arrows" "➵ ➙ ► ▻ ☛ ☞ 👆 👇 👈 👉 ⇰ ➛ ➜ ➝ ➞ ➟ ➠ ➢ ➣ ➤ ➥ ➦ ➧ ➨ ➲ ➺ ➻ ➼ ➽ ➾" nil :system t)

    ("hearts" "♥ 💕 💓 💔 💖 💗 💘 💝 💞 💟 💙 💚 💛 💜" nil :system t)

    ("rsi" "Repetitive Strain Injury" nil :system t)

    ;; ;    ("t" "the")                         ; problem with “doesn't ”
    ;; ;    ("2" "to")                          ; problem with just enter a number
    ;;     ("o" "of")
    ;;     ("n" "and")
    ;;     ("ta" "that")
    ;;     ("h" "have")
    ;; ;    ("4" "for")
    ;;     ("u" "you")
    ;;     ("w" "with")
    ;;     ("b" "but")

    ("comm" "communication" nil :system t)

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

    ("trad" "traditional" nil :system t)
    ("addr" "address" nil :system t)
    ("dict" "dictionary" nil :system t)
    ("dicts" "dictionaries" nil :system t)
    ("desc" "description" nil :system t)
    ("ty" "thank you" nil :system t)
    ("btw" "by the way" nil :system t)
    ("afaik" "As far as i know" nil :system t)
    ("intro" "introduction" nil :system t)
    ("8b" "because" nil :system t)
    ("pls" "please" nil :system t)
    ("atm" "at the moment" nil :system t)
    ("q" "question" nil :system t)
    ("i18n" "international" nil :system t)
    ("org" "organization" nil :system t)
    ("ia" "interactive" nil :system t)
    ("ann" "announcement" nil :system t)
    ("ref" "reference" nil :system t)
    ("temp" "temperature" nil :system t)
    ("gvn" "government" nil :system t)
    ("auto" "automatically" nil :system t)
    ("math" "mathematics" nil :system t)
    ("diff" "difference" nil :system t)
    ("comp" "computer" nil :system t)
    ("l" "language" nil :system t)
    ("ls" "languages" nil :system t)
    ("exp" "experience" nil :system t)
    ("tech" "technology" nil :system t)
    ("cs" "computer science" nil :system t)
    ("ai" "artificial intelligence" nil :system t)
    ("fs" "fullscreen" nil :system t)

    ("ch" "Chinese" nil :system t)
    ("eng" "English" nil :system t)
    ("jp" "Japanese" nil :system t)

    ;; tech
    ("ipa" "IP address" nil :system t)
    ("wp" "Wikipedia" nil :system t)
    ("ms" "Microsoft" nil :system t)
    ("qt" "QuickTime" nil :system t)
    ("8it" "IntelliType" nil :system t)
    ("win" "window" nil :system t)
    ("ie" "Internet Explorer" nil :system t)
    ("yt" "YouTube" nil :system t)
    ("g" "Google" nil :system t)
    ("ge" "Google Earth" nil :system t)
    ("gh" "Google Hangout" nil :system t)
    ("gm" "Google Map" nil :system t)
    ("gc" "Google Chrome" nil :system t)
    ("gp" "Google Plus" nil :system t)
    ("8so" "StackOverflow" nil :system t)
    ("gcd" "googlecode" nil :system t)
    ("ff" "Firefox" nil :system t)
    ("sl" "Second Life" nil :system t)
    ("bb" "Backbone" nil :system t)
    ("lsl" "Linden Scripting Language" nil :system t)
    ("8ll" "Linden Labs" nil :system t)
    ("fb" "Facebook" nil :system t)
    ("ahk" "AutoHotkey" nil :system t)
    ("pr" "POV-Ray" nil :system t)
    ("ps" "PowerShell" nil :system t)
    ("mma" "Mathematica" nil :system t)
    ("wl" "Wolfram Language" nil :system t)
    ("cl" "Common Lisp" nil :system t)
    ("8e" "emacs" nil :system t)
    ("js" "JavaScript" nil :system t)
    ("vb" "Visual Basic" nil :system t)
    ("pp" "PayPal" nil :system t)
    ("ahd" "American Heritage Dictionary" nil :system t)
    ("wm" "Window Manager" nil :system t)
    ("el" "emacs lisp" nil :system t)
    ("os" "Operating System" nil :system t)

    ;; for programers
    ("hex" "hexadecimal" nil :system t)
    ("ui" "user interface" nil :system t)
    ("gui" "graphical user interface" nil :system t)
    ("alt" "alternative" nil :system t)
    ("dev" "develop" nil :system t)
    ("p" "program" nil :system t)
    ("pl" "programing language" nil :system t)
    ("paren" "parenthesis" nil :system t)
    ("dir" "directory" nil :system t)
    ("dirs" "directories" nil :system t)
    ("subdir" "sub-directory" nil :system t)
    ("subdirs" "sub-directories" nil :system t)
    ("doc" "documentation" nil :system t)
    ("dl" "download" nil :system t)
    ("char" "character" nil :system t)
    ("chars" "characters" nil :system t)
    ("def" "definition" nil :system t)
    ("bg" "background" nil :system t)
    ("kw" "keyword" nil :system t)
    ("kb" "keyboard" nil :system t)
    ("kbs" "keyboards" nil :system t)
    ("kbd" "keybinding" nil :system t)
    ("env" "environment" nil :system t)
    ("v" "variable" nil :system t)
    ("vars" "variables" nil :system t)
    ("ev" "environment variable" nil :system t)
    ("evs" "environment variables" nil :system t)
    ("egn" "ergonomic" nil :system t)
    ("fp" "functional programing" nil :system t)
    ("fl" "functional language" nil :system t)
    ("ca" "cellular automata" nil :system t)
    ("f" "function" nil :system t)
    ("pm" "parameter" nil :system t)
    ("prog" "programing" nil :system t)
    ("db" "database" nil :system t)
    ("oop" "object oriented programing" nil :system t)

    ;; emacs lisp
    ("8date" "\\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\)" nil :system t)
    ("8d" "\\([0-9]+\\)" nil :system t)
    ("8dot" "\\(.\\)" nil :system t)
    ("8str" "\\([^\"]+?\\)" nil :system t)
    ("8curly" "“\\([^”]+?\\)”" nil :system t)
    ("8bracket" "\\[\\([^]]+?\\)\\]" nil :system t)
    ("8tag" "\\([</>=\" A-Za-z0-9]+\\)" nil :system t)
    ("8az" "\\([A-Za-z0-9]+\\)" nil :system t)

    ("tla" "<div class=\"¤tla\"><a href=\"url\">text</a></div>" nil :system t)
    ("8menu" "〖a ▸ b ▸ c〗" nil :system t)
    ("8key" "【Alt+f】" nil :system t)
    ("8song" "singer ❀ 〈title〉" nil :system t)

    ("li" "────────── ────────── ────────── ────────── ──────────" nil :system t)
    ("u" "-*- coding: utf-8 -*-" nil :system t)
    ("ascii" "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~" nil :system t)

    ("php" "<?php\n\n?>" nil :system t)

    ("faq" "<div class=\"a\">
<p class=\"q\">How to do this?</p>
<p>this way</p>
</div>

" nil :system t)
    ("css" "<link rel=\"stylesheet\" href=\"../../lbasic.css\" />" nil :system t)
    ("css2" "<style type=\"text/css\">\np {line-height:130%}\n</style>" nil :system t)

    
;;; unix shell

  ; rsync -r -v -t --chmod=Dugo+x --chmod=ugo+r --delete --exclude="**/My *" --rsh="ssh -l xah" /media/HP/Users/xah/Documents /media/HP/Users/xah/web /media/HP/Users/xah/Pictures /media/HP/Users/xah/Shared /media/HP/Users/xah/cinse_pixra3 xah@192.168.1.6:~/

  ; rsync -r -v -t --delete --rsh="ssh -l xah" ~/web/ xah@169.254.153.147:~/web/

  ; rsync -r -v -t --delete --exclude="**/My *" --rsh="ssh -l xah" ~/Documents/ xah@169.254.153.147:~/Documents/

  ; unison -servercmd /usr/bin/unison c:/Users/xah/web ssh://xah@169.254.145.104//Users/xah/web
  ; sftp u40651120@s168753655.onlinehome.us
  ;

    
;;; xah personal
    ("ee" "ErgoEmacs" nil :system t)
    ("em" "ergoemacs-mode" nil :system t)
    ("vdspc" "Visual Dictionary of Special Plane Curves" nil :system t)
    ("xl" "Xah Lee" nil :system t)
    ("xim" "Twitter: @xah_lee
Facebook: https://www.facebook.com/xahlee
g+: https://plus.google.com/112757647855302148298/posts
Google talk: xahlee@gmail.com
Skype: XahLee
AIM: xahlee
Yahoo: P0lyglut
MSN: p0lyglut@yahoo.com or xahlee
Second Life: Xah Toll
" nil :system t)

    ;; xah url
    ("xs" " Xah Lee
 xahlee@gmail.com
 408 470 0213
 http://xahlee.org/
 US citizen.
 Mountain View, CA." nil :system t)

    ("wec" "wordyenglish_com" nil :system t)

    ("uwe" "http://wordyenglish.com/" nil :system t)
    ("uxa" "http://xaharts.org/" nil :system t)
    ("uxl" "http://xahlee.org/" nil :system t)
    ("uxli" "http://xahlee.info/" nil :system t)
    ("uxm" "http://xahmusic.org/" nil :system t)
    ("uxp" "http://xahporn.org/" nil :system t)
    ("uxsl" "http://xahsl.org/" nil :system t)
    ("uunicode" "http://xahlee.info/comp/unicode_index.html" nil :system t)

    ("utek" "http://xahlee.info/kbd/Truly_Ergonomic_keyboard.html" nil :system t)
    ("uergodox" "http://xahlee.info/kbd/ergodox_keyboard.html" nil :system t)
    ("umaltron" "http://xahlee.info/kbd/Maltron_keyboard.html" nil :system t)
    ("ukinesis" "http://xahlee.info/kbd/keyboard_Kinesis.html" nil :system t)
    ("uutron" "http://xahlee.info/kbd/uTRON_keyboard.html" nil :system t)

    ("uvmm" "http://VirtualMathMuseum.org/" nil :system t)
    ("u3dxm" "http://3D-XplorMath.org/" nil :system t)

    ("uvi" "http://ergoemacs.org/emacs/emergency_vi.html" nil :system t)
    ("uemacs" "http://ergoemacs.org/emacs/emacs.html" nil :system t)
    ("uelisp" "http://ergoemacs.org/emacs/elisp.html" nil :system t)
    ("uahk" "http://xahlee.info/mswin/autohotkey.html" nil :system t)
    ("ueek" "http://ergoemacs.org/emacs/ergonomic_emacs_keybinding.html" nil :system t)
    ("uhtml" "http://xahlee.info/js/index.html" nil :system t)
    ("uem" "http://ergoemacs.github.io/ergoemacs-mode/" nil :system t)

    ("ulang" "<a href=\"../java-a-day/java.html\">Java</a>
<a href=\"../python/python3_basics.html\">Python</a>
<a href=\"../ruby/ruby_basics.html\">Ruby</a>
<a href=\"../perl-python/perl_basics.html\">Perl</a>
<a href=\"../php/php_basics.html\">PHP</a>
<a href=\"http://ergoemacs.org/emacs/elisp.html\">Emacs Lisp</a>" nil :system t)

    ("uapache" "http://xahlee.info/linux/apache_tutorial.html" nil :system t)
    ("uperl" "http://xahlee.info/perl-python/perl_basics.html" nil :system t)
    ("upython" "http://xahlee.info/python/python3_basics.html" nil :system t)
    ("uruby" "http://xahlee.info/ruby/ruby_basics.html" nil :system t)
    ("ujs" "http://xahlee.info/js/javascript_basics.html" nil :system t)
    ("uphp" "http://xahlee.info/php/php_basics.html" nil :system t)
    ("ucss" "http://xahlee.info/js/css_index.html" nil :system t)
    ("udvorak" "http://xahlee.info/comp/dvorak_keyboard_layout.html" nil :system t)

    ("ute" "http://xahlee.info/kbd/Truly_Ergonomic_keyboard.html" nil :system t)

    ("uocaml" "http://xahlee.info/ocaml/ocaml.html" nil :system t)
    ("ups" "http://xahlee.info/powershell/index.html" nil :system t)
    ("umma" "http://xahlee.info/M/index.html" nil :system t)
    ("ujava" "http://xahlee.info/java-a-day/java.html" nil :system t)
    ("utg" "http://xahlee.info/UnixResource_dir/writ/tech_geeker.html" nil :system t)

    ))

;; 
;; ;;; stop asking whether to save newly added abbrev when quitting emacs
(setq save-abbrevs nil)

