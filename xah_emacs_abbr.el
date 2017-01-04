;; -*- coding: utf-8 -*-
;; emacs abbrev def
;; 〈Using Emacs Abbrev Mode for Abbreviation〉 http://ergoemacs.org/emacs/emacs_abbrev_mode.html

; load my abbreviations file
;(read-abbrev-file "emacs_abbrev")



(defun xah-abbrev-h-f ()
  "Abbrev hook function, used for `define-abbrev'.
 Our use is to prevent inserting the char that triggered expansion. Experimental.
 the “h-f” stand for hook function.
Version 2016-10-24"
  t)

(put 'xah-abbrev-h-f 'no-self-insert t)

(clear-abbrev-table global-abbrev-table)

(define-abbrev-table 'global-abbrev-table
  '(

    ;; phrase
    ("afaik" "As far as i know" )
    ("atm" "at the moment" )
    ("dfb" "difference between" )
    ("ty" "thank you" )

    ("ai" "artificial intelligence" )
    ("btw" "by the way" )

    ("cnt" "can't" )
    ("ddnt" "didn't" )
    ("dnt" "don't" )
    ("dsnt" "doesn't" )
    ("dvp" "develop" )
    ("eg" "e.g." )
    ("hnt" "haven't" )
    ("isnt" "isn't" )
    ("shnt" "shouldn't" )
    ("wnt" "won't" )
    ("wsnt" "wasn't" )
    ("wtdb" "What's the difference between" )
    ("sa" "See also:" )

    ;; english word abbrev

    ("ann" "announcement" )
    ("autom" "automatic" )
    ("bc" "because" )
    ("bg" "background" )
    ("bt" "between" )
    ("ch" "Chinese" )
    ("comm" "communication" )
    ("comp" "computer" )
    ("desc" "description" )
    ("zdev" "development" )
    ("dict" "dictionary" )
    ("dicts" "dictionaries" )
    ("dir" "directory" )
    ("dirs" "directories" )
    ("disc" "discussion" )
    ("dl" "download" )
    ("eng" "English" )
    ("env" "environment" )
    ("esp" "especially" )
    ("ex" "example" )
    ("fex" "for example," )
    ("fu" "function" )
    ("arg" "argument" )
    ("gvn" "government" )
    ("hex" "hexadecimal" )
    ("ia" "interactive" )
    ("impl" "implementation" )
    ("implt" "implement" )
    ("intn" "international" )
    ("intro" "introduction" )
    ("jp" "Japanese" )
    ("kb" "keyboard" )
    ("kbd" "keybinding" )
    ("kbs" "keyboards" )
    ("kw" "keyword" )
    ("ob" "object" )
    ("paren" "parenthesis" )
    ("pl" "programing language" )
    ("pls" "please" )
    ("prof" "professor" )
    ("ref" "reference" )
    ("scs" "screenshot" )
    ("tb" "trackball" )
    ("techn" "technology" )
    ("trad" "traditional" )
    ("ver" "version" )
    ("vid" "video" )
    ("win" "window" )
    ("wp" "Wikipedia" )
    ("zalt" "alternative" )
    ("zauto" "automatic" )
    ("zc" "character" )
    ("zchars" "characters" )
    ("zergo" "ergonomic" )
    ("zexp" "experience" )
    ("zexpr" "expression" )
    ("zl" "language" )
    ("zorg" "organization" )
    ("zp" "program" )
    ("zpm" "parameter" )
    ("zprog" "programing" )
    ("zq" "question" )
    ("zs" "string" )

    ;; computing
    ("ahd" "American Heritage Dictionary" )
    ("ahk" "AutoHotkey" )
    ("cfg" "context-free grammar" )
    ("cj" "Clojure" )
    ("cl" "Common Lisp" )
    ("cs" "computer science" )
    ("ee" "ErgoEmacs" )

    ("ev" "environment variable" )
    ("evs" "environment variables" )
    ("faq" "frequently asked questions" )
    ("fb" "Facebook" )
    ("fs" "fullscreen" )
    ("gc" "Google Chrome" )
    ("gcd" "googlecode" )
    ("ge" "Google Earth" )
    ("gh" "Google Hangout" )
    ("gm" "Google Map" )
    ("gp" "Google Plus" )
    ("ie" "Internet Explorer" )
    ("ipa" "IP address" )
    ("ipv4" "IPv4" )
    ("ipv6" "IPv6" )
    ("jq" "jQuery" )
    ("jvm" "Java Virtual Machine" )
    ("lsl" "Linden Scripting Language" )
    ("mac" "Mac" )
    ("macosx" "Mac OS X" )
    ("mma" "Mathematica" )
    ("ms" "Microsoft" )
    ("msvs" "Microsoft Visual Studio" )
    ("msw" "Microsoft Windows" )

    ("osx" "OS X" )
    ("pp" "PayPal" )
    ("rsi" "Repetitive Strain Injury" )
    ("sf" "San Francisco" )
    ("sl" "Second Life" )
    ("subdir" "sub-directory" )
    ("subdirs" "sub-directories" )

    ("vb" "Visual Basic" )
    ("wd" "web development" )
    ("wl" "Wolfram Language" )
    ("wm" "Window Manager" )

    ("yt" "YouTube" )

    ("zca" "cellular automata" )
    ("zcli" "command line interface" )

    ("zdb" "database" )
    ("zdef" "definition" )
    ("zdf" "different" )
    ("zdfc" "difference" )
    ("zdoc" "documentation" )
    ("zdt" "data type" )
    ("zel" "emacs lisp" )

    ("zff" "Firefox" )
    ("zfl" "functional language" )
    ("zfp" "functional programing" )
    ("zg" "Google" )
    ("zgui" "graphical user interface" )
    ("zis" "image source" )
    ("zit" "IntelliType" )
    ("zjs" "JavaScript" )
    ("zlhs" "left-hand-side" )
    ("zlib" "library" )
    ("zlibs" "libraries" )
    ("zmath" "mathematics" )
    ("zmd" "metadata" )
    ("zoop" "object oriented programing" )
    ("zos" "operating system" )

    ("zps" "PowerShell" )
    ("zrhs" "right-hand-side" )
    ("zsc" "source code" )
    ("zsjw" "social justice warriors" )
    ("zso" "StackOverflow" )

    ("ztek" "Truly Ergonomic keyboard" )
    ("ztemp" "temperature" )
    ("zts" "TypeScript" )
    ("va" "variable" )
    ("vas" "variables" )

    ;; programing
    ("eq" "==" )
    ("eqq" "===" )

    ;; regex
    ("zaz" "\\([A-Za-z0-9]+\\)" )
    ("zbracket" "\\[\\([^]]+?\\)\\]" )
    ("zcurly" "“\\([^”]+?\\)”" )
    ("zd" "\\([0-9]+\\)" )
    ("zdate" "\\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\)" )
    ("zdot" "\\(.\\)" )
    ("zstr" "\\([^\"]+?\\)" )
    ("ztag" "\\([</>=\" A-Za-z0-9]+\\)" )

    ;; html
    ("zkey" "【Alt+f】" )
    ("zmenu" "〖a ▸ b ▸ c〗" )
    ("zfaq" "<div class=\"question-box32371\">
<p class=\"q\">How to do this?</p>
<p>this way</p>
</div>

" )

    ;; unicode
    ("md" "—" )
    ("zarrows" "➵➙►▻☛☞👉⇰➛➜➝➞➟➠➢➣➤➥➦➧➨➲➺➻➼➽➾" )
    ("zascii" "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~" )
    ("zbox" "┌─┬─┐
│ │ │
├─┼─┤
│ │ │
└─┴─┘" )
    ("bu" "•" )
    ("catface" "😸" )
    ("emoji" "😃😄😅😆
😉😊😋😌
😎😇😈
😁😂😀
😚😘😍😙😗
😛😜😝
😩😫😪😴
😏😓😔😖😐😑😕
😮😯😰😱😲😳😵😶😷
😒😠😡
😞😟😣😤😥😦😧😨😬
😢😭
😸😹😺😻😼😽😾😿🙀" )
    ("hearts" "♥💕💓💔💖💗💘💝💞💟💙💚💛💜" )
    ("omg" "😂" )
    ("la" "←" )
    ("ua" "↑" )
    ("ra" "→" )
    ("da" "↓" )

    ;; code
    ("zutf8" "-*- coding: utf-8 -*-" )

    ("vdspc" "Visual Dictionary of Special Plane Curves" )
    ("xfk" "xah-fly-keys" )
    ("eem" "ergoemacs-mode" )

    ;; url
    ("u3dxm" "http://3D-XplorMath.org/" )
    ("uahk" "http://xahlee.info/mswin/autohotkey.html" )
    ("uapache" "http://xahlee.info/linux/apache_tutorial.html" )
    ("ucss" "http://xahlee.info/js/css_index.html" )
    ("udvorak" "http://xahlee.info/comp/dvorak_keyboard_layout.html" )
    ("ueek" "http://ergoemacs.org/emacs/ergonomic_emacs_keybinding.html" )
    ("uelisp" "http://ergoemacs.org/emacs/elisp.html" )
    ("uem" "http://ergoemacs.github.io/" )
    ("uemacs" "http://ergoemacs.org/emacs/emacs.html" )
    ("uergodox" "http://xahlee.info/kbd/ergodox_keyboard.html" )
    ("uhtml" "http://xahlee.info/js/html_index.html" )
    ("ui" "user interface" )
    ("ujava" "http://xahlee.info/java-a-day/java.html" )
    ("ujs" "http://xahlee.info/js/javascript_basics.html" )
    ("ukinesis" "http://xahlee.info/kbd/keyboard_Kinesis.html" )
    ("ulang" "<a href=\"../java-a-day/java.html\">Java</a>
<a href=\"../python/python3_basics.html\">Python</a>
<a href=\"../ruby/ruby_basics.html\">Ruby</a>
<a href=\"../perl-python/perl_basics.html\">Perl</a>
<a href=\"../php/php_basics.html\">PHP</a>
<a href=\"http://ergoemacs.org/emacs/elisp.html\">Emacs Lisp</a>" )
    ("umaltron" "http://xahlee.info/kbd/Maltron_keyboard.html" )
    ("umma" "http://xahlee.info/M/index.html" )
    ("uns" "understand" )
    ("uocaml" "http://xahlee.info/ocaml/ocaml.html" )
    ("uperl" "http://xahlee.info/perl-python/perl_basics.html" )
    ("uphp" "http://xahlee.info/php/php_basics.html" )
    ("ups" "http://xahlee.info/powershell/index.html" )
    ("upython" "http://xahlee.info/python/python3_basics.html" )
    ("ur" "you are" )
    ("uruby" "http://xahlee.info/ruby/ruby_basics.html" )
    ("ute" "http://xahlee.info/kbd/Truly_Ergonomic_keyboard.html" )
    ("utek" "http://xahlee.info/kbd/Truly_Ergonomic_keyboard.html" )
    ("utg" "http://xahlee.info/UnixResource_dir/writ/tech_geeker.html" )
    ("uunicode" "http://xahlee.info/comp/unicode_index.html" )
    ("uutron" "http://xahlee.info/kbd/uTRON_keyboard.html" )
    ("uvi" "http://ergoemacs.org/emacs/emergency_vi.html" )
    ("uvmm" "http://VirtualMathMuseum.org/" )
    ("uwd" "http://xahlee.info/js/index.html" )
    ("uwe" "http://wordyenglish.com/" )
    ("uxa" "http://xaharts.org/" )
    ("uxl" "http://xahlee.org/" )
    ("uxli" "http://xahlee.info/" )
    ("uxm" "http://xahmusic.org/" )
    ("uxp" "http://xahporn.org/" )
    ("uxsl" "http://xahsl.org/" )

    ;; xah lee
    ("xim" "Twitter: @xah_lee
Facebook: https://www.facebook.com/xahlee
g+: https://plus.google.com/112757647855302148298/posts
Google talk: xahlee@gmail.com
Skype: XahLee
AIM: xahlee
Yahoo: P0lyglut
MSN: p0lyglut@yahoo.com or xahlee
Second Life: Xah Toll
QQ: http://user.qzone.qq.com/2609322939" )
    ("xl" "Xah Lee" )
    ("xs" " Xah Lee
 xahlee@gmail.com
 http://xahlee.org/
 US citizen.
 Mountain View, CA." )

    ;;

    ))

(set-default 'abbrev-mode t)

(setq save-abbrevs nil)
