;; -*- coding: utf-8; lexical-binding: t; -*-
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

    ;; shortcuts
    ("afaik" "as far as i know" )
    ("atm" "at the moment" )
    ("dfb" "difference between" )
    ("ty" "thank you" )

    ("ai" "artificial intelligence" )
    ("btw" "by the way" )
    ("ur" "you are" )
    ("ull" "you'll" )
    ("cnt" "can't" )
    ("ddnt" "didn't" )
    ("dnt" "don't" )
    ("dsnt" "doesn't" )
    ("dvp" "develop" )
    ("eg" "e.g." )
    ("ie" "i.e." )
    ("hnt" "haven't" )
    ("isnt" "isn't" )
    ("shnt" "shouldn't" )
    ("wnt" "won't" )
    ("wsnt" "wasn't" )
    ("wtdb" "What's the difference between" )
    ("sa" "See also:" )
    ("ure" "return" )

    ("upl" "+" )

    ;; english
    ("ann" "announcement" )
    ("maint" "maintenance" )
    ("mtn" "mountain" )
    ("qaa" "questions and answers" )
    ("cvnt" "convenient" )
    ("arg" "argument" )
    ("autom" "automatic" )
    ("bc" "because" )
    ("bg" "background" )
    ("bt" "between" )
    ("ch" "Chinese" )
    ("comm" "communication" )
    ("comp" "computer" )
    ("desc" "description" )
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
    ("gvn" "government" )
    ("hex" "hexadecimal" )
    ("ia" "interactive" )
    ("impl" "implementation" )
    ("implt" "implement" )
    ("intn" "international" )
    ("intro" "introduction" )
    ("jp" "Japanese" )
    ("sw" "software" )
    ("kb" "keyboard" )
    ("kp" "keypad" )
    ("kbd" "keybinding" )
    ("kbs" "keyboards" )
    ("kw" "keyword" )
    ("ob" "object" )
    ("oc" "occur" )
    ("occ" "occurrence" )
    ("paren" "parenthesis" )
    ("pl" "programing language" )
    ("pls" "please" )
    ("prog" "program" )
    ("prof" "professor" )
    ("ref" "reference" )
    ("scs" "screenshot" )
    ("tb" "trackball" )
    ("techn" "technology" )
    ("trad" "traditional" )
    ("ver" "version" )
    ("vid" "video" )
    ("wp" "Wikipedia" )
    ("ualt" "alternative" )
    ("uauto" "automatic" )
    ("uc" "character" )
    ("uchars" "characters" )
    ("udev" "development" )
    ("uergo" "ergonomic" )
    ("uexp" "experience" )
    ("uexpr" "expression" )
    ("ul" "language" )
    ("uorg" "organization" )
    ("upm" "parameter" )
    ("uprog" "programing" )
    ("uq" "question" )
    ("st" "string" )

    ;; tech company
    ("gc" "Google Chrome" )
    ("lsl" "Linden Scripting Language" )
    ("mac" "Mac" )
    ("macosx" "Mac OS X" )
    ("mma" "Mathematica" )
    ("ms" "Microsoft" )
    ("msvs" "Microsoft Visual Studio" )
    ("msw" "Microsoft Windows" )
    ("osx" "OS X" )
    ("pp" "PayPal" )
    ("sl" "Second Life" )
    ("fb" "Facebook" )
    ("wl" "Wolfram Language" )
    ("yt" "YouTube" )
    ("uff" "Firefox" )
    ("uso" "StackOverflow" )

    ;; computing
    ("ahd" "American Heritage Dictionary" )
    ("ahk" "AutoHotkey" )
    ("cfg" "context-free grammar" )
    ("cj" "Clojure" )
    ("cl" "Common Lisp" )
    ("cs" "computer science" )
    ("ee" "ergoemacs" )

    ("ev" "environment variable" )
    ("evs" "environment variables" )
    ("faq" "frequently asked questions" )
    ("fs" "fullscreen" )

    ("ipa" "IP address" )
    ("jq" "jQuery" )
    ("jvm" "Java Virtual Machine" )

    ("rsi" "Repetitive Strain Injury" )
    ("sf" "San Francisco" )

    ("subdir" "sub-directory" )
    ("subdirs" "sub-directories" )

    ("vb" "Visual Basic" )
    ("wd" "web development" )
    ("wm" "Window Manager" )

    ("uca" "cellular automata" )
    ("ucli" "command line interface" )

    ("udb" "database" )
    ("udef" "definition" )
    ("udf" "different" )
    ("udfc" "difference" )
    ("udoc" "documentation" )
    ("udt" "data type" )
    ("uel" "emacs lisp" )

    ("ufl" "functional language" )
    ("ufp" "functional programing" )
    ("ugui" "graphical user interface" )
    ("uis" "image source" )
    ("ujs" "JavaScript" )
    ("ulhs" "left-hand-side" )
    ("ulib" "library" )
    ("ulibs" "libraries" )
    ("umath" "mathematics" )
    ("umd" "metadata" )
    ("uoop" "object oriented programing" )
    ("uos" "operating system" )

    ("urhs" "right-hand-side" )
    ("usc" "source code" )
    ("usjw" "social justice warriors" )

    ("utek" "Truly Ergonomic keyboard" )
    ("utemp" "temperature" )
    ("uts" "TypeScript" )
    ("va" "variable" )
    ("nn" "non-nil" )
    ("vas" "variables" )

    ;; programing
    ("eq" "==" )
    ("eqq" "===" )

    ;; ("twittercard" "<meta name=\"twitter:image\" content=\"http://example.com/cat.jpg\">" )

    ;; regex
    ("uaz" "\\([A-Za-z0-9]+\\)" )
    ("ubracket" "\\[\\([^]]+?\\)\\]" )
    ("ucurly" "“\\([^”]+?\\)”" )
    ("ud" "\\([0-9]+\\)" )
    ;; ("udate" "\\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\)" )
    ("udate" "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" )
    ("udot" "\\(.\\)" )
    ("ustr" "\\([^\"]+?\\)" )
    ("utag" "\\([</>=\" A-Za-z0-9]+\\)" )

    ;; html
    ("ukey" "【Alt+f】" )
    ("pmoved" "<p class=\"page_moved_64598\">page moved to <a href=\"x.html\">x</a></p>" )
    ("umenu" "〖a ▸ b ▸ c〗" )
    ("ufaq" "<div class=\"question-box32371\">
<p class=\"q\">How to do this?</p>
<p>this way</p>
</div>

" )

    ;; unicode
    ("emd" "—" )
    ("uascii" "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~" )
    ("bu" "•" )
    ("catface" "😸" )
    ("uemoji" "😃😄😅😆
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
    ("uutf8" "-*- coding: utf-8 -*-" )

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

    ("xs" " Xah Lee
 xahlee@gmail.com
 http://xahlee.org/
 US citizen.
 Mountain View, CA." )

    ;;

    ))

(set-default 'abbrev-mode t)

(setq save-abbrevs nil)
