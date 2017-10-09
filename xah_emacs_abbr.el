;; -*- coding: utf-8; lexical-binding: t; -*-
;; emacs abbrev def
;; 〈Using Emacs Abbrev Mode for Abbreviation〉 http://ergoemacs.org/emacs/emacs_abbrev_mode.html

; load my abbreviations file
;(read-abbrev-file "emacs_abbrev")



(fset 'vmm
   [?1 ?j ?d ?r ?l ?i ?d ?3 ?  home ?9 ?k ?c ?h ?l ?c ?l ?  backspace ?m ?v ?x ?l])

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

("vmm3" "

f_cycle_image ({
 p_img_path_list: [

\"i/enneper2_cartesian_001.png\",

] ,
 p_img_tag_id: \"id_img_xl\",
 p_backforth_loop: true,
});

")

    ;; English phrases abbrev
    ("afaik" "as far as i know" )
    ("iirc" "if i recall correctly" )

    ("atm" "at the moment" )
    ("dfb" "difference between" )
    ("ty" "thank you" )
    ("btw" "by the way" )
    ("ur" "your" )
    ("ull" "you'll" )
    ("cnt" "can't" )
    ("ddnt" "didn't" )
    ("dnt" "don't" )
    ("dsnt" "doesn't" )
    ("dvp" "develop" )
    ("eg" "e.g." )
    ("ie" "i.e." )
    ("hv" "have" )
    ("hnt" "haven't" )
    ("isnt" "isn't" )
    ("shnt" "shouldn't" )
    ("wnt" "won't" )
    ("wsnt" "wasn't" )
    ("wtdb" "What's the difference between" )
    ("sa" "See also:" )
    ("r3" "return" )
    ("im" "I'm" )
    ("tr" "there are" )
    ("u" "you" )
    ("r" "are" )
    ("sd" "should" )
    ("wt" "want" )
    ("ts" "there is" )

    ;; english, single word

    ("fol" "following" )
    ("hev" "however" )
    ("alt3" "alternative" )
    ("ann" "announcement" )
    ("aprt" "apparently" )
    ("arg" "argument" )
    ("auto3" "automatic" )
    ("autom" "automatic" )
    ("bc" "because" )
    ("bg" "background" )
    ("bt" "between" )
    ("c3" "character" )
    ("chars3" "characters" )
    ("comm" "communication" )
    ("comp" "computer" )
    ("cprs" "comparison" )
    ("cvnt" "convenient" )
    ("def3" "definition" )
    ("desc" "description" )
    ("dev3" "development" )
    ("df3" "different" )
    ("dfc" "difference" )
    ("dict" "dictionary" )
    ("dicts" "dictionaries" )
    ("dir" "directory" )
    ("dirs" "directories" )
    ("disc" "discussion" )
    ("dl" "download" )
    ("doc3" "documentation" )
    ("eff" "efficient" )
    ("eg3" "ergonomic" )
    ("ei" "Put this in your emacs init file:" )
    ("env" "environment" )
    ("esp" "especially" )
    ("ex" "example" )
    ("exp3" "experience" )
    ("expr3" "expression" )
    ("faq" "frequently asked questions" )
    ("fex" "for example," )
    ("fp" "functional programing" )
    ("fu" "function" )
    ("gvn" "government" )
    ("hex" "hexadecimal" )
    ("ia" "interactive" )
    ("im3" "instant message" )
    ("impl" "implementation" )
    ("implt" "implement" )
    ("intn" "international" )
    ("intro" "introduction" )
    ("kb" "keyboard" )
    ("kbg" "keybinding" )
    ("kbs" "keyboards" )
    ("kp" "keypad" )
    ("kw" "keyword" )
    ("l3" "language" )
    ("maint" "maintenance" )
    ("math3" "mathematics" )
    ("mtn" "mountain" )
    ("ob" "object" )
    ("oc" "occur" )
    ("occ" "occurrence" )
    ("org3" "organization" )
    ("paren" "parenthesis" )
    ("pls" "please" )
    ("pm3" "parameter" )
    ("dec3" "declaration" )
    ("pp" "people" )
    ("prof" "professor" )
    ("prog" "program" )
    ("prog3" "programing" )
    ("q3" "question" )
    ("ref" "reference" )
    ("scs" "screenshot" )
    ("st" "string" )
    ("sw" "software" )
    ("tb" "trackball" )
    ("techn" "technology" )
    ("temp3" "temperature" )
    ("trad" "traditional" )
    ("uds" "underscore" )
    ("und" "understand" )
    ("ver" "version" )
    ("vid" "video" )
    ("wo" "without" )

    ;; english, multiple words

    ("mkb" "mechanical keyboard" )
    ("pl" "programing language" )
    ("qaa" "questions and answers" )
    ("sjw3" "social justice warriors" )
    ("wip" "work in progress" )

    ;; english, proper noun

    ("ahd" "American Heritage Dictionary" )
    ("cn" "Chinese" )
    ("eng" "English" )
    ("euro3" "Europe" )
    ("jp" "Japan" )
    ("jp3" "Japanese" )
    ("sf" "San Francisco" )
    ("wp" "Wikipedia" )

    ;; math

    ("sor" "surface of revolution" )
    ("fr" "fundamental region" )
    ("def" ":=" )
    ("dg3" "differential geometry" )
    ("de3" "differential equations" )
    ("dm" "dimension" )

    ;; computing, general

    ("ai" "artificial intelligence" )
    ("gv" "global variable" )
    ("ca3" "cellular automata" )
    ("cfg" "context-free grammar" )
    ("cli" "command line interface" )
    ("cp" "codepoint" )
    ("cs" "computer science" )
    ("db3" "database" )
    ("dt3" "data type" )
    ("ee" "ergoemacs" )
    ("ev" "environment variable" )
    ("evs" "environment variables" )
    ("fl3" "functional language" )
    ("fp3" "functional programing" )
    ("fs" "fullscreen" )
    ("gui3" "graphical user interface" )
    ("ipa" "IP address" )
    ("is3" "image source" )
    ("lhs3" "left-hand-side" )
    ("lib3" "library" )
    ("libs3" "libraries" )
    ("md3" "metadata" )
    ("oop3" "object oriented programing" )
    ("os3" "operating system" )
    ("rhs3" "right-hand-side" )
    ("rsi" "Repetitive Strain Injury" )
    ("sc3" "source code" )
    ("subdir" "sub-directory" )
    ("subdirs" "sub-directories" )
    ("ui3" "user interface" )
    ("va" "variable" )
    ("vas" "variables" )
    ("wd" "web development" )
    ("wm" "Window Manager" )

    ;; computing, proper noun
    ("ahk" "AutoHotkey" )
    ("cj" "Clojure" )
    ("cl" "Common Lisp" )
    ("el3" "emacs lisp" )
    ("fb" "Facebook" )
    ("ff3" "Firefox" )
    ("gc" "Google Chrome" )
    ("ie3" "Internet Explorer" )
    ("jq" "jQuery" )
    ("js3" "JavaScript" )
    ("jvm" "Java Virtual Machine" )
    ("lsl" "Linden Scripting Language" )
    ("mac" "Mac" )
    ("macosx" "Mac OS X" )
    ("macos" "macOS" )
    ("mma" "Mathematica" )
    ("ms" "Microsoft" )
    ("msvs" "Microsoft Visual Studio" )
    ("msw" "Microsoft Windows" )
    ("nn" "non-nil" )
    ("osx" "OS X" )
    ("pp3" "PayPal" )
    ("ps3" "powershell" )
    ("sl" "Second Life" )
    ("so3" "StackOverflow" )
    ("tek3" "Truly Ergonomic keyboard" )
    ("ts3" "TypeScript" )
    ("vb" "Visual Basic" )
    ("wl" "Wolfram Language" )
    ("yt" "YouTube" )

    ;; programing
    ("p3" "+" )
    ("eq" "==" )
    ("eqq" "===" )

    ("byline" "<div class=\"byline\">By Xah Lee. Date: <time>2009-07-30</time>. Last updated: <time>2017-05-09</time>.</div>" )

    ("mx" "<kbd>Alt</kbd>+<kbd>x</kbd>")

    ;; ("twittercard" "<meta name=\"twitter:image\" content=\"http://example.com/cat.jpg\">" )

    ;; regex
    ("az3" "\\([A-Za-z0-9]+\\)" )
    ("bracket3" "\\[\\([^]]+?\\)\\]" )
    ("curly3" "“\\([^”]+?\\)”" )
    ("d3" "\\([0-9]+\\)" )
    ;; ("date3" "\\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\)" )
    ("date3" "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" )
    ("dot3" "\\(.\\)" )
    ("str3" "\\([^\"]+?\\)" )
    ("tag3" "\\([</>=\" A-Za-z0-9]+\\)" )

    ;; html
    ("hkey" "【Alt+f】" )
    ("hpmoved" "<p class=\"page_moved_64598\">page moved to <a href=\"x.html\">x</a></p>" )
    ("htopic" "<div class=\"topic-xl\">
<h4>Keyboard Keys Topic</h4>
</div>

" )
    ("hfaq" "<section class=\"qna_xl\">

<h3 class=\"q\">How to ?</h3>

<p>this</p>

</section>

" )

("topic3" "<div class=\"topic-xl\">
<h4>Emacs Lisp Basics Topic</h4>
<ol>
<li><a href=\"elisp_basics.html\">Emacs Lisp Basics</a></li>
<li><a href=\"elisp_editing_basics.html\">Overview of Text-Processing in Emacs Lisp</a></li>
<li><a href=\"elisp_examples.html\">Emacs Lisp Examples, page 1</a></li>
<li><a href=\"elisp_eval_lisp_code.html\">How to Evaluate Emacs Lisp Code</a></li>
<li><a href=\"elisp_doc_lookup.html\">Elisp: Documentation Lookup</a></li>
<li><a href=\"elisp_search_documentation.html\">Elisp: Search Documentation</a></li>
<li><a href=\"emacs_editing_lisp.html\">How to Edit Lisp Code with Emacs</a></li>
</ol>
</div>

")

    ;; unicode
    ("emd" "—" )
    ("ascii3" "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~" )
    ("bu" "•" )
    ("catface" "😸" )
    ("clown" "🤡" )
    ("emoji3" "😃😄😅😆
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
😸😹😺😻😼😽😾😿🙀
🤡" )
    ("hearts" "♥💕💓💔💖💗💘💝💞💟💙💚💛💜" )
    ("omg" "😂" )
    ("la" "←" )
    ("ua" "↑" )
    ("ra" "→" )
    ("da" "↓" )

    ("fn" "ƒ" )

    ;; code
    ("utf83" "-*- coding: utf-8 -*-" )

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
    ("ujava" "http://xahlee.info/java-a-day/java.html" )
    ("ukinesis" "http://xahlee.info/kbd/keyboard_Kinesis.html" )
    ("ulang" "~/web/xahlee_info/js/javascript_basics.html
~/web/xahlee_info/python/python3_basics.html
~/web/xahlee_info/ruby/ruby_basics.html
~/web/xahlee_info/perl/perl_basics_1.html
~/web/xahlee_info/php/php_basics.html
~/web/ergoemacs_org/emacs/elisp_basics.html
~/web/xahlee_info/java-a-day/java.html
~/web/xahlee_info/clojure/clojure_index.html
"  )
    ("umaltron" "http://xahlee.info/kbd/Maltron_keyboard.html" )
    ("umma" "http://xahlee.info/M/index.html" )
    ("uocaml" "http://xahlee.info/ocaml/ocaml.html" )
    ("uperl" "http://xahlee.info/perl-python/perl_basics.html" )
    ("uphp" "http://xahlee.info/php/php_basics.html" )
    ("uups" "http://xahlee.info/powershell/index.html" )

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

(define-abbrev-table 'go-mode-abbrev-table
  '(
    ("g3" "package main

import \"fmt\"

func main() {

	fmt.Println( 3 )

}")

    ("if" "if x < 0 { 3 }")
    ("ie" " if x < 0 { 3 } else { 4 }")
    ("ei" "else if x > 0 { 3 }")
    ("else" "else { 3 }")
    ("for" "for i := 0; i < 4; i++ { i }")
    ("r" "return")
    ("ps" "+")
    ("eq" "==")
    ("pt" "fmt.Println(3)")
    ("fu" "func(x int) int { return 1 }")
    ("v" "var x = 3")
    ("ft" "fallthrough")
    ("switch" "	switch 3 {
	case 1:
		fmt.Println( 3 )
	case 2, 3:
		fmt.Println( 4 )
	default:
		fmt.Println( 5 )
	}")

    ("rto" "reflect.TypeOf()")

    ("map" "map[string]string")

    ;;

    ))

(set-default 'abbrev-mode t)

(setq save-abbrevs nil)
