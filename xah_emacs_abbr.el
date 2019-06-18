;; -*- coding: utf-8; lexical-binding: t; -*-
;; emacs abbrev def
;; 〈Using Emacs Abbrev Mode for Abbreviation〉 http://ergoemacs.org/emacs/emacs_abbrev_mode.html



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

    ;; English word abbrev

    ("ds" "does" )
    ("n" "and" )
    ("r" "are" )
    ("rt" "return" )
    ("sd" "should" )
    ("th" "there" )
    ("u" "you" )
    ("ur" "your" )
    ("w" "want" )
    ("hv" "have" )
    ("k" "know" )
    ("ab" "about" )
    ("dn" "down" )
    ("minh" "minutes" )

    ;; English phrases abbrev

    ("afaik" "as far as i know" )
    ("atm" "at the moment" )
    ("btw" "by the way" )
    ("cnt" "can't" )
    ("ddnt" "didn't" )
    ("dfb" "difference between" )
    ("dnt" "don't" )
    ("dsnt" "doesn't" )
    ("dvp" "develop" )
    ("hnt" "haven't" )
    ("hs" "here's" )
    ("ie" "i.e." )
    ("iirc" "if i recall correctly" )
    ("im" "I'm" )
    ("isnt" "isn't" )
    ("pov" "point of view" )
    ("sa" "See also:" )
    ("shnt" "shouldn't" )
    ("ths" "this is" )
    ("ti" "that is," )
    ("tr" "there are" )
    ("ts" "there is" )
    ("ty" "thank you" )
    ("uc" "you see" )
    ("ull" "you'll" )
    ("uv" "you've" )
    ("wnt" "won't" )
    ("wrt" "with respect to" )
    ("wsnt" "wasn't" )
    ("wtdb" "What's the difference between" )

    ;; english, single word

    ("alt3" "alternative" )
    ("ann" "announcement" )
    ("apr" "apparently" )
    ("arg3" "argument" )
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
    ("dec3" "declaration" )
    ("def3" "definition" )
    ("desc" "description" )
    ("dev3" "development" )
    ("df3" "different" )
    ("dfc" "difference" )
    ("dict" "dictionary" )
    ("dicts" "dictionaries" )
    ("dir3" "directory" )
    ("dirs" "directories" )
    ("disc" "discussion" )
    ("dl" "download" )
    ("doc3" "documentation" )
    ("dt" "deepthroat" )
    ("eff" "efficient" )
    ("eg3" "ergonomic" )
    ("ege" "e.g." )
    ("env" "environment" )
    ("esp" "especially" )
    ("ex" "example:" )
    ("exp3" "experience" )
    ("expr" "expression" )
    ("faq" "frequently asked questions" )
    ("fex" "for example," )
    ("fol" "following" )
    ("fp" "functional programing" )
    ("fu" "function" )
    ("gvn" "government" )
    ("hev" "however" )
    ("hex" "hexadecimal" )
    ("ia" "interactive" )
    ("im3" "instant message" )
    ("impl" "implementation" )
    ("implt" "implement" )
    ("intn" "international" )
    ("introh" "introduction" )
    ("kb" "keyboard" )
    ("kbg" "keybinding" )
    ("kbs" "keyboards" )
    ("kp" "keypad" )
    ("ks" "keyboard shortcut" )
    ("kw" "keyword" )
    ("l3" "language" )
    ("maint" "maintenance" )
    ("math3" "mathematics" )
    ("mln" "millennials" )
    ("mtn" "mountain" )
    ("ob" "object" )
    ("oc" "occur" )
    ("occ" "occurrence" )
    ("org3" "organization" )
    ("paren" "parenthesis" )
    ("pls" "please" )
    ("pm3" "parameter" )
    ("pp" "people" )
    ("prof" "professor" )
    ("prog" "program" )
    ("prog3" "programing" )
    ("pt" "Put this in your emacs init file:" )
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
    ("usu" "usually" )
    ("ver" "version" )
    ("vid" "video" )
    ("wo" "without" )

    ;; english, multiple words

    ("diy" "Do It Yourself" )
    ("mkb" "mechanical keyboard" )
    ("pc3" "political correctness" )
    ("pl" "programing language" )
    ("qaa" "questions and answers" )
    ("sj3" "social justice" )
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
    ("ode" "ordinary differential equations" )
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
    ("subdir3" "sub-directory" )
    ("subdirs3" "sub-directories" )
    ("ui3" "user interface" )
    ("va" "variable" )
    ("vas" "variables" )
    ("wd" "web development" )
    ("wm" "Window Manager" )
    ("wh" "width height" )

    ;; brackets, parenthesis

    ("eb" "" xah-insert-black-lenticular-bracket【】)
    ("ec" "" xah-insert-ascii-single-quote)
    ("ed" "" xah-insert-double-curly-quote“”)
    ("ef" "" xah-insert-emacs-quote)
    ("eg" "" xah-insert-ascii-double-quote)
    ("eh" "" xah-insert-brace) ; {}
    ("ei" "" xah-insert-curly-single-quote‘’)
    ("el3" "" xah-insert-formfeed)
    ("em" "" xah-insert-corner-bracket「」)
    ("en" "" xah-insert-square-bracket) ; []
    ("ep" "" xah-insert-single-angle-quote‹›)
    ("er" "" xah-insert-tortoise-shell-bracket〔〕 )
    ("es" "" xah-insert-string-assignment)
    ("et" "" xah-insert-paren)
    ("eu" "" xah-insert-date)
    ("ew" "" xah-insert-angle-bracket〈〉)
    ("ey" "" xah-insert-double-angle-quote«»)

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
    ("iPhone" "iPhone" )
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
    ("eq" "==" )
    ("eqq" "===" )
    ("hr" "--------------------------------------------------" )

    ("byline" "<div class=\"byline\">By Xah Lee. Date: <time>2009-07-30</time>. Last updated: <time>2017-05-09</time>.</div>" )

    ;; ("twittercard" "<meta name=\"twitter:image\" content=\"http://example.com/cat.jpg\">" )

    ;; regex
    ("az3" "\\([A-Za-z0-9]+\\)" )
    ("bracket3" "\\[\\([^]]+?\\)\\]" )
    ("curly3" "“\\([^”]+?\\)”" )
    ("digits3" "\\([0-9]+\\)" )
    ;; ("date3" "\\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\)" )
    ("date3" "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" )
    ("dot3" "\\(.\\)" )
    ("str3" "\\([^\"]+?\\)" )
    ("tag3" "\\([</>=\" A-Za-z0-9]+\\)" )

    ;; shell
    ("ytd" "youtube-dl --id -k -x --" )

    ;; html
    ("mxh" "<kbd>Alt</kbd>+<kbd>x</kbd>")
    ("pmovedh" "<p class=\"page_moved_64598\">page moved to <a href=\"x.html\">x</a></p>" )

    ("lyricsh" "<pre class=\"lyrics_xl\">
▮
</pre>")

    ("faqh" "<section class=\"qna_xl\">

<h3 class=\"q\">How to ?</h3>

<p>this</p>

</section>

" )

    ("topich" "<div class=\"topic_xl\">
<h4>Emacs Lisp Basics Topic</h4>
<ol>
<li><a href=\"elisp_basics.html\">Emacs Lisp Basics</a></li>
<li><a href=\"elisp_editing_basics.html\">Overview of Text-Processing in Emacs Lisp</a></li>
</ol>
</div>

")

    ;; unicode
    ("ascii3" "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~" )
    ("bu" "•" )
    ("usf" "🇺🇸" )
    ("catface" "😸" )
    ("bell" "🔔" )
    ("good3" "👍" )
    ("applaud3" "👏" )
    ("heartface" "😻" )
    ("clown" "🤡" )
    ("angry" "😠" )
    ("horror" "😱" )
    ("fear" "😬" )
    ("grin" "😁" )
    ("wink" "😜" )
    ("hearts" "♥💕💓💔💖💗💘💝💞💟💙💚💛💜" )
    ("omg" "😂" )
    ("star" "★" )
    ("star2" "🌟" )
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
    ("uxsl" "http://xahsl.org/" )

    ;; xah personal
    ("xkb" "#xahkeyboard" )
    ("xw" "#xahwords" )
    ("xa" "#xahart" )
    ("xc" "#xahcode" )
    ("xm" "#xahmusic" )
    ("xjs" "#xahjs" )
    ("xp" "#xahpop" )
    ("sszw" "#杀杀中文" )
    ("xts" "xah talk show" )

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

    ("xkb" "#xahkeyboard" )

    ;;

    ))

(progn
  ;; golang
  (when (boundp 'go-mode-abbrev-table)
    (clear-abbrev-table go-mode-abbrev-table))

  (define-abbrev-table 'go-mode-abbrev-table
    '(
      ("go" "package main

import \"fmt\"

func main() {

	fmt.Printf(\"%v\\n\", 33)

}")

      ("imp" "import \"fmt\"\n")
      ("p" "fmt.Printf(\"%v\\n\", ▮)")
      ("spf" "fmt.Sprintf(\"%v\", ▮)")

      ("pl" "fmt.Println(▮)")
      ("r" "return")
      ("st" "string")
      ("eq" "==")
      ("v" "var x = 3")
      ("df" "x := 3")
      ("c" "const x = 3")
      ("f" "func ff(x int) int {
	return nil
}")
      ("if" "if ▮ { 3 }")
      ("ie" " if err != nil { panic(err) }")
      ("ei" "else if x > 0 { 3 }")
      ("else" "else { 3 }")
      ("for" "for i := 0; i < 4; i++ { i }")
      ("fr" "for k, v := range xxx {
▮
    }
")
      ("cmt" "/* \n▮\n*/")
      ("stru" "type myS struct {
y string
x int
}")
      ("ft" "fallthrough")
      ("switch" "	switch 3 {
	case 1:
		fmt.Println( 3 )
	case 2, 3:
		fmt.Println( 4 )
	default:
		fmt.Println( 5 )
	}")

      ("mbs" "var bb = make([]byte, 0, 9)")
      ("sl" "var ss = []int{1,2}")
      ("mp" "var mm = map[string]string{`a`: `1`, `b`: `2`}")
      ("mm" "var mp = make(map[string]int)")
      ("len" "len(▮)")
      ("make" "make([]byte, 0, 9)")
      ("rmc" "regexp.MustCompile(`str`▮)")
      ("rfa" "re.FindAll(b▮, -1)")

      ;;

      )))

(progn
  ;; python

  (define-abbrev-table 'python-mode-abbrev-table
    '(

      ("p" "print(▮)")
      ("r" "return")
      ("eq" "==")
      ;;

      )))

(set-default 'abbrev-mode t)



(defun xah-abbrev-h-f ()
  "Abbrev hook function, used for `define-abbrev'.
 Our use is to prevent inserting the char that triggered expansion. Experimental.
 the “h-f” stand for hook function.
Version 2016-10-24"
  t)

(put 'xah-abbrev-h-f 'no-self-insert t)

(setq abbrev-expand-function 'xah-global-expand-abbrev)

(defun xah-global-expand-abbrev ()
  "function for value of `abbrev-expand-function'.
Expand the symbol before cursor,
if cursor is not in string or comment.
Returns the abbrev symbol if there's a expansion, else nil.
Version 2019-01-10"
  (interactive)
  (when (xah-elisp-abbrev-enable-function) ; abbrev property :enable-function doesn't seem to work, so check here instead
    (let ( $p1 $p2
               $abrStr
               $abrSymbol
               )

      ;; (save-excursion
      ;;   (forward-symbol -1)
      ;;   (setq $p1 (point))
      ;;   (goto-char $p0)
      ;;   (setq $p2 $p0))

      (save-excursion
        ;; 2017-01-16 note: we select the whole symbol to solve a problem. problem is: if “aa”  is a abbrev, and “▮bbcc” is existing word with cursor at beginning, and user wants to type aa- to result in aa-bbcc. Normally, aa immediately expands. This prevent people editing bbcc to become aa-bbcc. This happens for example in elisp, when editing “search-forward” to become “re-search-forward”. The downside of this is that, people cannot type a abbrev when in middle of a word.
        (forward-symbol -1)
        (setq $p1 (point))
        (forward-symbol 1)
        (setq $p2 (point)))

      (setq $abrStr (buffer-substring-no-properties $p1 $p2))
      (setq $abrSymbol (abbrev-symbol $abrStr))
      (if $abrSymbol
          (progn
            (abbrev-insert $abrSymbol $abrStr $p1 $p2 )
            (xah-global-abbrev-position-cursor $p1)
            $abrSymbol)
        nil))))

(defun xah-global-abbrev-position-cursor (&optional @pos)
  "Move cursor back to ▮ if exist, else put at end.
Return true if found, else false.
Version 2016-10-24"
  (interactive)
  (let (($found-p (search-backward "▮" (if @pos @pos (max (point-min) (- (point) 100))) t )))
    (when $found-p (delete-char 1))
    $found-p
    ))

(setq save-abbrevs nil)
