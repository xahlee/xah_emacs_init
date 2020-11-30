;; -*- coding: utf-8; lexical-binding: t; -*-
;; emacs abbrev def
;; 〈Using Emacs Abbrev Mode for Abbreviation〉 http://ergoemacs.org/emacs/emacs_abbrev_mode.html

;; HHH___________________________________________________________________

(clear-abbrev-table global-abbrev-table)

(define-abbrev-table 'global-abbrev-table
  '(

    ;; general scheme
    ;; words ending in ing has abbrev g
    ;; words ending in tion has abbrev n
    ;; words ending in ment has abbrev mt
    ;; words past tense ed abbrev d

    ;; maybe temp

    ("cite" "\n\n<span class=\"cite\">(Regan & Totten, 1975)</span>
<li>familyName, D. (1975). <cite>title</cite> Journal of Science.</li>\n\n" )

    ("todo" "<span class=\"todo\">▮ </span>" )

    ("exp" "experience" )
    ("struct" "structure" )

    ("tp" "transpersonal" )
    ("tps" "transpersonal psychology" )
    ("herm" "hermeneutics" )
    ("phen" "phenomenology" )
    ("phenon" "phenomenon" )

    ("wpm" "words per minute" )
    ("uhk" "Ultimate Hacking Keyboard")
    ("elispt" "emacs lisp" )
    ("sof" "StackOverflow" )

    ("phpt" "<?php 3 ?>" )

    ;; English word abbrev

    ("ab" "about" )
    ("dn" "down" )
    ("ds" "does" )
    ("hv" "have" )
    ("iv" "i have" )
    ("k" "know" )
    ("n" "and" )
    ("oft" "often" )
    ("ph" "perhaps" )
    ("pp" "people" )
    ("prb" "probably" )
    ("prob" "problem" )
    ("q" "question" )
    ("r" "are" )
    ("rly" "really" )
    ("rt" "return" )
    ("sd" "should" )
    ("sts" "sometimes" )
    ("th" "there" )
    ("thx" "thanks" )
    ("u" "you" )
    ("und" "understand" )
    ("undg" "understanding" )
    ("ur" "your" )
    ("usu" "usually" )
    ("w" "want" )
    ("wa" "what" )
    ("wh" "where" )
    ("wi" "with" )
    ("wo" "without" )

    ("cnt" "can't" )
    ("ddnt" "didn't" )
    ("dnt" "don't" )
    ("dsnt" "doesn't" )
    ("hnt" "haven't" )
    ("hs" "here's" )
    ("im" "I'm" )
    ("isnt" "isn't" )
    ("pls" "please" )
    ("tu" "thank you" )
    ("ull" "you'll" )
    ("uv" "you've" )
    ("wnt" "won't" )
    ("ws" "what is" )
    ("wsnt" "wasn't" )

    ;; English phrases abbrev

    ("afaik" "as far as i know" )
    ("idk" "I don't know" )
    ("atm" "at the moment" )
    ("btw" "by the way" )
    ("dfb" "difference between" )
    ("dvp" "develop" )
    ("dvmt" "development" )
    ("ie" "i.e." )
    ("iirc" "if i recall correctly" )
    ("pov" "point of view" )
    ("shnt" "shouldn't" )
    ("ths" "this is" )
    ("ti" "that is," )
    ("tr" "there are" )
    ("ts" "there is" )
    ("ty" "thank you" )
    ("uc" "you see" )
    ("wrt" "with respect to" )
    ("wtdb" "What's the difference between" )
    ("evt" "every time" )

    ;; english, single word

    ("alt" "alternative" )
    ("sig" "significant" )
    ("sigc" "significance" )
    ("ann" "announcement" )
    ("apr" "apparently" )
    ("argt" "argument" )
    ("autom" "automatic" )
    ("bc" "because" )
    ("bg" "background" )
    ("bt" "between" )
    ("charst" "characters" )
    ("cmdt" "command" )
    ("comm" "communication" )
    ("comp" "computer" )
    ("cprs" "comparison" )
    ("ct" "character" )
    ("cvnt" "convenient" )
    ("dect" "declaration" )
    ("def" "definition" )
    ("desc" "describe" )
    ("descn" "description" )
    ("devt" "development" )
    ("dfc" "difference" )
    ("dft" "different" )
    ("dict" "dictionary" )
    ("dicts" "dictionaries" )
    ("dirs" "directories" )
    ("dirt" "directory" )
    ("disc" "discussion" )
    ("dl" "download" )
    ("doct" "documentation" )
    ("dt" "deepthroat" )
    ("eff" "efficient" )
    ("eg" "e.g." )
    ("egt" "ergonomic" )
    ("ele" "element")
    ("env" "environment" )
    ("esp" "especially" )
    ("ex" "example" )
    ("expln" "explanation")
    ("expr" "expression" )
    ("expt" "experience" )
    ("exs" "examples" )
    ("flw" "follow" )
    ("flwg" "following" )
    ("ftr" "feature")
    ("fu" "function" )
    ("gov" "government" )
    ("hev" "however" )
    ("hex" "hexadecimal" )
    ("ia" "interactive" )
    ("imp" "implement" )
    ("impttn" "implementation" )
    ("intn" "international" )
    ("introd" "introduction" )
    ("kb" "keyboard" )
    ("kbg" "keybinding" )
    ("kbs" "keyboards" )
    ("kp" "keypad" )
    ("kw" "keyword" )
    ("lit" "literature" )
    ("lst" "live stream" )
    ("lt" "language" )
    ("maint" "maintenance" )
    ("marj" "marijuana" )
    ("matht" "mathematics" )
    ("mech" "mechanical")
    ("mln" "millennials" )
    ("mtn" "mountain" )
    ("nyt" "New York")
    ("ob" "object" )
    ("oc" "occur" )
    ("occ" "occurrence" )
    ("orgn" "organize" )
    ("orgntn" "organization" )
    ("parag" "paragraph" )
    ("paren" "parenthesis" )
    ("phil" "philosophy" )
    ("pj" "project" )
    ("pjtn" "projection" )
    ("pmt" "parameter" )
    ("pos" "position")
    ("pg" "program" )
    ("pgb" "programable" )
    ("pgg" "programing" )
    ("prof" "professor" )
    ("psy" "psychology" )
    ("ref" "reference" )
    ("scn" "screen" )
    ("ss" "screenshot" )
    ("st" "string" )
    ("sw" "software" )
    ("tb" "trackball" )
    ("techn" "technology" )
    ("tempt" "temperature" )
    ("toc" "table of contents" )
    ("trad" "traditional" )
    ("uds" "underscore" )
    ("ver" "version" )
    ("vid" "video" )

    ;; english, multiple words

    ("diy" "Do It Yourself" )
    ("faq" "frequently asked questions" )
    ("fex" "for example" )
    ("fp" "functional programing" )
    ("imt" "instant message" )
    ("ks" "keyboard shortcut" )
    ("mkb" "mechanical keyboard" )
    ("pct" "political correctness" )
    ("pl" "programing language" )
    ("qna" "questions and answers" )
    ("sa" "See also" )
    ("sj" "social justice" )
    ("wip" "work in progress" )

    ;; english, proper noun

    ("ahd" "American Heritage Dictionary" )
    ("cali" "California")
    ("cn" "Chinese" )
    ("eng" "English" )
    ("europ" "Europe" )
    ("jp" "Japan" )
    ("jpt" "Japanese" )
    ("sf" "San Francisco" )
    ("wp" "Wikipedia" )

    ;; math

    ("det" "differential equations" )
    ("dgt" "differential geometry" )
    ("dm" "dimension" )
    ("fr" "fundamental region" )
    ("ode" "ordinary differential equations" )
    ("sor" "surface of revolution" )

    ;; computing, general

    ("wml" "Windows, Mac, Linux")
    ("ai" "artificial intelligence" )
    ("gv" "global variable" )
    ("cat" "cellular automata" )
    ("cfg" "context-free grammar" )
    ("cli" "command line interface" )
    ("cp" "codepoint" )
    ("cs" "computer science" )
    ("db" "database" )
    ("dtt" "data type" )
    ("ev" "environment variable" )
    ("evs" "environment variables" )
    ("flt" "functional language" )
    ("fs" "fullscreen" )
    ("gui" "graphical user interface" )
    ("ipa" "IP address" )
    ("ist" "image source" )
    ("lhst" "left-hand-side" )
    ("libt" "library" )
    ("libst" "libraries" )
    ("mdt" "metadata" )
    ("oop" "object oriented programing" )
    ("os" "operating system" )
    ("rhs" "right-hand-side" )
    ("rsi" "Repetitive Strain Injury" )
    ("sc" "source code" )
    ("subdir" "subdirectory" )
    ("subdirs" "subdirectories" )
    ("ui" "user interface" )
    ("va" "variable" )
    ("vas" "variables" )
    ("wd" "web development" )
    ("wm" "Window Manager" )

    ;; computing, proper noun
    ("ahk" "AutoHotkey" )
    ("cj" "Clojure" )
    ("cl" "Common Lisp" )
    ("elt" "emacs lisp" )
    ("fb" "Facebook" )
    ("fft" "Firefox" )
    ("gc" "Google Chrome" )
    ("iet" "Internet Explorer" )
    ("jq" "jQuery" )
    ("jst" "JavaScript" )
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
    ("ppt" "PayPal" )
    ("pst" "PowerShell" )
    ("sl" "Second Life" )
    ("tek" "Truly Ergonomic keyboard" )
    ("tst" "TypeScript" )
    ("vb" "Visual Basic" )
    ("wl" "Wolfram Language" )
    ("yt" "YouTube" )

    ;; programing
    ("eq" "==" )
    ("eqq" "===" )
    ("eqt" "=\"▮\"" )

    ("hh" "HHH___________________________________________________________________" )

    ("byline" "<div class=\"byline\">By Xah Lee. Date: <time>2009-07-30</time>. Last updated: <time>2017-05-09</time>.</div>" )

    ;; ("twittercard" "<meta name=\"twitter:image\" content=\"http://example.com/cat.jpg\">" )

    ;; regex
    ("azt" "\\([A-Za-z0-9]+\\)" )
    ("brackett" "\\[\\([^]]+?\\)\\]" )
    ("curlyt" "“\\([^”]+?\\)”" )
    ("digitst" "\\([0-9]+\\)" )
    ;; ("datet" "\\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\)" )
    ("datet" "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" )
    ("dott" "\\(.\\)" )
    ("strt" "\\([^\"]+?\\)" )
    ("tagt" "\\([</>=\" A-Za-z0-9]+\\)" )

    ;; shell
    ("ytd" "youtube-dl --id -k -x --" )

    ;; html
    ("mxh" "<kbd>Alt</kbd>+<kbd>x</kbd>")
    ("pmovedh" "<p class=\"page_moved_64598\">page moved to <a href=\"x.html\">x</a></p>" )
    ("byline" "<div class=\"byline\">By Xah Lee. Date: <time>2010-10-05</time>. Last updated: <time>2020-08-16</time>.</div>")
    ("divflow" "<section class=\"divFlow81777\">\n\n</section>\n\n")
    ("xls" "live stream in x hours. x San Francisco time.
Goto YouTube xah lee, sub + 🔔 .
https://www.youtube.com/c/xahlee/live" )
    ("lyricsh" "<pre class=\"lyrics_xl\">\n▮\n</pre>")
    ("pt" "<p>\nPut this in your emacs init file:\n</p>\n\n" )
    ("topich" "<div class=\"topic_xl\">\n<h4>JavaScript Reverse Key/Value</h4>\n<ul>\n<li><a href=\"xx1.html\">xx1</a></li>\n<li><a href=\"xx2.html\">xx2</a></li>\n</ul>\n</div>\n\n")

    ("faqh" "<section class=\"qna_xl\">\n\n<h3 class=\"q\">How to ?</h3>\n\n<p>this</p>\n\n</section>\n\n" )

    ;; unicode
    ("asciit" "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~" )
    ("bu" "•" )
    ("la" "←" )
    ("ua" "↑" )
    ("ra" "→" )
    ("da" "↓" )
    ("fn" "ƒ" )

    ("catu" "😸" )
    ("hahau" "🤩" )
    ("bellu" "🔔" )
    ("goodu" "👍" )
    ("clapu" "👏" )
    ("thxu" "🙏" )
    ("clownu" "🤡" )
    ("angryu" "😠" )
    ("horroru" "😱" )
    ("fearu" "😬" )
    ("grinu" "😁" )
    ("winku" "😜" )
    ("heartu" "♥💖" )
    ("omgu" "😂" )
    ("startu" "★" )
    ("newu" "🆕" )
    ("staru" "🌟" )
    ("emo" "😃😄😅🤩🤡🤗😠🚀🛸🌛🌞☄🌬🎪🎠🎡🎢☠🌟🔷💠" )

    ;; code
    ("utf8t" "-*- coding: utf-8 -*-" )

    ("vdspc" "Visual Dictionary of Special Plane Curves" )
    ("xfk" "xah fly keys" )
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

    ("umma" "http://xahlee.info/M/index.html" )
    ("uocaml" "http://xahlee.info/ocaml/ocaml.html" )
    ("uperl" "http://xahlee.info/perl-python/perl_basics.html" )
    ("uphp" "http://xahlee.info/php/php_basics.html" )
    ("uups" "http://xahlee.info/powershell/index.html" )

    ("upython" "http://xahlee.info/python/python3_basics.html" )
    ("uruby" "http://xahlee.info/ruby/ruby_basics.html" )

    ("utg" "http://xahlee.info/UnixResource_dir/writ/tech_geeker.html" )
    ("uunicode" "http://xahlee.info/comp/unicode_index.html" )

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
    ("xts" "Xah Talk Show" )
    ("ee" "ergoemacs" )
    ("xtv" "XahTV")
    ("xl" "xahlee" )

    ("xim" "Twitter: @xah_lee
Google talk: xahlee@gmail.com
Skype: XahLee
AIM: xahlee
Yahoo: P0lyglut
MSN: p0lyglut@yahoo.com or xahlee
Second Life: Xah Toll
QQ: http://user.qzone.qq.com/2609322939" )

    ;;

    ))

(progn
  ;; golang
  (when (boundp 'go-mode-abbrev-table)
    (clear-abbrev-table go-mode-abbrev-table))

  (define-abbrev-table 'go-mode-abbrev-table
    '(
      ("go" "package main\n\nimport \"fmt\"\n\nfunc main() {\n\n	fmt.Printf(\"%v\\n\", 33)\n\n}")

      ("imp" "import \"fmt\"\n")
      ("p" "fmt.Printf(\"%v\\n\", ▮)")
      ("spf" "fmt.Sprintf(\"%v\", ▮)")

      ("pl" "fmt.Println(▮)")
      ("r" "return")
      ("st" "string")
      ("eq" "==")
      ("v" "var ▮ = ")
      ("df" "x := 3")
      ("c" "const x = 3")
      ("f" "func ff(x int) int {\n	return nil\n}")
      ("if" "if ▮ { 3 }")
      ("ie" " if err != nil { panic(err) }")
      ("ei" "else if x > 0 { 3 }")
      ("else" "else { 3 }")
      ("for" "for i := 0; i < 4; i++ { i }")
      ("fr" "for key, val := range xxx {
▮
    }
")
      ("cmt" "/* \n▮\n*/")
      ("stru" "type myS struct {\ny string\nx int\n}")
      ("ft" "fallthrough")
      ("switch" "	switch 3 {\n	case 1:\n		fmt.Println( 3 )\n	case 2, 3:\n		fmt.Println( 4 )\n	default:\n		fmt.Println( 5 )\n	}")

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

;; HHH___________________________________________________________________

(defun xah-abbrev-hook-function ()
  "Abbrev hook function, used for `define-abbrev'.
 Our use is to prevent inserting the char that triggered expansion. Experimental.
Version 2016-10-24"
  t)

(put 'xah-abbrev-hook-function 'no-self-insert t)

;; (setq abbrev-expand-function 'xah-global-expand-abbrev)

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
