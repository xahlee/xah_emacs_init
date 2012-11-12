;; -*- coding: utf-8 -*-
;; emacs abbrev def
;; Xah Lee
;; 2007-06, …, 2011-12-20
;; ∑ http://xahlee.org/

; load my abbreviations file
;(read-abbrev-file "emacs_abbrev")

;; clear existing value; when changing a abbrev, it is convenient for user to not have previous abbrev around
(clear-abbrev-table global-abbrev-table)

(define-abbrev-table 'global-abbrev-table '(

    ("8btw" "by the way")
    ("8afaik" "As far as i know")

    ("8b" "because")
    ("8ex" "example")
    ("8atm" "at the moment")
    ("8q" "question")
    ("8i18n" "international")
    ("8ref" "reference")
    ("8temp" "temperature")
    ("8gvn" "government")
    ("8auto" "automatically")
    ("8math" "mathematics")
    ("8diff" "difference")
    ("8comp" "computer")
    ("8lang" "language")
    ("8langs" "languages")
    ("8p" "programing")
    ("8exp" "experience")
    ("8tech" "technology")
    ("8cs" "computer science")
    ("8ai" "artificial intelligence")
    ("8fs" "fullscreen")
    ("8ch" "Chinese")

    ("eg" "e.g.")

    ;; tech
    ("8wp" "Wikipedia")
    ("8ms" "Microsoft")
    ("8qt" "QuickTime")
    ("8it" "IntelliType")
    ("8msw" "Microsoft Windows")
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
    ("8osx" "OS X")

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
    ("84ex" "For example")
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

    ("8line" "────────── ────────── ────────── ────────── ──────────")
    ("8utf8" "-*- coding: utf-8 -*-")
    ("8ascii" "!\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~")

;; elisp regex
    ("8date" "\\([0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]\\)")
    ("8d" "\\([0-9]+\\)")
    ("8str" "\\([^\"]+?\\)")
    ("8tag" "\\([^<]+?\\)<")
    ("8curly" "“\\([^”]+?\\)”")
    ("8bracket" "\\[\\([^]]+?\\)\\]")

    ;; css colors
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

;; html css
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
    ("8sb" "<p class=\"sb\"><span class=\"sb\">✻    ✻    ✻</span></p>")
    ("8css" "<link rel=\"stylesheet\" href=\"../../lbasic.css\" />")
    ("8css2" "<style type=\"text/css\">\np {line-height:130%}\n</style>")

;; unix shell
    ("8ditto" "ditto -ck --sequesterRsrc --keepParent src dest")
    ("8im" "convert -quality 85% ")
    ("8ims" "convert -scale 50% -quality 85% ")
    ("8im256" "convert +dither -colors 256 ")
    ("8imf" "find . -name \"*png\" | xargs -l -i basename \"{}\" \".png\" | xargs -l -i  convert -quality 85% \"{}.png\" \"{}.jpg\"")
    ("8bmp2png" "find . -name \"*bmp\" | xargs -l -i basename \"{}\" \".bmp\" | xargs -l -i  convert \"{}.bmp\" \"{}.png\"")

    ("8f0" "find . -type f -empty")
    ("8f0rm" "find . -type f -size 0 -exec rm {} ';'")
    ("8frm" "find . -name \"*~\" -exec rm {} ';'")
    ("8f0d" "find . -depth -empty -type d")
    ("8f0drm" "find . -depth -empty -type d -exec rmdir {} ';'")
    ("8chmod" "find . -type f -exec chmod 644 {} ';'")
    ("8chmod2" "find . -type d -exec chmod 755 {} ';'")

    ("8lynx" "lynx -dump -display_charset=utf-8 ")
    ("8unison" "unison -servercmd /usr/bin/unison c:/Users/xah/web ssh://xah@169.254.145.104//Users/xah/web")
    ("8sftp" "sftp u40651120@s168753655.onlinehome.us")
    ("8ssh" "ssh -l u40651120 xahlee.org")
    ("8rsn" "rsync -z -r -v -t --chmod=Dugo+x --chmod=ugo+r --delete --exclude=\"*~\" --exclude=\".git/\" --exclude=\".bash_history\" --exclude=\"logs/\"  --rsh=\"ssh -l u40651120\" ~/web/ u40651120@s168753655.onlinehome.us:~/")
    ("8rsnbackup" "rsync -r -v -t --chmod=Dugo+x --chmod=ugo+r --delete --exclude=\"**/My *\" --rsh=\"ssh -l xah\" /media/HP/Users/xah/Documents /media/HP/Users/xah/web /media/HP/Users/xah/Pictures /media/HP/Users/xah/Shared /media/HP/Users/xah/cinse_pixra3 xah@192.168.1.6:~/")
    ("8rsn-web" "rsync -r -v -t --delete --rsh=\"ssh -l xah\" ~/web/ xah@169.254.153.147:~/web/")
    ("8rsn-doc" "rsync -r -v -t --delete --exclude=\"**/My *\" --rsh=\"ssh -l xah\" ~/Documents/ xah@169.254.153.147:~/Documents/")

;; xah personal
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

("8uemacs" "http://ergoemacs.org/emacs/emacs.html")
("8uvi" "http://ergoemacs.org/emacs/emergency_vi.html")
("8uelisp" "http://ergoemacs.org/emacs/elisp.html")
("8uahk" "http://xahlee.info/mswin/autohotkey.html")
("8ueek" "http://ergoemacs.org/emacs/ergonomic_emacs_keybinding.html")
("8uhtml" "http://xahlee.info/js/index.html")
("8uperl" "http://xahlee.info/perl-python/index.html")
("8uphp" "http://xahlee.info/php/index.html")
("8ucss" "http://xahlee.info/js/css_index.html")
("8upython" "http://xahlee.info/perl-python/index.html")
("8uocaml" "http://xahlee.info/ocaml/ocaml.html")
("8ups" "http://xahlee.info/powershell/index.html")
("8umma" "http://xahlee.info/M/index.html")
("8ujava" "http://xahlee.info/java-a-day/java.html")
("8ujs" "http://xahlee.info/js/js.html")
("8utg" "http://xahlee.info/UnixResource_dir/writ/tech_geeker.html")

    ))

;; stop asking whether to save newly added abbrev when quitting emacs
(setq save-abbrevs nil)
