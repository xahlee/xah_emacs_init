;;-*- coding: utf-8 -*-
;; emacs customization. keys for inserting brackets or unicode chars (Dvorak layout).
;; • xah_emacs_keybinding.el
;; • xah_emacs_unicode_input.el
;; • xah_emacs_hyper_super_setup.el
;; • xah_emacs_insert_pairs.el
;; • 〈Emacs Unicode Math Symbols Input Mode (xmsi-mode)〉 http://ergoemacs.org/emacs/xmsi-math-symbols-input.html
;; • 〈Matching Brackets in Unicode〉 http://xahlee.info/comp/unicode_matching_brackets.html
;; • 〈Computing Symbols in Unicode〉 http://xahlee.info/comp/unicode_computing_symbols.html
;; Xah Lee
;; 2007-10
;; ∑ http://xahlee.org/


;; problem keys
;; for qwerty: () [] {} -_ =+
;; for dvorak: () [] {} /? =+

;; (define-prefix-command 'xah-unicode-keymap)
;; (global-set-key (kbd "<menu> u") xah-unicode-keymap)

(defvar xah-unicode-list nil "alist of Unicode symbols. first element is unicode, second element is a string used as key shortcut in ido-completing-read")
(setq xah-unicode-list
      '(
        ("◇" . "3" )
        ("◆" . "4" )
        ("¤" . "2" )
        ("…" . "l" )
        (" " . "s" )
        ("、" . "," )
        ("•" . "8" )
        ("⭑" . "9" )
        ("🎶" . "5" )
        ("—" . "-" )
        ("＆" . "7" )
        ("↓" . "at")
        ("←" . "ah")
        ("→" . "an")
        ("↑" . "ac")
        ("👍" . "tu")
        ) )

(defun xah-insert-unicode ()
  "insert a unicode"
  (interactive)
  (let (gotThis)
    (setq gotThis
          (ido-completing-read "insert:" (mapcar (lambda (x) (concat (car x) (cdr x))) xah-unicode-list)) )
    (insert (car (assoc (substring gotThis 0 1) xah-unicode-list)))
    )
  )

(progn

  (define-key key-translation-map (kbd "<menu> SPC") (kbd "_")) ;low line (underscore)

  (define-key key-translation-map (kbd "<menu> u -") (kbd "—")) ; EM DASH
  (global-set-key (kbd "<menu> u ,") 'xah-insert-greater-less)

  (global-set-key (kbd "<menu> u RET") 'xah-insert-unicode)
  (define-key key-translation-map (kbd "<menu> u 7") (kbd "＆")) 
  (define-key key-translation-map (kbd "<menu> u 8") (kbd "•"))

  (define-key key-translation-map (kbd "<menu> u . <down>") (kbd "⇓"))
  (define-key key-translation-map (kbd "<menu> u . <left>") (kbd "⇐"))
  (define-key key-translation-map (kbd "<menu> u . <right>") (kbd "⇒"))
  (define-key key-translation-map (kbd "<menu> u . <up>") (kbd "⇑"))
  (global-set-key (kbd "<menu> u . b") 'xah-insert-white-lenticular-bracket〖〗)
  (global-set-key (kbd "<menu> u . m") 'xah-insert-white-corner-bracket『』)
  (global-set-key (kbd "<menu> u . w") 'xah-insert-double-angle-bracket《》)

  (define-key key-translation-map (kbd "<menu> u <down>") (kbd "↓"))
  (define-key key-translation-map (kbd "<menu> u <left>") (kbd "←"))
  (define-key key-translation-map (kbd "<menu> u <right>") (kbd "→"))
  (define-key key-translation-map (kbd "<menu> u <up>") (kbd "↑"))
  (define-key key-translation-map (kbd "<menu> u SPC") (kbd " ")) ;insert non-breaking space
  (define-key key-translation-map (kbd "<menu> u \\") (kbd "、")) ; IDEOGRAPHIC COMMA

  (global-set-key (kbd "<menu> u b") 'xah-insert-black-lenticular-bracket【】)
  (define-key key-translation-map (kbd "<menu> u c") (kbd "=")) ; equal
  (global-set-key (kbd "<menu> u d") 'xah-insert-double-curly-quote“”)
  (global-set-key (kbd "<menu> u f") 'xah-insert-single-straight-quote)
  (global-set-key (kbd "<menu> u g") 'xah-insert-double-straight-quote)
  (global-set-key (kbd "<menu> u h") 'xah-insert-brace)              ;{}
  (global-set-key (kbd "<menu> u i") 'xah-insert-single-curly-quote‘’)
  (define-key key-translation-map (kbd "<menu> u l") (kbd "…")) ; HORIZONTAL ELLIPSIS
  (global-set-key (kbd "<menu> u m") 'xah-insert-corner-bracket「」)
  (global-set-key (kbd "<menu> u n") 'xah-insert-bracket)            ;[]
  (global-set-key (kbd "<menu> u p") 'xah-insert-double-angle-quote«»)
  (define-key key-translation-map (kbd "<menu> u r") (kbd "+")) ; plus
  (global-set-key (kbd "<menu> u t") 'xah-insert-paren)              ;()
  (global-set-key (kbd "<menu> u w") 'xah-insert-angle-bracket〈〉)
  (global-set-key (kbd "<menu> u x") 'xah-insert-tortoise-shell-bracket〔〕)
  (global-set-key (kbd "<menu> u y") 'xah-insert-single-angle-quote‹›)
  )
