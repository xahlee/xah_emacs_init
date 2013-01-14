;;-*- coding: utf-8 -*-
;; emacs customization. keys for inserting brackets and unicode (Dvorak layout).
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

(define-prefix-command 'xah-unicode-keymap)
(global-set-key (kbd "<menu> u") 'xah-unicode-keymap)

(define-key key-translation-map (kbd "<menu> u -") (kbd "—")) ; EM DASH
(define-key key-translation-map (kbd "<menu> u .") (kbd "…")) ; HORIZONTAL ELLIPSIS
(define-key key-translation-map (kbd "<menu> u 0") (kbd "✲")) ; OPEN CENTRE ASTERISK
(define-key key-translation-map (kbd "<menu> u 3") (kbd "☛")) ; BLACK RIGHT POINTING INDEX
(define-key key-translation-map (kbd "<menu> u 4") (kbd "◆")) ; black diamond
(define-key key-translation-map (kbd "<menu> u 7") (kbd "＆")) ; full width ampersand
(define-key key-translation-map (kbd "<menu> u 8") (kbd "•")) ; bullet
(define-key key-translation-map (kbd "<menu> u 9") (kbd "◇")) ; white diamond
(define-key key-translation-map (kbd "<menu> u r") (kbd "⇒"))
(define-key key-translation-map (kbd "<menu> u <right>") (kbd "→"))
(define-key key-translation-map (kbd "<menu> u SPC") (kbd " ")) ; NO-BREAK SPACE
(define-key key-translation-map (kbd "<menu> u \\") (kbd "、")) ; IDEOGRAPHIC COMMA
(define-key key-translation-map (kbd "<menu> u c") (kbd "=")) ; equal
(define-key key-translation-map (kbd "<menu> u r") (kbd "+")) ; plus
(define-key key-translation-map (kbd "<menu> u `") (kbd "〜")) ; WAVE DASH
(global-set-key (kbd "<menu> u B") 'insert-pair-white-lenticular-bracket〖〗)
(global-set-key (kbd "<menu> u M") 'insert-pair-white-corner-bracket『』)
(global-set-key (kbd "<menu> u W") 'insert-pair-double-angle-bracket《》)
(global-set-key (kbd "<menu> u b") 'insert-pair-black-lenticular-bracket【】)
(global-set-key (kbd "<menu> u h") 'insert-pair-brace)              ;{}
(global-set-key (kbd "<menu> u i") 'insert-pair-single-curly-quote‘’)
(global-set-key (kbd "<menu> u m") 'insert-pair-corner-bracket「」)
(global-set-key (kbd "<menu> u n") 'insert-pair-bracket)            ;[]
(global-set-key (kbd "<menu> u p") 'insert-pair-double-angle-quote«»)
(global-set-key (kbd "<menu> u t") 'insert-pair-paren)              ;()
(global-set-key (kbd "<menu> u u") 'insert-pair-double-curly-quote“”)
(global-set-key (kbd "<menu> u w") 'insert-pair-angle-bracket〈〉)
(global-set-key (kbd "<menu> u x") 'insert-pair-tortoise-shell-bracket〔〕)
(global-set-key (kbd "<menu> u y") 'insert-pair-single-angle-quote‹›)
