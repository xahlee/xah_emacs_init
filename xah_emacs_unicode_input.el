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
(global-set-key (kbd "<menu> y") 'xah-unicode-keymap)

(define-key key-translation-map (kbd "<menu> e -") (kbd "—")) ; EM DASH
(define-key key-translation-map (kbd "<menu> e .") (kbd "…")) ; HORIZONTAL ELLIPSIS
(define-key key-translation-map (kbd "<menu> e 0") (kbd "✲")) ; OPEN CENTRE ASTERISK
(define-key key-translation-map (kbd "<menu> e 3") (kbd "☛")) ; BLACK RIGHT POINTING INDEX
(define-key key-translation-map (kbd "<menu> e 4") (kbd "◆")) ; black diamond
(define-key key-translation-map (kbd "<menu> e 7") (kbd "＆")) ; full width ampersand
(define-key key-translation-map (kbd "<menu> e 8") (kbd "•")) ; bullet
(define-key key-translation-map (kbd "<menu> e 9") (kbd "◇")) ; white diamond
(define-key key-translation-map (kbd "<menu> e r") (kbd "⇒"))
(define-key key-translation-map (kbd "<menu> e <right>") (kbd "→"))
(define-key key-translation-map (kbd "<menu> e SPC") (kbd " ")) ; NO-BREAK SPACE
(define-key key-translation-map (kbd "<menu> e \\") (kbd "、")) ; IDEOGRAPHIC COMMA
(define-key key-translation-map (kbd "<menu> e c") (kbd "=")) ; equal
(define-key key-translation-map (kbd "<menu> e r") (kbd "+")) ; plus
(define-key key-translation-map (kbd "<menu> e `") (kbd "〜")) ; WAVE DASH
(global-set-key (kbd "<menu> e B") 'insert-pair-white-lenticular-bracket〖〗)
(global-set-key (kbd "<menu> e M") 'insert-pair-white-corner-bracket『』)
(global-set-key (kbd "<menu> e W") 'insert-pair-double-angle-bracket《》)
(global-set-key (kbd "<menu> e b") 'insert-pair-black-lenticular-bracket【】)
(global-set-key (kbd "<menu> e h") 'insert-pair-brace)              ;{}
(global-set-key (kbd "<menu> e i") 'insert-pair-single-curly-quote‘’)
(global-set-key (kbd "<menu> e m") 'insert-pair-corner-bracket「」)
(global-set-key (kbd "<menu> e n") 'insert-pair-bracket)            ;[]
(global-set-key (kbd "<menu> e p") 'insert-pair-double-angle-quote«»)
(global-set-key (kbd "<menu> e t") 'insert-pair-paren)              ;()
(global-set-key (kbd "<menu> e u") 'insert-pair-double-curly-quote“”)
(global-set-key (kbd "<menu> e w") 'insert-pair-angle-bracket〈〉)
(global-set-key (kbd "<menu> e x") 'insert-pair-tortoise-shell-bracket〔〕)
(global-set-key (kbd "<menu> e y") 'insert-pair-single-angle-quote‹›)
