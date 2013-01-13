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
(global-set-key (kbd "<lwindow> y") 'xah-unicode-keymap)


;; right hand top
;; f gcrl

(define-key key-translation-map (kbd "<lwindow> y c") (kbd "=")) ; equal
(define-key key-translation-map (kbd "<lwindow> y r") (kbd "+")) ; plus


;; right hand home row
;; d htns

(global-set-key (kbd "<lwindow> y h") 'insert-pair-brace)              ;{}
(global-set-key (kbd "<lwindow> y t") 'insert-pair-paren)              ;()
(global-set-key (kbd "<lwindow> y n") 'insert-pair-bracket)            ;[]


;; right hand bottom row
;; b mwvz

(global-set-key (kbd "<lwindow> y m") 'insert-pair-corner-bracket「」)
(global-set-key (kbd "<lwindow> y w") 'insert-pair-angle-bracket〈〉)
(global-set-key (kbd "<lwindow> y b") 'insert-pair-black-lenticular-bracket【】)

(global-set-key (kbd "<lwindow> y M") 'insert-pair-white-corner-bracket『』)
(global-set-key (kbd "<lwindow> y W") 'insert-pair-double-angle-bracket《》)
(global-set-key (kbd "<lwindow> y B") 'insert-pair-white-lenticular-bracket〖〗)


;; left hand top row
;; ',.p y

(global-set-key (kbd "<lwindow> y p") 'insert-pair-double-angle-quote«»)
(global-set-key (kbd "<lwindow> y y") 'insert-pair-single-angle-quote‹›)


;; left hand home row
;; aoeu i

(global-set-key (kbd "<lwindow> y u") 'insert-pair-double-curly-quote“”)
(global-set-key (kbd "<lwindow> y i") 'insert-pair-single-curly-quote‘’)


;; left hand bottom row
;; ;qjk x

(global-set-key (kbd "<lwindow> y x") 'insert-pair-tortoise-shell-bracket〔〕)


;; bullets and other symbols


(define-key key-translation-map (kbd "<lwindow> y 0") (kbd "☛")) ; BLACK RIGHT POINTING INDEX
(define-key key-translation-map (kbd "<lwindow> y 3") (kbd "✲")) ; OPEN CENTRE ASTERISK
(define-key key-translation-map (kbd "<lwindow> y 4") (kbd "¤")) ; CURRENCY SIGN
(define-key key-translation-map (kbd "<lwindow> y 6") (kbd "▸")) ; BLACK RIGHT-POINTING SMALL TRIANGLE
(define-key key-translation-map (kbd "<lwindow> y 7") (kbd "▮")) ; BLACK VERTICAL RECTANGLE
(define-key key-translation-map (kbd "<lwindow> y 8") (kbd "•")) ; bullet
(define-key key-translation-map (kbd "<lwindow> y 9") (kbd "◇")) ; white diamond
(define-key key-translation-map (kbd "<lwindow> y (") (kbd "◆")) ; black diamond
(define-key key-translation-map (kbd "<lwindow> y ?") (kbd "�")) ; REPLACEMENT CHARACTER
(define-key key-translation-map (kbd "<lwindow> y ~") (kbd "〜")) ; WAVE DASH

(define-key key-translation-map (kbd "<lwindow> y \\") (kbd "、")) ; IDEOGRAPHIC COMMA

(define-key key-translation-map (kbd "<lwindow> y &") (kbd "＆")) ; full width ampersand
(define-key key-translation-map (kbd "<lwindow> y -") (kbd "—")) ; EM DASH
(define-key key-translation-map (kbd "<lwindow> y SPC") (kbd " ")) ; NO-BREAK SPACE

(define-key key-translation-map (kbd "<lwindow> y .") (kbd "…")) ; HORIZONTAL ELLIPSIS

;; ‣ triangle bullet



(define-key key-translation-map (kbd "<lwindow> y <left>") (kbd "←")) ; arrow
(define-key key-translation-map (kbd "<lwindow> y <right>") (kbd "→"))
(define-key key-translation-map (kbd "<lwindow> y <up>") (kbd "↑"))
(define-key key-translation-map (kbd "<lwindow> y <down>") (kbd "↓"))

(define-key key-translation-map (kbd "<lwindow> y <S-left>") (kbd "⇐")) ; arrow
(define-key key-translation-map (kbd "<lwindow> y <S-right>") (kbd "⇒"))
(define-key key-translation-map (kbd "<lwindow> y <S-up>") (kbd "⇑"))
(define-key key-translation-map (kbd "<lwindow> y <S-down>") (kbd "⇓"))
