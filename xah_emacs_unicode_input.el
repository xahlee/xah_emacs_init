;;-*- coding: utf-8 -*-
;; emacs customization. keys for inserting brackets and unicode (Dvorak layout).
;; • http://ergoemacs.org/emacs/xah_emacs_keybinding.el
;; • http://ergoemacs.org/emacs/xah_emacs_unicode_input.el
;; • http://ergoemacs.org/emacs/xah_emacs_hyper_super_setup.el
;; • http://ergoemacs.org/emacs/xah_emacs_insert_pairs.el
;; • 〈Emacs Unicode Math Symbols Input Mode (xmsi-mode)〉 http://ergoemacs.org/emacs/xmsi-math-symbols-input.html
;; • 〈Matching Brackets in Unicode〉 http://xahlee.org/comp/unicode_matching_brackets.html
;; • 〈Computing Symbols in Unicode〉 http://xahlee.org/comp/unicode_computing_symbols.html
;; Xah Lee
;; 2007-10
;; ∑ http://xahlee.org/


;; § ----------------------------------------
;; problem keys
;; for qwerty: () [] {} -_ =+
;; for dvorak: () [] {} /? =+


;; right hand top
;; f gcrl

(global-set-key (kbd "C-2 f") 'insert-pair-single-straight-quote) ; ''
(global-set-key (kbd "C-2 g") 'insert-pair-double-straight-quote) ; ""
(define-key key-translation-map (kbd "C-2 c") (kbd "=")) ; equal
(define-key key-translation-map (kbd "C-2 r") (kbd "+")) ; plus


;; § ----------------------------------------
;; right hand home row
;; d htns

(global-set-key (kbd "C-2 h") 'insert-pair-brace)              ;{}
(global-set-key (kbd "C-2 t") 'insert-pair-paren)              ;()
(global-set-key (kbd "C-2 n") 'insert-pair-bracket)            ;[]


;; § ----------------------------------------
;; right hand bottom row
;; b mwvz

(global-set-key (kbd "C-2 m") 'insert-pair-corner-bracket) ;「」
(global-set-key (kbd "C-2 w") 'insert-pair-angle-bracket)        ;〈〉
(global-set-key (kbd "C-2 b") 'insert-pair-black-lenticular-bracket) ;【】

(global-set-key (kbd "C-2 M") 'insert-pair-white-corner-bracket) ;『』
(global-set-key (kbd "C-2 W") 'insert-pair-double-angle-bracket) ;《》
(global-set-key (kbd "C-2 B") 'insert-pair-white-lenticular-bracket) ;〖〗


;; left hand top row
;; ',.p y

(global-set-key (kbd "C-2 p") 'insert-pair-double-angle-quote) ;«»
(global-set-key (kbd "C-2 y") 'insert-pair-single-angle-quote) ;‹›


;; left hand home row
;; aoeu i

(global-set-key (kbd "C-2 u") 'insert-pair-double-curly-quote) ;“”
(global-set-key (kbd "C-2 i") 'insert-pair-single-curly-quote) ;‘’


;; left hand bottom row
;; ;qjk x

(global-set-key (kbd "C-2 x") 'insert-pair-tortoise-shell-bracket)   ;〔〕


;; § ----------------------------------------
;; bullets and other symbols


(define-key key-translation-map (kbd "C-2 0") (kbd "➲")) ; CIRCLED HEAVY WHITE RIGHTWARDS ARROW
(define-key key-translation-map (kbd "C-2 2") (kbd "♺")) ; RECYCLING SYMBOL FOR GENERIC MATERIALS
(define-key key-translation-map (kbd "C-2 3") (kbd "✲")) ; OPEN CENTRE ASTERISK
(define-key key-translation-map (kbd "C-2 4") (kbd "¤")) ; CURRENCY SIGN
(define-key key-translation-map (kbd "C-2 5") (kbd "†")) ; dagger
(define-key key-translation-map (kbd "C-2 %") (kbd "‡")) ; double dagger
(define-key key-translation-map (kbd "C-2 6") (kbd "▸")) ; BLACK RIGHT-POINTING SMALL TRIANGLE
(define-key key-translation-map (kbd "C-2 7") (kbd "▮")) ; BLACK VERTICAL RECTANGLE
(define-key key-translation-map (kbd "C-2 8") (kbd "•")) ; bullet
(define-key key-translation-map (kbd "C-2 9") (kbd "◇")) ; white diamond
(define-key key-translation-map (kbd "C-2 (") (kbd "◆")) ; black diamond
(define-key key-translation-map (kbd "C-2 ?") (kbd "�")) ; REPLACEMENT CHARACTER
(define-key key-translation-map (kbd "C-2 ~") (kbd "〜")) ; WAVE DASH
(define-key key-translation-map (kbd "C-2 =") (kbd "≈"))  ; ALMOST EQUAL TO

(define-key key-translation-map (kbd "C-2 &") (kbd "＆")) ; full width ampersand
(define-key key-translation-map (kbd "C-2 |") (kbd "│")) ; BOX DRAWINGS LIGHT VERTICAL
(define-key key-translation-map (kbd "C-2 -") (kbd "—")) ; EM DASH
(define-key key-translation-map (kbd "C-2 _") (kbd "─")) ; BOX DRAWINGS LIGHT HORIZONTAL
(define-key key-translation-map (kbd "C-2 SPC") (kbd " ")) ; NO-BREAK SPACE

(define-key key-translation-map (kbd "C-2 .") (kbd "…")) ; HORIZONTAL ELLIPSIS

;; ‣ triangle bullet


;; § ----------------------------------------

;; 【Hyper+‹arrow›】
;; (define-key key-translation-map (kbd "<H-left>") (kbd "←")) ; arrow
;; (define-key key-translation-map (kbd "<H-right>") (kbd "→"))
;; (define-key key-translation-map (kbd "<H-up>") (kbd "↑"))
;; (define-key key-translation-map (kbd "<H-down>") (kbd "↓"))

;; 【Hyper+Shift+‹arrow›】
;; (define-key key-translation-map (kbd "<H-S-left>") (kbd "⇐"))
;; (define-key key-translation-map (kbd "<H-S-right>") (kbd "⇒"))
;; (define-key key-translation-map (kbd "<H-S-up>") (kbd "⇑"))
;; (define-key key-translation-map (kbd "<H-S-down>") (kbd "⇓"))

(define-key key-translation-map (kbd "C-2 <left>") (kbd "←")) ; arrow
(define-key key-translation-map (kbd "C-2 <right>") (kbd "→"))
(define-key key-translation-map (kbd "C-2 <up>") (kbd "↑"))
(define-key key-translation-map (kbd "C-2 <down>") (kbd "↓"))

(define-key key-translation-map (kbd "C-2 <S-left>") (kbd "⇐")) ; arrow
(define-key key-translation-map (kbd "C-2 <S-right>") (kbd "⇒"))
(define-key key-translation-map (kbd "C-2 <S-up>") (kbd "⇑"))
(define-key key-translation-map (kbd "C-2 <S-down>") (kbd "⇓"))
