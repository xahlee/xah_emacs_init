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

(define-prefix-command 'xah-unicode-keymap)
(global-set-key (kbd "<menu> SPC") xah-unicode-keymap)

(progn
  (define-key key-translation-map (kbd "<menu> SPC SPC") (kbd "_")) ;low line (underscore)
  (define-key key-translation-map (kbd "<menu> SPC RET") (kbd "-"))

  (define-key key-translation-map (kbd "<menu> SPC -") (kbd "—")) ; EM DASH

  (define-key key-translation-map (kbd "<menu> SPC . <down>") (kbd "⇓"))
  (define-key key-translation-map (kbd "<menu> SPC . <left>") (kbd "⇐"))
  (define-key key-translation-map (kbd "<menu> SPC . <right>") (kbd "⇒"))
  (define-key key-translation-map (kbd "<menu> SPC . <up>") (kbd "⇑"))
  (define-key key-translation-map (kbd "<menu> SPC <down>") (kbd "↓"))
  (define-key key-translation-map (kbd "<menu> SPC <left>") (kbd "←"))
  (define-key key-translation-map (kbd "<menu> SPC <right>") (kbd "→"))
  (define-key key-translation-map (kbd "<menu> SPC <up>") (kbd "↑"))
  (define-key key-translation-map (kbd "<menu> SPC \\") (kbd "、")) ; IDEOGRAPHIC COMMA

  (global-set-key (kbd "<menu> SPC ,") 'xah-insert-greater-less)

  (define-key key-translation-map (kbd "<menu> SPC 7") (kbd "＆"))
  (define-key key-translation-map (kbd "<menu> SPC 8") (kbd "•"))

  (global-set-key (kbd "<menu> SPC . b") 'xah-insert-white-lenticular-bracket〖〗)
  (global-set-key (kbd "<menu> SPC . m") 'xah-insert-white-corner-bracket『』)
  (global-set-key (kbd "<menu> SPC . w") 'xah-insert-double-angle-bracket《》)

  (global-set-key (kbd "<menu> SPC b") 'xah-insert-black-lenticular-bracket【】)
  (define-key key-translation-map (kbd "<menu> SPC c") (kbd "=")) ; equal
  (global-set-key (kbd "<menu> SPC d") 'xah-insert-double-curly-quote“”)
  (global-set-key (kbd "<menu> SPC f") 'xah-insert-single-straight-quote)
  (global-set-key (kbd "<menu> SPC g") 'xah-insert-double-straight-quote)
  (global-set-key (kbd "<menu> SPC h") 'xah-insert-brace)              ;{}
  (global-set-key (kbd "<menu> SPC i") 'xah-insert-single-curly-quote‘’)
  (define-key key-translation-map (kbd "<menu> SPC l") (kbd "…")) ; HORIZONTAL ELLIPSIS
  (global-set-key (kbd "<menu> SPC m") 'xah-insert-corner-bracket「」)
  (global-set-key (kbd "<menu> SPC n") 'xah-insert-bracket)            ;[]
  (global-set-key (kbd "<menu> SPC p") 'xah-insert-double-angle-quote«»)
  (define-key key-translation-map (kbd "<menu> SPC r") (kbd "+")) ; plus
  (global-set-key (kbd "<menu> SPC t") 'xah-insert-paren)              ;()
  (global-set-key (kbd "<menu> SPC u") 'xah-insert-unicode)
  (global-set-key (kbd "<menu> SPC w") 'xah-insert-angle-bracket〈〉)
  (global-set-key (kbd "<menu> SPC x") 'xah-insert-tortoise-shell-bracket〔〕)
  (global-set-key (kbd "<menu> SPC y") 'xah-insert-single-angle-quote‹›)
  )
