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
  (global-set-key (kbd "<menu> SPC <menu>") 'xah-insert-paren)
  (define-key key-translation-map (kbd "<menu> SPC SPC") (kbd "_")) ; low line (underscore)
  (global-set-key (kbd "<menu> SPC RET") 'xah-insert-unicode)

  (define-key key-translation-map (kbd "<menu> SPC .") nil) 


  (define-key key-translation-map (kbd "<menu> SPC <down>") (kbd "↓"))
  (define-key key-translation-map (kbd "<menu> SPC <left>") (kbd "←"))
  (define-key key-translation-map (kbd "<menu> SPC <right>") (kbd "→"))
  (define-key key-translation-map (kbd "<menu> SPC <up>") (kbd "↑"))
  (define-key key-translation-map (kbd "<menu> SPC \\") (kbd "、")) ; IDEOGRAPHIC COMMA

  (global-set-key (kbd "<menu> SPC ,") 'xah-insert-greater-less)

  (define-key key-translation-map (kbd "<menu> SPC 3") (kbd "φ"))
  (define-key key-translation-map (kbd "<menu> SPC 4") (kbd "ξ"))
  (define-key key-translation-map (kbd "<menu> SPC 6") (kbd "ƒ"))
  (define-key key-translation-map (kbd "<menu> SPC 7") (kbd "＆"))
  (define-key key-translation-map (kbd "<menu> SPC 8") (kbd "•"))
  (define-key key-translation-map (kbd "<menu> SPC 9") (kbd "—")) ; EM DASH

  (global-set-key (kbd "<menu> SPC a") nil)
  (global-set-key (kbd "<menu> SPC b") 'xah-insert-black-lenticular-bracket【】)
  (global-set-key (kbd "<menu> SPC c") 'xah-insert-ascii-single-quote)
  (global-set-key (kbd "<menu> SPC d") 'xah-insert-double-curly-quote“”)
  (define-key key-translation-map (kbd "<menu> SPC e") (kbd "=")) ; equal
  (global-set-key (kbd "<menu> SPC f") 'xah-insert-emacs-quote)
  (global-set-key (kbd "<menu> SPC g") 'xah-insert-ascii-double-quote)
  (global-set-key (kbd "<menu> SPC h") 'xah-insert-brace)              ;{}
  (global-set-key (kbd "<menu> SPC i") 'xah-insert-curly-single-quote‘’)
  (global-set-key (kbd "<menu> SPC j") nil)
  (global-set-key (kbd "<menu> SPC k") nil)
  (define-key key-translation-map (kbd "<menu> SPC l") (kbd "…")) ; HORIZONTAL ELLIPSIS
  (global-set-key (kbd "<menu> SPC m") 'xah-insert-corner-bracket「」)
  (global-set-key (kbd "<menu> SPC n") 'xah-insert-bracket)            ;[]
  (global-set-key (kbd "<menu> SPC o") nil)
  (define-key key-translation-map (kbd "<menu> SPC p") (kbd "+")) ; plus
  (global-set-key (kbd "<menu> SPC q") nil)
  (global-set-key (kbd "<menu> SPC r") nil)
  (global-set-key (kbd "<menu> SPC s") nil)
  (global-set-key (kbd "<menu> SPC t") 'xah-insert-paren)
  (define-key key-translation-map (kbd "<menu> SPC u") (kbd "-")) ; minus

  (global-set-key (kbd "<menu> SPC v") 'xah-insert-double-angle-quote«»)
  (global-set-key (kbd "<menu> SPC w") 'xah-insert-angle-bracket〈〉)
  (global-set-key (kbd "<menu> SPC x") 'xah-insert-tortoise-shell-bracket〔〕)
  (global-set-key (kbd "<menu> SPC y") 'xah-insert-single-angle-quote‹›)
  (global-set-key (kbd "<menu> SPC z") nil)

  )
