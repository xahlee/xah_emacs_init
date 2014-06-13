;;-*- coding: utf-8 -*-
;; emacs customization. keys for inserting brackets or unicode chars (Dvorak layout).
;; • xah_emacs_keybinding.el
;; • xah_emacs_unicode_input.el
;; • xah_emacs_hyper_super_setup.el
;; • xah_emacs_insert_brackets.el
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

(global-set-key (kbd "<menu> u") xah-unicode-keymap)

(progn
  (global-set-key (kbd "<menu> u <menu>") 'xah-insert-paren)
  (define-key key-translation-map (kbd "<menu> u SPC") (kbd "_")) ; low line (underscore)
  (global-set-key (kbd "<menu> u RET") 'xah-insert-unicode)

  (define-key key-translation-map (kbd "<menu> u .") nil)

  (define-key key-translation-map (kbd "<menu> u <down>") (kbd "↓"))
  (define-key key-translation-map (kbd "<menu> u <left>") (kbd "←"))
  (define-key key-translation-map (kbd "<menu> u <right>") (kbd "→"))
  (define-key key-translation-map (kbd "<menu> u <up>") (kbd "↑"))
  (define-key key-translation-map (kbd "<menu> u \\") (kbd "、")) ; IDEOGRAPHIC COMMA

  (global-set-key (kbd "<menu> u ,") 'xah-insert-greater-less)

  (define-key key-translation-map (kbd "<menu> u 3") (kbd "φ"))
  (define-key key-translation-map (kbd "<menu> u 4") (kbd "ξ"))
  (define-key key-translation-map (kbd "<menu> u 6") (kbd "ƒ"))
  (define-key key-translation-map (kbd "<menu> u 7") (kbd "＆"))
  (define-key key-translation-map (kbd "<menu> u 8") (kbd "•"))
  (define-key key-translation-map (kbd "<menu> u 9") (kbd "—")) ; EM DASH

  (global-set-key (kbd "<menu> u a") nil)
  (global-set-key (kbd "<menu> u b") 'xah-insert-black-lenticular-bracket【】)
  (global-set-key (kbd "<menu> u c") 'xah-insert-ascii-single-quote)
  (global-set-key (kbd "<menu> u d") 'xah-insert-double-curly-quote“”)
  (define-key key-translation-map (kbd "<menu> u e") (kbd "=")) ; equal
  (global-set-key (kbd "<menu> u f") 'xah-insert-emacs-quote)
  (global-set-key (kbd "<menu> u g") 'xah-insert-ascii-double-quote)
  (global-set-key (kbd "<menu> u h") 'xah-insert-brace)              ;{}
  (global-set-key (kbd "<menu> u i") 'xah-insert-curly-single-quote‘’)
  (global-set-key (kbd "<menu> u j") nil)
  (global-set-key (kbd "<menu> u k") nil)
  (define-key key-translation-map (kbd "<menu> u l") (kbd "…")) ; HORIZONTAL ELLIPSIS
  (global-set-key (kbd "<menu> u m") 'xah-insert-corner-bracket「」)
  (global-set-key (kbd "<menu> u n") 'xah-insert-bracket)            ;[]
  (global-set-key (kbd "<menu> u o") nil)
  (define-key key-translation-map (kbd "<menu> u p") (kbd "+")) ; plus
  (global-set-key (kbd "<menu> u q") nil)
  (global-set-key (kbd "<menu> u r") 'xah-insert-tortoise-shell-bracket〔〕)
  (global-set-key (kbd "<menu> u s") nil)
  (global-set-key (kbd "<menu> u t") 'xah-insert-paren)
  (define-key key-translation-map (kbd "<menu> u u") (kbd "-")) ; minus

  (global-set-key (kbd "<menu> u v") 'xah-insert-double-angle-quote«»)
  (global-set-key (kbd "<menu> u w") 'xah-insert-angle-bracket〈〉)
  (global-set-key (kbd "<menu> u x") nil)
  (global-set-key (kbd "<menu> u y") 'xah-insert-single-angle-quote‹›)
  (global-set-key (kbd "<menu> u z") nil)

  )
