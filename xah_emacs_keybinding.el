;; -*- coding: utf-8 -*-
;; xah's emacs keybinding.

;; jump points to other files
;; • xah_emacs_keybinding.el
;; • xah_emacs_unicode_input.el
;; • xah_emacs_hyper_super_setup.el
;; • xah_emacs_mouse_binding.el
;; • xah_emacs_alias.el
;; • xah_emacs_abbr.el
;; ~/git/ergoemacs/ergoemacs/ergoemacs-keybindings/ergoemacs-variants.el

;; • 〈Dvorak Keyboard Layout〉 http://xahlee.info/comp/dvorak_keyboard_layout.html
;; • 〈ErgoEmacs Keybinding〉 http://ergoemacs.org/emacs/ergonomic_emacs_keybinding.html
;; • 〈xah_autohotkey.ahk〉 http://xahlee.info/mswin/autohotkey.html
;; • 〈Emacs: How to define Hyper ＆ Super Keys〉 http://ergoemacs.org/emacs/emacs_hyper_super_keys.html
;; • 〈Emacs: Remapping Keys Using key-translation-map〉 http://ergoemacs.org/emacs/emacs_key-translation-map.html
;; • 〈Emacs: Add Custom Keys to Enhance Productivity〉 http://ergoemacs.org/emacs/emacs_useful_user_keybinding.html
;; • 〈Keyboard Layouts Fight! Dvorak, Maltron, Colemak, NEO, Bépo, Turkish-F, …〉  http://xahlee.info/kbd/dvorak_and_all_keyboard_layouts.html

;; Xah Lee
;; created: 2007-06.


;; generic

(define-key key-translation-map (kbd "<apps>") (kbd "<menu>"))
;(define-key key-translation-map (kbd "C-8") (kbd "<menu>"))
;(define-key key-translation-map (kbd "<f6>") (kbd "<menu>"))
;; (define-key key-translation-map (kbd "<henkan>") (kbd "<delete>")) ; henkan is the 変換 key on Japanese keyboard for “do convert”

;; (global-set-key (kbd "<delete>") 'set-mark-command)

;; in linux, by default, <lwindow> is Super,  s-
;; Linux, the menu/apps key syntax is <menu>
;; Windows, the menu/apps key syntax is <apps>

;; (global-set-key (kbd "<f8>") ctl-x-map)
;; (global-set-key (kbd "<f8>") mode-specific-map)

(global-set-key (kbd "]") 'ergoemacs-close-current-buffer)
;; (global-set-key (kbd "<f8>") 'ergoemacs-switch-to-next-frame)
;; (global-set-key (kbd "<f9>") 'split-window-vertically)
;; (global-set-key (kbd "<f9>") 'delete-other-windows)
;(global-set-key (kbd "<f9>") 'ergoemacs-move-cursor-next-pane)
;; (global-set-key (kbd "<f10>") 'split-window-vertically)
(global-set-key (kbd "<f11>") 'ergoemacs-previous-user-buffer)
(global-set-key (kbd "<f12>") 'ergoemacs-next-user-buffer)

(global-set-key (kbd "<M-backspace>") 'nil) ;need to set this to something useful

;; (global-set-key (kbd "<f8>") 'set-mark-command)
;; (global-set-key (kbd "M-SPC") 'set-mark-command)
;; (global-set-key (kbd "M-SPC") 'beep)



(global-set-key (kbd "M-m") 'hippie-expand)

(define-prefix-command 'xah-keymap)
(global-set-key (kbd "<menu>") xah-keymap)

;; Truly Ergonomic keyboard + Dvorak layout
;; \` 1234 5 6 7890 []
;; \z ',.p y f gcrl /=
;;    aoeu i d htns
;;    ;qjk x b mwv-

;; prefix keys should be
;; .p yf gc
;; eu id ht

;; u is for unicode insert
;; e is mode-specific
;; h is help-map
;; t is generic dump space
;; x is for ctl-x-map

;; add find-file
;; keyboard-quit

;; stars indicate frequency of use
;; ★★★ every minute
;; ★★ every hour
;; ★ few times a day

; qwerty
; er t y ui
; df g h jk

; matt's ErgoEmacs key sequence bindings
; 【.】 【p】 【y】       | 【f】 C-h 【g】 C-c 【c】
; 【e】 【u】 C-x   【i】  | 【d】 C-h 【h】 C-c 【t】
;                                 【m】 commit

(progn
  (define-key help-map (kbd "c") 'describe-char)
  (define-key help-map (kbd "3") 'man)
  (define-key help-map (kbd "7") 'lookup-google)
  (define-key help-map (kbd "8") 'lookup-wikipedia)
  (define-key help-map (kbd "9") 'lookup-word-definition)
  (define-key help-map (kbd "0") 'lookup-all-dictionaries)
  (define-key help-map (kbd "`") 'elisp-index-search)
  (define-key help-map (kbd "m") 'ergoemacs-describe-major-mode)
  (define-key help-map (kbd "o") 'nil)  ; ergoemacs-where-is-old-binding
  (define-key help-map (kbd "h") 'nil) ; view-hello-file
  )

(global-set-key (kbd "<menu>") 'nil)

(global-set-key (kbd "<menu> RET") 'smex) ; in ErgoEmacs
(global-set-key (kbd "<menu> <return>") 'smex) ; in ErgoEmacs
(global-set-key (kbd "<menu> <backspace>") 'delete-indentation)
(global-set-key (kbd "<menu> <tab>") 'indent-region)

(global-set-key (kbd "<menu> <f2>") 'ergoemacs-cut-all)
(global-set-key (kbd "<menu> <f3>") 'ergoemacs-copy-all)
(global-set-key (kbd "<menu> <f9>") 'linum-mode)
(global-set-key (kbd "<menu> <f10>") 'whitespace-mode)
(global-set-key (kbd "<menu> <f11>") 'calc)
(global-set-key (kbd "<menu> <f12>") 'shell)

;; (global-set-key (kbd "<menu> <tab>") 'yas/expand)

;xah-cycle-camel-style-case

(global-set-key (kbd "<menu> .") 'nil)
(global-set-key (kbd "<menu> '") 'nil)
(global-set-key (kbd "<menu> ,") 'shell-command-on-region)
(global-set-key (kbd "<menu> -") 'xc-comment-smart) ; ★★★ , comment-dwim

(global-set-key (kbd "<menu> /") 'nil)
(global-set-key (kbd "<menu> 0") 'nil)
(global-set-key (kbd "<menu> 1") 'nil)
(global-set-key (kbd "<menu> 2") 'delete-window) ; ★★★
(global-set-key (kbd "<menu> 3") 'delete-other-windows) ; ★★★
(global-set-key (kbd "<menu> 4") 'nil)
(global-set-key (kbd "<menu> 4") 'split-window-vertically) ; ★★★
(global-set-key (kbd "<menu> 5") 'shell-command) ; ★★★
(global-set-key (kbd "<menu> 6") 'run-current-file) ;; immediate, but dangerous eval-buffer
(global-set-key (kbd "<menu> 7") 'xah-open-file-at-cursor) ; ★★★ find-file-at-point
(global-set-key (kbd "<menu> 8") 'dired-jump) ; ★★★
(global-set-key (kbd "<menu> 9") 'ispell-word) ; ★★★
(global-set-key (kbd "<menu> ;") 'nil)
(global-set-key (kbd "<menu> =") 'nil)
(global-set-key (kbd "<menu> SPC") 'set-mark-command) ; ★★★
(global-set-key (kbd "<menu> [") 'remove-square-brackets)
(global-set-key (kbd "<menu> \\") 'xah-escape-quotes)
(global-set-key (kbd "<menu> `") 'make-backup)
(global-set-key (kbd "<menu> a") 'mark-whole-buffer) ; ★★★
(global-set-key (kbd "<menu> b") 'xah-shell-commands)
(global-set-key (kbd "<menu> c") 'xah-open-file-fast)

; (global-set-key (kbd "<menu> e") 'nil)  ; mode specific
 (global-set-key (kbd "<menu> f") 'copy-file-path)

; ~/git/xah_emacs_init/xah_emacs_keybinding_shift_switch.el
(progn
  (global-set-key (kbd "<menu> g") 'nil)
)

(progn
;  (global-set-key (kbd "<menu> h") help-map) ; ★★★
  (global-set-key (kbd "<menu> h") 'nil)
  (global-set-key (kbd "<menu> h `") 'elisp-index-search)

  (global-set-key (kbd "<menu> h 0") 'lookup-all-dictionaries)
  (global-set-key (kbd "<menu> h 1") 'describe-function)
  (global-set-key (kbd "<menu> h 2") 'describe-variable)
  (global-set-key (kbd "<menu> h 3") 'man)
  (global-set-key (kbd "<menu> h 4") 'describe-char)
  (global-set-key (kbd "<menu> h 5") 'nil)
  (global-set-key (kbd "<menu> h 6") 'nil)
  (global-set-key (kbd "<menu> h 7") 'lookup-google)
  (global-set-key (kbd "<menu> h 8") 'lookup-wikipedia)
  (global-set-key (kbd "<menu> h 9") 'lookup-word-definition)

  (global-set-key (kbd "<menu> h a") 'apropos-command)
  (global-set-key (kbd "<menu> h b") 'describe-bindings)
  (global-set-key (kbd "<menu> h c") 'describe-char)
  (global-set-key (kbd "<menu> h C") 'describe-coding-system)
  (global-set-key (kbd "<menu> h d") 'apropos-documentation)
  (global-set-key (kbd "<menu> h e") 'view-echo-area-messages)
  (global-set-key (kbd "<menu> h f") 'describe-function)
  (global-set-key (kbd "<menu> h F") 'Info-goto-emacs-command-node)
  (global-set-key (kbd "<menu> h i") 'info)
  (global-set-key (kbd "<menu> h I") 'describe-input-method)
  (global-set-key (kbd "<menu> h I") 'Info-goto-emacs-key-command-node)
  (global-set-key (kbd "<menu> h k") 'describe-key)
  (global-set-key (kbd "<menu> h l") 'view-lossage)
  (global-set-key (kbd "<menu> h L") 'describe-language-environment)
  (global-set-key (kbd "<menu> h m") 'ergoemacs-describe-major-mode)
  (global-set-key (kbd "<menu> h n") 'view-emacs-news)
  (global-set-key (kbd "<menu> h p") 'finder-by-keyword)
  (global-set-key (kbd "<menu> h r") 'info-emacs-manual)
  (global-set-key (kbd "<menu> h s") 'describe-syntax)
  (global-set-key (kbd "<menu> h S") 'info-lookup-symbol)
  (global-set-key (kbd "<menu> h v") 'describe-variable)
  (global-set-key (kbd "<menu> h w") 'where-is)
  )

(progn
  (global-set-key (kbd "<menu> i") 'nil)
  (global-set-key (kbd "<menu> i d") 'insert-date)
  (global-set-key (kbd "<menu> i r h") 'ξ-insert-random-hex)
  (global-set-key (kbd "<menu> i r n") 'ξ-insert-random-number)
  (global-set-key (kbd "<menu> i r s") 'ξ-insert-random-string)
  (global-set-key (kbd "<menu> i r u") 'ξ-insert-random-uuid)
 )

(global-set-key (kbd "<menu> j") 'kmacro-start-macro)
(global-set-key (kbd "<menu> k") 'kmacro-end-macro)
(global-set-key (kbd "<menu> l") 'recenter-top-bottom)

(global-set-key (kbd "<menu> m") "_")

(global-set-key (kbd "<menu> n") 'nil)
(global-set-key (kbd "<menu> n y") 'visual-line-mode)
(global-set-key (kbd "<menu> n 3") 'xah-elisp-mode)
(global-set-key (kbd "<menu> n 4") 'xah-php-mode)
(global-set-key (kbd "<menu> n 5") 'xah-html-mode)

(global-set-key (kbd "<menu> o") 'nil)
(global-set-key (kbd "<menu> o c") 'bookmark-bmenu-list)
(global-set-key (kbd "<menu> o g") 'ibuffer)
(global-set-key (kbd "<menu> o h") 'recentf-open-files) ; ★★★
(global-set-key (kbd "<menu> o t") 'ido-switch-buffer)  ; ★★★
(global-set-key (kbd "<menu> o d") 'ergoemacs-open-in-desktop)

(global-set-key (kbd "<menu> p") 'nil)

(global-set-key (kbd "<menu> q") 'quoted-insert) ; ★★★
(global-set-key (kbd "<menu> r") 'nil)
(global-set-key (kbd "<menu> r j") 'copy-to-register-1)
(global-set-key (kbd "<menu> r k") 'paste-from-register-1)
(global-set-key (kbd "<menu> r '") 'xah-replace-straight-quotes)
(global-set-key (kbd "<menu> r ,") 'xah-remove-punctuation-trailing-redundant-space)
(global-set-key (kbd "<menu> r .") 'xah-convert-english-chinese-punctuation)
(global-set-key (kbd "<menu> r d") 'delete-matching-lines) ; ★★
(global-set-key (kbd "<menu> r e") 'query-replace) ; ★★★
(global-set-key (kbd "<menu> r f") 'xah-find-text)
(global-set-key (kbd "<menu> r l") 'list-matching-lines) ; ★★★
(global-set-key (kbd "<menu> r p") 'xah-convert-asian/ascii-space)
(global-set-key (kbd "<menu> r r") 'xah-find-replace-text)
(global-set-key (kbd "<menu> r u") 'query-replace-regexp)
(global-set-key (kbd "<menu> s") 'save-buffer) ; ★★★

(progn
  (global-set-key (kbd "<menu> t") 'nil)
  (global-set-key (kbd "<menu> t c") 'xah-cite)
  (global-set-key (kbd "<menu> t f") 'xah-open-file-from-clipboard)
  (global-set-key (kbd "<menu> t l") 'xah-clean-whitespace)
  (global-set-key (kbd "<menu> t r") 'repeat-complex-command)
  (global-set-key (kbd "<menu> t s") 'title-case-string-region-or-line)
  (global-set-key (kbd "<menu> t w") 'delete-trailing-whitespace)
  (global-set-key (kbd "<menu> t b") 'flyspell-buffer)
  (global-set-key (kbd "<menu> <delete>") 'delete-current-file)
)

(progn
  (global-set-key (kbd "<menu> u") 'nil) ;
  (global-set-key (kbd "<menu> u -") "—") ; EM DASH
  (global-set-key (kbd "<menu> u ,") 'insert-pair-greater-less)

  (global-set-key (kbd "<menu> u RET") 'xah-insert-unicode)

  (global-set-key (kbd "<menu> u . <down>") "⇓")
  (global-set-key (kbd "<menu> u . <left>") "⇐")
  (global-set-key (kbd "<menu> u . <right>") "⇒")
  (global-set-key (kbd "<menu> u . <up>") "⇑")
  (global-set-key (kbd "<menu> u . b") 'insert-pair-white-lenticular-bracket〖〗)
  (global-set-key (kbd "<menu> u . m") 'insert-pair-white-corner-bracket『』)
  (global-set-key (kbd "<menu> u . w") 'insert-pair-double-angle-bracket《》)
  (global-set-key (kbd "<menu> u 2") "¤")
  (global-set-key (kbd "<menu> u 3") "◇") ; white diamond
  (global-set-key (kbd "<menu> u 4") "◆") ; black diamond
  (global-set-key (kbd "<menu> u 5") "🎶") ; MULTIPLE MUSICAL NOTES
  (global-set-key (kbd "<menu> u 7") "＆") ; full width ampersand
  (global-set-key (kbd "<menu> u 8") "•") ; bullet
  (global-set-key (kbd "<menu> u 9") "⭑") ; BLACK SMALL STAR
  (global-set-key (kbd "<menu> u <down>") "↓")
  (global-set-key (kbd "<menu> u <left>") "←")
  (global-set-key (kbd "<menu> u <right>") "→")
  (global-set-key (kbd "<menu> u <up>") "↑")
  (global-set-key (kbd "<menu> u SPC") (lambda () (interactive) (insert " "))) ;insert non-breaking space
  (global-set-key (kbd "<menu> u \\") "、") ; IDEOGRAPHIC COMMA
  (global-set-key (kbd "<menu> u b") 'insert-pair-black-lenticular-bracket【】)
  (global-set-key (kbd "<menu> u c") "=") ; equal
  (global-set-key (kbd "<menu> u d") 'insert-pair-double-curly-quote“”)
  (global-set-key (kbd "<menu> u f") 'insert-pair-single-straight-quote)
  (global-set-key (kbd "<menu> u g") 'insert-pair-double-straight-quote)
  (global-set-key (kbd "<menu> u h") 'insert-pair-brace)              ;{}
  (global-set-key (kbd "<menu> u i") 'insert-pair-single-curly-quote‘’)
  (global-set-key (kbd "<menu> u l") "…") ; HORIZONTAL ELLIPSIS
  (global-set-key (kbd "<menu> u m") 'insert-pair-corner-bracket「」)
  (global-set-key (kbd "<menu> u n") 'insert-pair-bracket)            ;[]
  (global-set-key (kbd "<menu> u p") 'insert-pair-double-angle-quote«»)
  (global-set-key (kbd "<menu> u r") "+") ; plus
  (global-set-key (kbd "<menu> u t") 'insert-pair-paren)              ;()
  (global-set-key (kbd "<menu> u u") 'nil)
  (global-set-key (kbd "<menu> u w") 'insert-pair-angle-bracket〈〉)
  (global-set-key (kbd "<menu> u x") 'insert-pair-tortoise-shell-bracket〔〕)
  (global-set-key (kbd "<menu> u y") 'insert-pair-single-angle-quote‹›)
  )

(global-set-key (kbd "<menu> v") 'nil)
(global-set-key (kbd "<menu> w") 'nil) ; ★★★
(global-set-key (kbd "<menu> x") 'nil) ;
(global-set-key (kbd "<menu> y") 'universal-argument) ; ★★
(global-set-key (kbd "<menu> z") 'nil)


;; special keys

(global-set-key (kbd "<insert>") 'ergoemacs-switch-to-next-frame)


;; mode-specific

(defun xah-html-mode-keys ()
  "Modify keymaps used by `html-mode'."
  ;; .p gc
  ;; eu ht

  (local-set-key (kbd "<f6>") 'xah-browse-url-of-buffer)

  (local-set-key (kbd "<C-right>") 'xhm-skip-tag-forward)
  (local-set-key (kbd "<C-left>") 'xhm-skip-tag-backward)

  (local-set-key (kbd "<menu> e 0") 'xhm-remove-html-tags)
  (local-set-key (kbd "<menu> e 7") 'xhm-htmlize-or-de-precode)
  (local-set-key (kbd "<menu> e 8") 'xhm-get-precode-make-new-file)
  (local-set-key (kbd "<menu> e c") 'xhm-make-citation)
  (local-set-key (kbd "<menu> e k") 'xhm-htmlize-keyboard-shortcut-notation)
  (local-set-key (kbd "<menu> e l 6") 'xhm-source-url-linkify)
  (local-set-key (kbd "<menu> e l .") 'xhm-lines-to-html-list)
  (local-set-key (kbd "<menu> e l u") 'xhm-wrap-url)
  (local-set-key (kbd "<menu> e l w") 'xhm-wikipedia-linkify)
  (local-set-key (kbd "<menu> e m") 'xhm-pre-source-code)
  (local-set-key (kbd "<menu> e p") 'xhm-wrap-p-tag)
  (local-set-key (kbd "<menu> e r ,") 'xhm-replace-html-chars-to-unicode)
  (local-set-key (kbd "<menu> e r .") 'xhm-replace-html-&<>-to-entities)
  (local-set-key (kbd "<menu> e r 3") 'xhm-update-title)
  (local-set-key (kbd "<menu> e r e") 'xhm-htmlize-elisp-keywords)
  (local-set-key (kbd "<menu> e r k") 'xhm-emacs-to-windows-kbd-notation)
  (local-set-key (kbd "<menu> e r m") 'xhm-make-html-table)
  (local-set-key (kbd "<menu> e t r") 'xhm-rename-html-inline-image)
  (local-set-key (kbd "<menu> e t u") 'xhm-extract-url)
  (local-set-key (kbd "<menu> e u") 'xhm-wrap-html-tag)

  (local-set-key (kbd "<menu> e 5") 'mark-unicode)
  (local-set-key (kbd "<menu> e a") 'xwe-annotate)
  (local-set-key (kbd "<menu> e b") 'make-blogger-entry)
  (local-set-key (kbd "<menu> e d") 'insert-date-tag)
  (local-set-key (kbd "<menu> e e") 'nil)
  (local-set-key (kbd "<menu> e f") 'xah-copy-url-current-file)
  (local-set-key (kbd "<menu> e g") 'xah-browse-url-of-buffer)
  (local-set-key (kbd "<menu> e h") 'xah-all-linkify)
  (local-set-key (kbd "<menu> e i") 'nil)
  (local-set-key (kbd "<menu> e j") 'nil)
  (local-set-key (kbd "<menu> e l c") 'xwe-chinese-linkify)
  (local-set-key (kbd "<menu> e l d") 'perldoc-ref-linkify)
  (local-set-key (kbd "<menu> e l e") 'emacs-ref-linkify)
  (local-set-key (kbd "<menu> e l f") 'full-size-img-linkify)
  (local-set-key (kbd "<menu> e l i") 'image-linkify)
  (local-set-key (kbd "<menu> e l j") 'image-file-to-html-figure-tag)
  (local-set-key (kbd "<menu> e l p") 'php-ref-linkify)
  (local-set-key (kbd "<menu> e l t") 'xwe-word-etymology-linkify)
  (local-set-key (kbd "<menu> e l z") 'amazon-linkify)
  (local-set-key (kbd "<menu> e n") 'nil)
  (local-set-key (kbd "<menu> e o") 'nil)
  (local-set-key (kbd "<menu> e q") 'nil)
  (local-set-key (kbd "<menu> e r 4") 'xahsite-update-article-timestamp)
  (local-set-key (kbd "<menu> e r c") 'code-bracket-to-html-tag)
  (local-set-key (kbd "<menu> e r t") 'title-bracket-to-html-tag)
  (local-set-key (kbd "<menu> e s") 'nil)
  (local-set-key (kbd "<menu> e t") 'nil)
  (local-set-key (kbd "<menu> e t a") 'xah-make-atom-entry)
  (local-set-key (kbd "<menu> e t l") 'xah-add-to-related-links)
  (local-set-key (kbd "<menu> e v") 'nil)
  (local-set-key (kbd "<menu> e w") (lambda () (interactive) (xhm-wrap-html-tag "b" "w")))
  (local-set-key (kbd "<menu> e x") 'nil)
  (local-set-key (kbd "<menu> e y") 'nil)
  (local-set-key (kbd "<menu> e z") 'nil)

)

(add-hook 'html-mode-hook 'xah-html-mode-keys)
(add-hook 'xah-html-mode-hook 'xah-html-mode-keys)
(add-hook 'nxml-mode-hook 'xah-html-mode-keys)

(defun xah-eval-defun ()
  "like `eval-defun' but doesn't need proper indentation for it to work.
Still, the code isn't 100% correct.
"
  (interactive)
  (save-excursion
    (search-backward "(defun")
    ;;    (mark-sexp)
    ;;    (eval-region (region-beginning) (region-end))
    (forward-sexp)
    (call-interactively 'eval-last-sexp)
    )
  )

;; (defun xah-cperl-mode-keys ()
;;   "Modify keymaps used by cperl-mode."
;;   (local-set-key (kbd ")") 'self-insert-command)
;;   (local-set-key (kbd "]") 'self-insert-command)
;; )
;; (add-hook 'cperl-mode-hook 'xah-cperl-mode-keys)

;; ;(setq mybuf (get-buffer-create "*show commands*"))
;; (defun xx ()
;;   "tttttt"
;;   (let ()
;;     (princ (format "%s\n" last-command) mybuf )
;;     )
;;   ;;(message "%s" last-command)
;;   )
;; (add-hook 'post-command-hook 'xx)
;; (remove-hook 'post-command-hook 'xx)

(defun xah-elisp-mode-keys ()
  "Modify keymaps used by lisp mode."
  ;; .p gc
  ;; eu ht

  (local-set-key (kbd "<menu> e t") 'eval-last-sexp)
  (local-set-key (kbd "<menu> e f") 'xah-eval-defun)
)

(add-hook 'xah-elisp-mode-hook 'xah-elisp-mode-keys)
(add-hook 'emacs-lisp-mode-hook 'xah-elisp-mode-keys)

;; (unload-feature 'sgml-mode)
;; (remove-hook 'html-mode-hook 'xah-html-mode-keys)

;; (defun xah-isearch-mode-keys ()
;;   "my keybindings for `isearch-mode'.
;; For `isearch-mode-hook'."
;;   (define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
;;   )
;; (add-hook 'isearch-mode-hook 'xah-isearch-mode-keys )

(defun xah-rcirc-mode-keys ()
  "my keybindings for `rcirc'.
For `rcirc-mode-hook'."
  (local-set-key (kbd "<f11>") 'rcirc-insert-prev-input)
  (local-set-key (kbd "<f12>") 'rcirc-insert-next-input)
  )
(add-hook 'rcirc-mode-hook 'xah-rcirc-mode-keys)

(defun xah-org-mode-keys ()
  "my keybindings for `org-mode'.
For `org-mode-hook'."
  (local-set-key (kbd "<M-up>") 'org-metaup)
  (local-set-key (kbd "<M-down>") 'org-metadown)
  (local-set-key (kbd "<M-left>") 'org-metaleft)
  (local-set-key (kbd "<M-right>") 'org-metaright)
  )
(add-hook 'org-mode-hook 'xah-org-mode-keys)

(defun xah-Info-mode-keys ()
  "my keybindings for `Info-mode'.
For `Info-mode-hook'."
  (local-set-key (kbd "<menu> e g") 'xah-view-emacs-manual-in-browser)
  )
(add-hook 'Info-mode-hook 'xah-Info-mode-keys)

(defun xah-dired-mode-keys ()
  "Modify keymaps used by `dired'."
;;  (define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file) ; was dired-advertised-find-file
;;  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file ".."))) ; was dired-up-directory
  ;; (define-key dired-mode-map (kbd "M-$") nil) ; was dired-up-directory
;; (local-set-key (kbd "6") 'dired-up-directory)
  (when (>= emacs-major-version 23)
;    (define-key dired-mode-map (kbd "M-s") 'isearch-forward)
;    (define-key dired-mode-map (kbd "M-S") 'isearch-backward)
    (define-key dired-mode-map (kbd "<menu> e 8") 'wdired-change-to-wdired-mode)
    )
  )

(add-hook 'dired-mode-hook 'xah-dired-mode-keys)


;;;; misc

;; (load (fullpath-relative-to-current-file "xah_emacs_keybinding_unset_keys.el"))

    ;; ("<f1> 6" lookup-all-dictionaries)
    ;; ("<f1> 7" lookup-google)
    ;; ("<f1> 8" lookup-wikipedia)
    ;; ("<f1> 9" lookup-word-definition)
    ;; ("<f1> 0" lookup-answers.com)
    ;; ("<f1> [" lookup-word-dict-org)
    ;; ("<f1> ]" lookup-wiktionary)

(global-set-key (kbd "<f1> <f2>") 'helm-mini)

;; (load (fullpath-relative-to-current-file "xah_emacs_keybinding_number_pad"))
(load (fullpath-relative-to-current-file "xah_emacs_keybinding_number_pad_number"))
;; (load (fullpath-relative-to-current-file "xah_emacs_keybinding_truly_ergonomic"))
(load (fullpath-relative-to-current-file "xah_emacs_unicode_input"))

(global-set-key (kbd "C--") 'xah-cycle-camel-style-case)
(global-set-key (kbd "C-1") 'cycle-font-backward)
(global-set-key (kbd "C-2") 'cycle-font-forward)
(global-set-key (kbd "C-3") 'cycle-font-2)
(global-set-key (kbd "C-4") 'xah-convert-fullwidth-chars)
(global-set-key (kbd "C-5") 'xah-convert-latin-alphabet-gothic)
(global-set-key (kbd "C-6") 'nil)
(global-set-key (kbd "C-7") 'xah-cycle-hyphen-underscore-space)
(global-set-key (kbd "C-8") 'nil)
(global-set-key (kbd "C-9") 'nil)

;; ;; (global-set-key (kbd "C-a") 'nil) ; select all
;; (global-set-key (kbd "C-b") 'nil)
;; ;; (global-set-key (kbd "C-c") 'nil) ; mode specific
;; (global-set-key (kbd "C-d") 'nil)
;; (global-set-key (kbd "C-e") 'nil)
;; ;; (global-set-key (kbd "C-f") 'nil) ; find
;; ;; (global-set-key (kbd "C-g") 'nil) ; cancel
;; ;; (global-set-key (kbd "C-h") 'nil) ; help
;; ;; (global-set-key (kbd "C-i") 'nil) ; tab
;; ;; (global-set-key (kbd "C-j") 'nil) ; newline
;; (global-set-key (kbd "C-k") 'nil)
;; (global-set-key (kbd "C-l") 'nil)
;; ;; (global-set-key (kbd "C-m") 'nil) ; return
;; ;; (global-set-key (kbd "C-n") 'nil) ; new
;; ;; (global-set-key (kbd "C-o") 'nil) ; open
;; (global-set-key (kbd "C-p") 'nil)
;; ;; (global-set-key (kbd "C-q") 'nil) ; quote
;; (global-set-key (kbd "C-r") 'nil)
;; (global-set-key (kbd "C-s") 'nil)
;; (global-set-key (kbd "C-t") 'nil)
;; (global-set-key (kbd "C-u") 'nil)
;; (global-set-key (kbd "C-v") 'nil)
;; ;; (global-set-key (kbd "C-w") 'nil) ; close
;; ;; (global-set-key (kbd "C-x") 'nil) C-x
;; ;; (global-set-key (kbd "C-y") 'nil)
;; (global-set-key (kbd "C-z") 'nil)

;(global-set-key (kbd "C-o") 'backward-sexp)
;(global-set-key (kbd "C-u") 'forward-sexp)
;(global-set-key (kbd "C-e") 'down-list)
;(global-set-key (kbd "C-.") 'backward-up-list)

;(global-unset-key (kbd "C-+") )         ; text-scale-increase
;(global-unset-key (kbd "C--") )         ; text-scale-decrease
;(global-unset-key (kbd "C-a") )         ; mark-whole-buffer
;(global-unset-key (kbd "C-s") )         ; save
;(global-unset-key (kbd "C-o") )         ; open
;(global-unset-key (kbd "C-0") ) ; text-scale-normal-size
;(global-unset-key (kbd "M-5") )
;(global-unset-key (kbd "M-3") )
;(global-unset-key (kbd "M-4") )
;(global-unset-key (kbd "M--") )
;(global-unset-key (kbd "M-%") )
;(global-unset-key (kbd "M-l") )         ; recenter-top-bottom
;(global-unset-key (kbd "M-0") )         ; delete-window
;(global-unset-key (kbd "C-S-t") )       ; ergoemacs-open-last-closed
;(global-unset-key (kbd "C-u") )       ; universal-argument

(global-set-key (kbd "M-\"") 'xah-compact-uncompact-Block)

(global-set-key (kbd "M-2") 'delete-window)
(global-set-key (kbd "M-9") 'ergoemacs-select-text-in-quote)
(global-set-key (kbd "M-s") 'ergoemacs-toggle-letter-case)

(global-set-key (kbd "<home>") 'ergoemacs-backward-open-bracket)
(global-set-key (kbd "<end>") 'ergoemacs-forward-close-bracket)

;; (global-set-key (kbd "<backspace>") 'delete-backward-char)

(global-set-key (kbd "<prior>") 'ergoemacs-backward-block) ; page up
(global-set-key (kbd "<next>") 'ergoemacs-forward-block) ; page down

(global-set-key (kbd "<S-prior>") 'scroll-down) ; page up
(global-set-key (kbd "<S-next>") 'scroll-up) ; page down

;; keys for moving to prev/next code section (form feed; ^L)
(global-set-key (kbd "<C-M-prior>") 'backward-page) ; Ctrl+Alt+PageUp
(global-set-key (kbd "<C-M-next>") 'forward-page)   ; Ctrl+Alt+PageDown

(global-set-key (kbd "<C-M-next>") 'forward-page)   ; Ctrl+Alt+PageDown

(require 'ido)
(define-key ido-file-completion-map (kbd "C-o") 'ido-fallback-command)

;; (defun toggle-menu-key ()
;;   "toggle the value of `w32-apps-modifier' between 'meta and 'nil"
;;   (interactive)
;;   (if (eq w32-apps-modifier 'meta)
;;         (progn (setq w32-apps-modifier 'nil))
;;       (progn (setq w32-apps-modifier 'meta) )
;;       ))

;; ~/web/ergoemacs_org/emacs/gnu_emacs_keybinding_C-x.txt

;; some idea about command categories, in context to chosing keys for them

;; • whether a command has immediate effect, no prompt. ⁖ shell vs delete-matching-lines
;; • whether a command has is safe to run by mistake. ⁖ whitespace-mode vs eval-buffer
;; • whether a command is frequently needed ⁖ few times a min, hour, day

;; idea about key groups
;; all should be sequence of single keys. 2 to 3 keys. All should start with F7. And all commands should be globally useful.
;; • 2 keys vs 3 keys
;; • whether the key ends in a digit key 0 to 9. These probably should be most frequently used, or immediate effect.

;; (ergoemacs-ignore-prev-global) ; Do not honor previously defined global keys. 2013-06-24

(global-set-key (kbd "<S-backspace>") 'delete-char)

