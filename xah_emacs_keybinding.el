;; -*- coding: utf-8 -*-
;; xah's emacs keybinding.

;; jump points to other files
;; • xah_emacs_keybinding.el
;; • xah_emacs_unicode_input.el
;; • xah_emacs_hyper_super_setup.el
;; • xah_emacs_mouse_binding.el
;; • xah_emacs_alias.el
;; • xah_emacs_abbr.el

;; • 〈Dvorak Keyboard Layout〉 http://xahlee.info/comp/dvorak_keyboard_layout.html
;; • 〈ErgoEmacs Keybinding〉 http://ergoemacs.org/emacs/ergonomic_emacs_keybinding.html
;; • 〈xah_autohotkey.ahk〉 http://xahlee.info/mswin/autohotkey.html
;; • 〈Emacs: How to define Hyper ＆ Super Keys〉 http://ergoemacs.org/emacs/emacs_hyper_super_keys.html
;; • 〈Emacs: Remapping Keys Using key-translation-map〉 http://ergoemacs.org/emacs/emacs_key-translation-map.html
;; • 〈Emacs: Add Custom Keys to Enhance Productivity〉 http://ergoemacs.org/emacs/emacs_useful_user_keybinding.html

;; Xah Lee
;; created: 2007-06.


;; generic

(define-key key-translation-map (kbd "<apps>") (kbd "<menu>"))
(define-key key-translation-map (kbd "<f6>") (kbd "<menu>"))
(define-key key-translation-map (kbd "<henkan>") (kbd "<delete>")) ; henkan is the 変換 key on Japanese keyboard for “do convert”

;; in linux, by default, <lwindow> is Super,  s-
;; linux, the menu/apps key is <menu>
;; windows, the menu/apps key is <apps>

;; (global-set-key (kbd "<f8>") ctl-x-map)
;; (global-set-key (kbd "<f8>") mode-specific-map)

(global-set-key (kbd "<f9>") 'ergoemacs-switch-to-next-frame)
(global-set-key (kbd "<f10>") 'ergoemacs-close-current-buffer)
(global-set-key (kbd "]") 'ergoemacs-close-current-buffer)

;; (global-set-key (kbd "<f9>") 'ergoemacs-move-cursor-next-pane)
;; (global-set-key (kbd "<f8>") 'ergoemacs-switch-to-next-frame)
;; (global-set-key (kbd "<f9>") 'split-window-vertically)
(global-set-key (kbd "<f11>") 'ergoemacs-next-user-buffer)
(global-set-key (kbd "<f12>") 'ergoemacs-previous-user-buffer)

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
;; .p gc
;; eu ht

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

(global-set-key (kbd "<menu> <return>") 'smex)
(global-set-key (kbd "<menu> RET") 'smex)
(global-set-key (kbd "<menu> <delete>") 'delete-current-file)
(global-set-key (kbd "<menu> <f2>") 'ergoemacs-cut-all)
(global-set-key (kbd "<menu> <f3>") 'ergoemacs-copy-all)

;; (global-set-key (kbd "<menu> <tab>") 'yas/expand)
;; (global-set-key (kbd "<menu> <tab>") 'yas/expand)

;cycle-camel-style-case

(global-set-key (kbd "<menu> '") 'nil)
(global-set-key (kbd "<menu> ,") 'shell-command-on-region)
(global-set-key (kbd "<menu> -") 'xc-comment-smart) ; ★★★ , comment-dwim
(global-set-key (kbd "<menu> .") 'shell-command)   ; ★★★
(global-set-key (kbd "<menu> <backspace>") 'delete-indentation)
(global-set-key (kbd "<menu> <tab>") 'indent-region)

(global-set-key (kbd "<menu> /") 'nil)
(global-set-key (kbd "<menu> 0") 'delete-window)
(global-set-key (kbd "<menu> 1") 'copy-to-register-1)
(global-set-key (kbd "<menu> 2") 'paste-from-register-1)
(global-set-key (kbd "<menu> 3") 'delete-other-windows) ; ★★★
(global-set-key (kbd "<menu> 4") 'split-window-vertically) ; ★★★
(global-set-key (kbd "<menu> 5") 'nil) ; ★★★
(global-set-key (kbd "<menu> 6") 'run-current-file) ;; immediate, but dangerous eval-buffer
(global-set-key (kbd "<menu> 7") 'xah-open-file-at-cursor) ; ★★★ find-file-at-point
(global-set-key (kbd "<menu> 8") 'dired-jump)              ; ★★★
(global-set-key (kbd "<menu> 9") 'ispell-word) ; ★★★
(global-set-key (kbd "<menu> ;") 'nil)
(global-set-key (kbd "<menu> =") 'nil)
(global-set-key (kbd "<menu> SPC") 'set-mark-command) ; ★★★
(global-set-key (kbd "<menu> [") 'remove-square-brackets)
(global-set-key (kbd "<menu> \\") 'escape-quotes)
(global-set-key (kbd "<menu> `") 'make-backup)
(global-set-key (kbd "<menu> a") 'mark-whole-buffer) ; ★★
(global-set-key (kbd "<menu> b") 'flyspell-buffer) ; ★★
(global-set-key (kbd "<menu> c") 'nil)
(global-set-key (kbd "<menu> d") 'ergoemacs-open-in-desktop)
; (global-set-key (kbd "<menu> e") 'nil)  ; mode specific
(global-set-key (kbd "<menu> f") 'copy-file-path)

(global-set-key (kbd "<menu> g") 'nil)
(global-set-key (kbd "<menu> g '") "\"")
(global-set-key (kbd "<menu> g ,") "<")
(global-set-key (kbd "<menu> g -") "_")
(global-set-key (kbd "<menu> g .") ">")
(global-set-key (kbd "<menu> g /") "?")
(global-set-key (kbd "<menu> g ;") ":")
(global-set-key (kbd "<menu> g =") "+")
(global-set-key (kbd "<menu> g \\") "|")
(global-set-key (kbd "<menu> g `") "~")

(global-set-key (kbd "<menu> g 0") ")")
(global-set-key (kbd "<menu> g 1") "!")
(global-set-key (kbd "<menu> g 2") "@")
(global-set-key (kbd "<menu> g 3") "#")
(global-set-key (kbd "<menu> g 4") "$")
(global-set-key (kbd "<menu> g 5") "%")
(global-set-key (kbd "<menu> g 6") "^")
(global-set-key (kbd "<menu> g 7") "&")
(global-set-key (kbd "<menu> g 8") "*")
(global-set-key (kbd "<menu> g 9") "(")

(global-set-key (kbd "<menu> g a") "A")
(global-set-key (kbd "<menu> g b") "B")
(global-set-key (kbd "<menu> g c") "C")
(global-set-key (kbd "<menu> g d") "D")
(global-set-key (kbd "<menu> g e") "E")
(global-set-key (kbd "<menu> g f") "F")
(global-set-key (kbd "<menu> g g") "G")
(global-set-key (kbd "<menu> g h") "H")
(global-set-key (kbd "<menu> g i") "I")
(global-set-key (kbd "<menu> g j") "J")
(global-set-key (kbd "<menu> g k") "K")
(global-set-key (kbd "<menu> g l") "L")
(global-set-key (kbd "<menu> g m") "M")
(global-set-key (kbd "<menu> g n") "N")
(global-set-key (kbd "<menu> g o") "O")
(global-set-key (kbd "<menu> g p") "P")
(global-set-key (kbd "<menu> g q") "Q")
(global-set-key (kbd "<menu> g r") "R")
(global-set-key (kbd "<menu> g s") "S")
(global-set-key (kbd "<menu> g t") "T")
(global-set-key (kbd "<menu> g u") "U")
(global-set-key (kbd "<menu> g v") "V")
(global-set-key (kbd "<menu> g w") "W")
(global-set-key (kbd "<menu> g x") "X")
(global-set-key (kbd "<menu> g y") "Y")
(global-set-key (kbd "<menu> g z") "Z")

(global-set-key (kbd "<menu> h") help-map) ; ★★★
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

(global-set-key (kbd "<menu> i") 'nil)
(global-set-key (kbd "<menu> i d") 'insert-date)
(global-set-key (kbd "<menu> i r h") 'insert-random-hex)
(global-set-key (kbd "<menu> i r n") 'insert-random-number)
(global-set-key (kbd "<menu> i r s") 'insert-random-string)
(global-set-key (kbd "<menu> i r u") 'insert-random-uuid)
(global-set-key (kbd "<menu> i t") 'insert-date-time)
(global-set-key (kbd "<menu> j") 'kmacro-start-macro)
(global-set-key (kbd "<menu> k") 'kmacro-end-macro)
(global-set-key (kbd "<menu> l") 'recenter-top-bottom)
(global-set-key (kbd "<menu> m") 'nil)
(global-set-key (kbd "<menu> m c") 'calc)
(global-set-key (kbd "<menu> m e") 'xah-elisp-mode)
(global-set-key (kbd "<menu> m h") 'xah-html-mode)
(global-set-key (kbd "<menu> m l") 'global-linum-mode)
(global-set-key (kbd "<menu> m p") 'php-mode)
(global-set-key (kbd "<menu> m s") 'shell)
(global-set-key (kbd "<menu> m t") 'text-mode)
(global-set-key (kbd "<menu> m v") 'visual-line-mode)
(global-set-key (kbd "<menu> m w") 'whitespace-mode)
(global-set-key (kbd "<menu> m x") 'nxml-mode)
(global-set-key (kbd "<menu> n") 'ergoemacs-new-empty-buffer) ; ★★★
(global-set-key (kbd "<menu> o") 'nil)
(global-set-key (kbd "<menu> o SPC") 'xah-open-file-fast)
(global-set-key (kbd "<menu> o b") 'ibuffer)
(global-set-key (kbd "<menu> o f") 'ido-find-file)
(global-set-key (kbd "<menu> o l") 'bookmark-bmenu-list)
(global-set-key (kbd "<menu> o r") 'recentf-open-files) ; ★★★
(global-set-key (kbd "<menu> o s") 'ido-switch-buffer)  ; ★★★
(global-set-key (kbd "<menu> p") 'nil)
(global-set-key (kbd "<menu> q") 'quoted-insert) ; ★★★
(global-set-key (kbd "<menu> r") 'nil)
(global-set-key (kbd "<menu> r '") 'replace-straight-quotes)
(global-set-key (kbd "<menu> r ,") 'remove-punctuation-trailing-redundant-space)
(global-set-key (kbd "<menu> r .") 'convert-english-chinese-punctuation)
(global-set-key (kbd "<menu> r d") 'delete-matching-lines) ; ★★
(global-set-key (kbd "<menu> r e") 'query-replace) ; ★★★
(global-set-key (kbd "<menu> r f") 'xah-find-text)
(global-set-key (kbd "<menu> r l") 'list-matching-lines) ; ★★★
(global-set-key (kbd "<menu> r p") 'convert-ideographic/ascii-space)
(global-set-key (kbd "<menu> r r") 'xah-find-replace-text)
(global-set-key (kbd "<menu> r u") 'query-replace-regexp)
(global-set-key (kbd "<menu> s") 'save-buffer) ; ★★★
(global-set-key (kbd "<menu> t") 'nil)
(global-set-key (kbd "<menu> t c") 'xah-cite)
(global-set-key (kbd "<menu> t f") 'xah-open-file-from-clipboard)
(global-set-key (kbd "<menu> t l") 'xah-clean-whitespace)
(global-set-key (kbd "<menu> t n") 'make-frame-command)
(global-set-key (kbd "<menu> t o") 'ergoemacs-open-last-closed)
(global-set-key (kbd "<menu> t r") 'repeat-complex-command)
(global-set-key (kbd "<menu> t s") 'title-case-string-region-or-line)
(global-set-key (kbd "<menu> t w") 'delete-trailing-whitespace)

(global-set-key (kbd "<menu> u") 'nil) ;
(global-set-key (kbd "<menu> u -") "—") ; EM DASH
(global-set-key (kbd "<menu> u ,") 'insert-pair-greater-less)

(global-set-key (kbd "<menu> u . <down>") "⇓")
(global-set-key (kbd "<menu> u . <left>") "⇐")
(global-set-key (kbd "<menu> u . <right>") "⇒")
(global-set-key (kbd "<menu> u . <up>") "⇑")
(global-set-key (kbd "<menu> u . b") 'insert-pair-white-lenticular-bracket〖〗)
(global-set-key (kbd "<menu> u . m") 'insert-pair-white-corner-bracket『』)
(global-set-key (kbd "<menu> u . w") 'insert-pair-double-angle-bracket《》)
(global-set-key (kbd "<menu> u 3") "◇") ; white diamond
(global-set-key (kbd "<menu> u 4") "◆") ; black diamond
(global-set-key (kbd "<menu> u 5") "🎶") ; MULTIPLE MUSICAL NOTES
(global-set-key (kbd "<menu> u 7") "＆") ; full width ampersand
(global-set-key (kbd "<menu> u 8") "•") ; bullet
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

(global-set-key (kbd "<menu> v") 'nil)
(global-set-key (kbd "<menu> w") 'ergoemacs-close-current-buffer) ; ★★★
(global-set-key (kbd "<menu> x") ctl-x-map) ; ★★★
(global-set-key (kbd "<menu> y") 'universal-argument) ; ★★
(global-set-key (kbd "<menu> z") 'nil)

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


;; special keys

(global-set-key (kbd "<insert>") 'ergoemacs-switch-to-next-frame)


(global-set-key (kbd "M-\"") 'xah-compact-uncompact-block)
(global-set-key (kbd "M-2") 'cycle-hyphen-underscore-space)


;; mode-specific

(defun xah-html-mode-keys ()
  "Modify keymaps used by `html-mode'."
  ;; .p gc
  ;; eu ht

  (local-set-key (kbd "<menu> e .") 'xah-browse-url-of-buffer)
  (local-set-key (kbd "<menu> e 0") 'xhm-remove-html-tags)
  (local-set-key (kbd "<menu> e 5") 'mark-unicode)
  (local-set-key (kbd "<menu> e 7") 'htmlize-or-dehtmlize-pre-block)
  (local-set-key (kbd "<menu> e 8") 'get-pre-block-make-new-file)
  (local-set-key (kbd "<menu> e <delete>") 'sgml-delete-tag)
  (local-set-key (kbd "<menu> e <left>") 'sgml-skip-tag-backward)
  (local-set-key (kbd "<menu> e <right>") 'sgml-skip-tag-forward)
  (local-set-key (kbd "<menu> e a") 'xwe-annotate)
  (local-set-key (kbd "<menu> e b") 'make-blogger-entry)
  (local-set-key (kbd "<menu> e c") 'xhm-make-citation)
  (local-set-key (kbd "<menu> e d") 'insert-date-tag)
  (local-set-key (kbd "<menu> e e") 'xah-all-linkify)
  (local-set-key (kbd "<menu> e f") 'xah-copy-url-current-file)
  (local-set-key (kbd "<menu> e k") 'xhm-htmlize-keyboard-shortcut-notation)
  (local-set-key (kbd "<menu> e l 6") 'xhm-source-url-linkify)
  (local-set-key (kbd "<menu> e l c") 'xwe-chinese-linkify)
  (local-set-key (kbd "<menu> e l d") 'perldoc-ref-linkify)
  (local-set-key (kbd "<menu> e l e") 'emacs-ref-linkify)
  (local-set-key (kbd "<menu> e l f") 'full-size-img-linkify)
  (local-set-key (kbd "<menu> e l i") 'image-linkify)
  (local-set-key (kbd "<menu> e l j") 'image-file-to-html-figure-tag)
  (local-set-key (kbd "<menu> e l l") 'xhm-lines-to-html-list)
  (local-set-key (kbd "<menu> e l p") 'php-ref-linkify)
  (local-set-key (kbd "<menu> e l t") 'xwe-word-etymology-linkify)
  (local-set-key (kbd "<menu> e l u") 'xhm-wrap-url)
  (local-set-key (kbd "<menu> e l w") 'xhm-wikipedia-linkify)
  (local-set-key (kbd "<menu> e l z") 'amazon-linkify)
  (local-set-key (kbd "<menu> e m a") 'xah-make-atom-entry)
  (local-set-key (kbd "<menu> e m l") 'xah-add-to-related-links)
  (local-set-key (kbd "<menu> e p") 'xhm-wrap-p-tag)
  (local-set-key (kbd "<menu> e r ,") 'xhm-replace-html-chars-to-unicode)
  (local-set-key (kbd "<menu> e r .") 'xhm-replace-html-chars-to-entities)
  (local-set-key (kbd "<menu> e r 3") 'xhm-update-title)
  (local-set-key (kbd "<menu> e r 4") 'xahsite-update-article-timestamp)
  (local-set-key (kbd "<menu> e r c") 'code-bracket-to-html-tag)
  (local-set-key (kbd "<menu> e r e") 'curly-quotes-to-emacs-function-tag)
  (local-set-key (kbd "<menu> e r k") 'emacs-to-windows-kbd-notation)
  (local-set-key (kbd "<menu> e r m") 'xhm-make-html-table)
  (local-set-key (kbd "<menu> e r t") 'title-bracket-to-html-tag)
  (local-set-key (kbd "<menu> e t c") 'insert-random-color-hsl)
  (local-set-key (kbd "<menu> e t r") 'xhm-rename-html-inline-image)
  (local-set-key (kbd "<menu> e t u") 'xhm-extract-url)
  (local-set-key (kbd "<menu> e u") 'xhm-wrap-html-tag)

  (local-set-key (kbd "<menu> e w") (lambda () (interactive) (xhm-wrap-html-tag "b" "w")))
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

(defun xah-elisp-mode-keys ()
  "Modify keymaps used by lisp mode."
  ;; .p gc
  ;; eu ht

  (local-set-key (kbd "<menu> e t") 'eval-last-sexp)
  (local-set-key (kbd "<menu> e f") 'xah-eval-defun)
)

(add-hook 'xah-elisp-mode-hook 'xah-elisp-mode-keys)
(add-hook 'emacs-lisp-mode-hook 'xah-elisp-mode-keys)

(add-hook 'xah-elisp-mode-hook 'ac-emacs-lisp-mode-setup)

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

(defun xah-dired-mode-keys ()
  "my keybindings for `dired'.
For `dired-mode-hook'."
  (local-set-key (kbd "6") 'dired-up-directory)
  )
(add-hook 'dired-mode-hook 'xah-dired-mode-keys)

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
  (local-set-key (kbd "<menu> e 6") 'xah-view-emacs-manual-in-browser)
  )
(add-hook 'Info-mode-hook 'xah-Info-mode-keys)

(defun xah-dired-mode-keys ()
  "Modify keymaps used by `dired'."
;;  (define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file) ; was dired-advertised-find-file
;;  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file ".."))) ; was dired-up-directory
  (define-key dired-mode-map (kbd "M-$") nil) ; was dired-up-directory
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

;; remove ErgoEmacs's changing font size
(global-unset-key (kbd "C-+") )         ; text-scale-increase
(global-unset-key (kbd "C--") )         ; text-scale-decrease
(global-unset-key (kbd "C-a") )         ; mark-whole-buffer
(global-unset-key (kbd "C-s") )         ; save
(global-unset-key (kbd "C-o") )         ; open
(global-unset-key (kbd "C-0") ) ; text-scale-normal-size
(global-unset-key (kbd "M-5") )
(global-unset-key (kbd "M-3") )
(global-unset-key (kbd "M-4") )
(global-unset-key (kbd "M--") )
(global-unset-key (kbd "M-%") )
(global-unset-key (kbd "M-l") )         ; recenter-top-bottom
(global-unset-key (kbd "M-0") )         ; delete-window
(global-unset-key (kbd "C-S-t") )       ; ergoemacs-open-last-closed
(global-unset-key (kbd "C-u") )       ; universal-argument

(global-set-key (kbd "C--") 'cycle-camel-style-case)
(global-set-key (kbd "C-1") 'cycle-font-backward)
(global-set-key (kbd "C-2") 'cycle-font-forward)
(global-set-key (kbd "C-3") 'cycle-font-2)
(global-set-key (kbd "C-8") 'ergoemacs-extend-selection)

(global-set-key (kbd "C-o") 'backward-sexp)
(global-set-key (kbd "C-u") 'forward-sexp)
(global-set-key (kbd "C-e") 'down-list)
(global-set-key (kbd "C-.") 'backward-up-list)

(global-set-key (kbd "M-3") 'ergoemacs-select-text-in-quote)
(global-set-key (kbd "M-7") 'ergoemacs-select-current-line)
(global-set-key (kbd "M-s") 'ergoemacs-toggle-letter-case)

(global-set-key (kbd "<home>") 'ergoemacs-backward-open-bracket)
(global-set-key (kbd "<end>") 'ergoemacs-forward-close-bracket)

(global-set-key (kbd "<prior>") 'ergoemacs-backward-block) ; page up
(global-set-key (kbd "<next>") 'ergoemacs-forward-block) ; page down

(global-set-key (kbd "<S-prior>") 'scroll-down) ; page up
(global-set-key (kbd "<S-next>") 'scroll-up) ; page down

;; keys for moving to prev/next code section (form feed; ^L)
(global-set-key (kbd "<C-M-prior>") 'backward-page) ; Ctrl+Alt+PageUp
(global-set-key (kbd "<C-M-next>") 'forward-page)   ; Ctrl+Alt+PageDown

(global-set-key (kbd "<C-M-next>") 'forward-page)   ; Ctrl+Alt+PageDown

; idea from eric crosson, 2013-04-06 http://ericscrosson.wordpress.com/2013/04/05/minimizing-keystrokes-required-by-punctuation/
(global-set-key (kbd ",") (lambda() (interactive) (insert ", ")))

(defun toggle-menu-key ()
  "toggle the value of `w32-apps-modifier' between 'meta and 'nil"
  (interactive)
  (if (eq w32-apps-modifier 'meta)
        (progn (setq w32-apps-modifier 'nil))
      (progn (setq w32-apps-modifier 'meta) )
      ))

;; c:/Users/h3/web/ergoemacs_org/emacs/gnu_emacs_keybinding_C-x.txt

;; some idea about command categories, in context to chosing keys for them

;; • whether a command has immediate effect, no prompt. e.g. shell vs delete-matching-lines
;; • whether a command has is safe to run by mistake. e.g. whitespace-mode vs eval-buffer
;; • whether a command is frequently needed (few times a hour). e.g.

;; idea about key groups
;; all should be sequence of single keys. 2 to 3 keys. All should start with F7. And all commands should be globally useful.
;; • 2 keys vs 3 keys
;; • whether the key ends in a digit key 0 to 9. These probably should be most frequently used, or immediate effect.

;; command that are not immediate (has prompt) probably should not have a key.

