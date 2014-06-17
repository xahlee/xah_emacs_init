;; -*- coding: utf-8 -*-
;; xah's emacs keybinding.

;; jump points to other files

;; • xah_emacs_abbr.el
;; • xah_emacs_alias.el
;; • xah_emacs_hyper_super_setup.el
;; • xah_emacs_keybinding.el
;; • xah_emacs_keybinding_control_key.el
;; • xah_emacs_keybinding_ergoemacs_raw.el
;; • xah_emacs_keybinding_ergoemacs_xah.el
;; • xah_emacs_keybinding_functions.el
;; • xah_emacs_keybinding_mode_specific.el
;; • xah_emacs_keybinding_shift_switch.el
;; • xah_emacs_keybinding_special_keys.el
;; • xah_emacs_keybinding_unset_keys.el
;; • xah_emacs_mouse_binding.el
;; • xah_emacs_unicode_input.el

;; • 〈Ergoemacs-vi Mode〉 http://ergoemacs.org/misc/ergoemacs_vi_mode.html
;; • 〈Dvorak Keyboard Layout〉 http://xahlee.info/comp/dvorak_keyboard_layout.html
;; • 〈ErgoEmacs Keybinding〉 http://ergoemacs.org/emacs/ergonomic_emacs_keybinding.html
;; • 〈xah_autohotkey.ahk〉 http://xahlee.info/mswin/autohotkey.html
;; • 〈Emacs: How to define Hyper ＆ Super Keys〉 http://ergoemacs.org/emacs/emacs_hyper_super_keys.html
;; • 〈Emacs: Remapping Keys Using key-translation-map〉 http://ergoemacs.org/emacs/emacs_key-translation-map.html
;; • 〈Emacs: Add Custom Keys to Enhance Productivity〉 http://ergoemacs.org/emacs/emacs_useful_user_keybinding.html
;; • 〈Keyboard Layouts Fight! Dvorak, Maltron, Colemak, NEO, Bépo, Turkish-F, …〉  http://xahlee.info/kbd/dvorak_and_all_keyboard_layouts.html

;; http://ergoemacs.org/emacs/command-frequency/Xah_Lee_2013-09-04.txt

;; Xah Lee
;; created: 2007-06.



;; stars indicate frequency of use
;; ★★★ every minute
;; ★★ every hour
;; ★ few times a day

;; 2014-06-06 this file are keybinding of key sequences, starting with the menu key
;; eventually, i shall have a complete system that covers entire emacs usage.

;; design sketch

;; the keys are now hardcoded for dvorak.

;; some design principles
;; • each key sequence should have 2 or 3 keys, no more. (counting the lead key)
;; • the keys:

;; .p gc
;; eu ht

;; should be the most frequently used. Each is 3-keys sequence. ⁖ 【menu e 3】, 【menu u h】,

;; basically, after the menu key, there are a total of 6 keys to start. These keys are on the home row or the row above, and are pressed by 2nd 3rd 4th fingers. (thumb is 1st finger) like this:

;; ,.p gcr
;; oeu htn

;; • all other letters should be 2-keys sequence. ⁖ 【menu z】, 【menu 8】

;; • the 【menu t ‹key›】 space is reserved for user to define their own commands
;; • the 【menu h ‹key›】 space is for emacs help. basically equivalent to 【C-h ‹key›】
;; • the 【menu space ‹key›】 space is for inserting brackets (){}[]""''“”‘’ and other brackets, and for inserting “=” “+” any unicode chars.
;; • 【menu enter】 is for execute-extended-command
;; • 【menu menu】  is undecided.

;; • the last key can be a number. ⁖ 【menu 8】, 【menu e 3】. For numbers, 3 4 and 7 8 are easiest. (pressed by 2nd ＆ 3rd fingers)

;; • can a key ends 【enter】 or 【space】 ? Yes, absolutely. These are top easy keys. There should be some scheme for them to make commands with such key sharing some theme/characteristics. ⁖ commands with keys ending in them should prompt… or they are all related to…
;; • can a key ends in 【menu】 key? I think so, but undecided.

;; • note: the binding should be based on command frequency and the key's ease.
;; Emacs vs vi: How to Compute a Keybinding's Efficiency? http://xahlee.info/kbd/efficiency_of_keybinding_emacs_vs_vim.html
;; Emacs's Command Frequency Statistics http://ergoemacs.org/emacs/command-frequency.html

;; • none of the key sequence should be mapped to a fast-repeat command.
;; Emacs: Fast-repeat vs Non-fast-repeat Commands ＆ Keys
;; http://xahlee.info/kbd/repeatable_vs_non-repeatable_keys_commands.html

(define-key key-translation-map (kbd "<apps>") (kbd "<menu>"))
(define-key key-translation-map (kbd "C-p") (kbd "<menu>")) ; Mac OS X don't do menu/app key.

(define-prefix-command 'xah-menu-keymap)
(global-set-key (kbd "<menu>") 'xah-menu-keymap)

;; (define-key key-translation-map (kbd "<menu> <end>") (kbd "C-g")) ; doesn't work in some cases in minibuffer. ⁖ when emacs asking yes or no when opening a none-existing file path

;; (global-set-key (kbd "<menu> <menu>") 'keyboard-quit)

(global-set-key (kbd "<menu> <return>") 'smex)

;; (global-set-key (kbd "<menu> SPC") (lambda () (interactive) (insert "_"))) ; low line (underscore)

(progn
  (define-prefix-command 'xah-menu-backspace-keymap)
  (global-set-key (kbd "<menu> <backspace>") xah-menu-backspace-keymap)
;; 'xah-delete-text-block
  )

(progn
  (define-prefix-command 'xah-menu-delete-keymap)
  (global-set-key (kbd "<menu> <delete>") xah-menu-delete-keymap)
  )

(progn
  (define-prefix-command 'xah-menu-tab-keymap)
  (global-set-key (kbd "<menu> <tab>") xah-menu-tab-keymap)
  (global-set-key (kbd "<menu> <tab> y") 'yas/expand)
  (global-set-key (kbd "<menu> <tab> <tab>") 'indent-for-tab-command)
  (global-set-key (kbd "<menu> <tab> i") 'complete-symbol)
  (global-set-key (kbd "<menu> <tab> r") 'indent-rigidly)
  )

(progn
  ;; this should reserved for user-defined keys
  (define-prefix-command 'xah-menu-space-keymap)
  (global-set-key (kbd "<menu> SPC") xah-menu-space-keymap)

  (global-set-key (kbd "<menu> SPC <menu>") 'xah-open-file-fast)
  (global-set-key (kbd "<menu> SPC SPC") nil)
  (global-set-key (kbd "<menu> SPC <return>") 'xah-run-current-file) ;  1494    0.09%  xah-run-current-file
  (global-set-key (kbd "<menu> SPC <backspace>") 'xah-delete-current-file)
  (global-set-key (kbd "<menu> SPC <tab>") nil)
  (global-set-key (kbd "<menu> SPC <delete>") nil)
  (global-set-key (kbd "<menu> SPC <home>") nil)
  (global-set-key (kbd "<menu> SPC <end>") nil)

  (global-set-key (kbd "<menu> SPC -") 'xah-insert-form-feed)
  (global-set-key (kbd "<menu> SPC .") 'title-case-string-region-or-line)

  (global-set-key (kbd "<menu> SPC 1") 'xah-copy-to-register-1)
  (global-set-key (kbd "<menu> SPC 2") 'xah-paste-from-register-1)
  (global-set-key (kbd "<menu> SPC 3") nil)
  (global-set-key (kbd "<menu> SPC 4") nil)
  (global-set-key (kbd "<menu> SPC 7") nil)
  (global-set-key (kbd "<menu> SPC 8") nil)

;; 'xah-open-in-desktop

  (global-set-key (kbd "<menu> SPC a") 'ace-jump-mode-pop-mark)
  (global-set-key (kbd "<menu> SPC b") 'xah-shell-commands)
  (global-set-key (kbd "<menu> SPC c") 'xah-cite)
  (global-set-key (kbd "<menu> SPC d") 'insert-date)
  (global-set-key (kbd "<menu> SPC e") nil)
  (global-set-key (kbd "<menu> SPC f") 'xah-find-text)
  (global-set-key (kbd "<menu> SPC g") 'ace-jump-mode)

  (global-set-key (kbd "<menu> SPC h") nil)
  (global-set-key (kbd "<menu> SPC i n") 'xah-insert-random-number)
  (global-set-key (kbd "<menu> SPC i s") 'xah-insert-random-string)
  (global-set-key (kbd "<menu> SPC i u") 'xah-insert-random-uuid)
  (global-set-key (kbd "<menu> SPC i x") 'xah-insert-random-hex)
  (global-set-key (kbd "<menu> SPC j") nil)
  (global-set-key (kbd "<menu> SPC k") nil)
  (global-set-key (kbd "<menu> SPC l") nil)
  (global-set-key (kbd "<menu> SPC m") 'magit-status)
  (global-set-key (kbd "<menu> SPC n") nil)
  (global-set-key (kbd "<menu> SPC o") 'xah-open-file-from-clipboard)
  (global-set-key (kbd "<menu> SPC p") 'xah-copy-file-path)
  (global-set-key (kbd "<menu> SPC q") nil)
  (global-set-key (kbd "<menu> SPC r 3") 'xah-escape-quotes)
  (global-set-key (kbd "<menu> SPC r '") 'xah-replace-straight-quotes)
  (global-set-key (kbd "<menu> SPC r ,") 'xah-remove-punctuation-trailing-redundant-space)
  (global-set-key (kbd "<menu> SPC r .") 'xah-convert-english-chinese-punctuation)
  (global-set-key (kbd "<menu> SPC r [") 'xah-remove-square-brackets)
  (global-set-key (kbd "<menu> SPC r g") 'xah-convert-latin-alphabet-gothic)
  (global-set-key (kbd "<menu> SPC r p") 'xah-convert-asian/ascii-space)
  (global-set-key (kbd "<menu> SPC r w") 'xah-convert-fullwidth-chars)
  (global-set-key (kbd "<menu> SPC s") nil)
  (global-set-key (kbd "<menu> SPC t") nil)
  (global-set-key (kbd "<menu> SPC u") 'xah-find-replace-text)
  (global-set-key (kbd "<menu> SPC v") 'xah-make-backup)
  (global-set-key (kbd "<menu> SPC w") nil)
  (global-set-key (kbd "<menu> SPC y") nil)
  (global-set-key (kbd "<menu> SPC z") 'xah-toggle-read-novel-mode)

 )

(global-set-key (kbd "<menu> .") 'universal-argument)
(global-set-key (kbd "<menu> '") nil)

(progn
  (define-prefix-command 'xah-menu-comma-keymap)
  (global-set-key (kbd "<menu> ,") xah-menu-comma-keymap)
  )

(global-set-key (kbd "<menu> -") nil)

(global-set-key (kbd "<menu> /") nil)
(global-set-key (kbd "<menu> ;") nil)
(global-set-key (kbd "<menu> =") nil)
(global-set-key (kbd "<menu> [") nil)
(global-set-key (kbd "<menu> \\") nil)
(global-set-key (kbd "<menu> `") nil)

(global-set-key (kbd "<menu> 0") nil)
(global-set-key (kbd "<menu> 1") nil)
(global-set-key (kbd "<menu> 2") 'delete-window)
(global-set-key (kbd "<menu> 3") 'delete-other-windows)
(global-set-key (kbd "<menu> 4") 'split-window-vertically)
(global-set-key (kbd "<menu> 5") nil)
(global-set-key (kbd "<menu> 6") nil)
(global-set-key (kbd "<menu> 7") 'ffap)
(global-set-key (kbd "<menu> 8") 'dired-jump)
(global-set-key (kbd "<menu> 9") 'ispell-word)

(progn
  (define-prefix-command 'xah-menu-a-keymap)
  (global-set-key (kbd "<menu> a") xah-menu-a-keymap)
  )

  (global-set-key (kbd "<menu> b") 'end-of-buffer)

(progn
  (define-prefix-command 'xah-menu-c-keymap)
  (global-set-key (kbd "<menu> c") xah-menu-c-keymap)
  (global-set-key (kbd "<menu> c SPC") nil)
  (global-set-key (kbd "<menu> c <return>") nil)

  (global-set-key (kbd "<menu> c c") 'bookmark-bmenu-list)
  (global-set-key (kbd "<menu> c u") nil)
  (global-set-key (kbd "<menu> c e") nil)
  (global-set-key (kbd "<menu> c g") 'ido-switch-buffer)
  (global-set-key (kbd "<menu> c h") 'recentf-open-files)
  (global-set-key (kbd "<menu> c t") 'ibuffer)
  (global-set-key (kbd "<menu> c p") 'query-replace-regexp)
  )

  (global-set-key (kbd "<menu> d") 'beginning-of-buffer)

(progn
  ;; this is mode-specific
  (define-prefix-command 'xah-menu-e-keymap)
  (global-set-key (kbd "<menu> e") xah-menu-e-keymap)

  )

(progn
  (define-prefix-command 'xah-menu-f-keymap)
  (global-set-key (kbd "<menu> f") xah-menu-f-keymap)
  )

(global-set-key (kbd "<menu> g") 'isearch-forward)

(progn

  (define-prefix-command 'xah-menu-h-keymap)
  (global-set-key (kbd "<menu> h") xah-menu-h-keymap)
  (global-set-key (kbd "<menu> h 1") nil)
  (global-set-key (kbd "<menu> h 2") nil)
  (global-set-key (kbd "<menu> h 3") 'man)
  (global-set-key (kbd "<menu> h 4") 'elisp-index-search)
  (global-set-key (kbd "<menu> h 5") 'apropos-variable)
  (global-set-key (kbd "<menu> h 6") 'apropos-value)
  (global-set-key (kbd "<menu> h 7") 'lookup-google)
  (global-set-key (kbd "<menu> h 8") 'lookup-wikipedia)
  (global-set-key (kbd "<menu> h 9") 'lookup-word-definition)
  (global-set-key (kbd "<menu> h 0") 'lookup-all-dictionaries)

  (global-set-key (kbd "<menu> h a") 'apropos-command)
  (global-set-key (kbd "<menu> h b") 'describe-bindings)
  (global-set-key (kbd "<menu> h c") 'describe-char)
  (global-set-key (kbd "<menu> h C") 'describe-coding-system)
  (global-set-key (kbd "<menu> h d") 'apropos-documentation)
  (global-set-key (kbd "<menu> h e") 'view-echo-area-messages)
  (global-set-key (kbd "<menu> h f") 'describe-function)
  (global-set-key (kbd "<menu> h F") 'Info-goto-emacs-command-node)
  (global-set-key (kbd "<menu> h g") nil)
  (global-set-key (kbd "<menu> h h") nil)
  (global-set-key (kbd "<menu> h i") 'info)
  (global-set-key (kbd "<menu> h I") 'describe-input-method)
  (global-set-key (kbd "<menu> h j") nil)
  (global-set-key (kbd "<menu> h k") 'describe-key)
  (global-set-key (kbd "<menu> h K") 'Info-goto-emacs-key-command-node)
  (global-set-key (kbd "<menu> h l") 'view-lossage)
  (global-set-key (kbd "<menu> h L") 'describe-language-environment)
  (global-set-key (kbd "<menu> h m") 'xah-describe-major-mode)
  (global-set-key (kbd "<menu> h n") nil)
  (global-set-key (kbd "<menu> h o") nil)
  (global-set-key (kbd "<menu> h p") 'finder-by-keyword)
  (global-set-key (kbd "<menu> h q") nil)
  (global-set-key (kbd "<menu> h r") nil)
  (global-set-key (kbd "<menu> h s") 'describe-syntax)
  (global-set-key (kbd "<menu> h S") 'info-lookup-symbol)
  (global-set-key (kbd "<menu> h t") nil)
  (global-set-key (kbd "<menu> h u") nil)
  (global-set-key (kbd "<menu> h v") 'describe-variable)
  (global-set-key (kbd "<menu> h w") nil)
  (global-set-key (kbd "<menu> h x") nil)
  (global-set-key (kbd "<menu> h y") nil)
  (global-set-key (kbd "<menu> h z") nil)
  )

(progn
  (define-prefix-command 'xah-menu-i-keymap)
  (global-set-key (kbd "<menu> i") xah-menu-i-keymap)
  )

(global-set-key (kbd "<menu> j") 'xah-copy-all)

(global-set-key (kbd "<menu> k") 'xah-clean-whitespace)

(global-set-key (kbd "<menu> l") 'recenter-top-bottom)

(global-set-key (kbd "<menu> m") search-map)

(progn
  ;; commands here shouldn't change the buffer immediately.
  ;; they turn on minor/major mode, or prompt
  (define-prefix-command 'xah-menu-n-keymap)
  (global-set-key (kbd "<menu> n") xah-menu-n-keymap)

  (global-set-key (kbd "<menu> n SPC") nil)
  (global-set-key (kbd "<menu> n <return>") nil)

  (global-set-key (kbd "<menu> n SPC e") 'xah-elisp-mode)
  (global-set-key (kbd "<menu> n SPC h") 'xah-html-mode)
  (global-set-key (kbd "<menu> n SPC j") 'xah-js-mode)
  (global-set-key (kbd "<menu> n SPC c") 'xah-css-mode)

  (global-set-key (kbd "<menu> n 3") 'whitespace-mode)
  (global-set-key (kbd "<menu> n 4") 'linum-mode)
  (global-set-key (kbd "<menu> n 5") 'visual-line-mode)
  (global-set-key (kbd "<menu> n 6") 'calendar)
  (global-set-key (kbd "<menu> n 7") 'calc)
  (global-set-key (kbd "<menu> n 8") 'shell)
  (global-set-key (kbd "<menu> n 9") 'shell-command)
  (global-set-key (kbd "<menu> n 0") 'shell-command-on-region)

  (global-set-key (kbd "<menu> n b") 'toggle-debug-on-error)
  (global-set-key (kbd "<menu> n c") 'toggle-case-fold-search)
  (global-set-key (kbd "<menu> n e") 'eshell)
  (global-set-key (kbd "<menu> n h") 'widen)
  (global-set-key (kbd "<menu> n n") 'narrow-to-region)
  (global-set-key (kbd "<menu> n s") 'flyspell-buffer)
  (global-set-key (kbd "<menu> n t") 'narrow-to-defun)
  (global-set-key (kbd "<menu> n w") nil)

  )

(progn
  (define-prefix-command 'xah-menu-o-keymap)
  (global-set-key (kbd "<menu> o") xah-menu-o-keymap)
  (global-set-key (kbd "<menu> o SPC") nil)
  )

(global-set-key (kbd "<menu> p") 'query-replace) ; 2746    0.17%  query-replace

(global-set-key (kbd "<menu> q") 'xah-cut-all)

(progn
  ;; kinda replacement related
  (define-prefix-command 'xah-menu-r-keymap)
  (global-set-key (kbd "<menu> r") xah-menu-r-keymap)

  (global-set-key (kbd "<menu> r 1") 'kmacro-start-macro)
  (global-set-key (kbd "<menu> r 2") 'kmacro-end-macro)
  (global-set-key (kbd "<menu> r 3") 'apply-macro-to-region-lines)
  (global-set-key (kbd "<menu> r 4") 'sort-lines)
  (global-set-key (kbd "<menu> r 5") 'sort-numeric-fields)
  (global-set-key (kbd "<menu> r 6") 'reverse-region)
  (global-set-key (kbd "<menu> r 7") 'list-matching-lines)
  (global-set-key (kbd "<menu> r 8") 'delete-matching-lines)
  (global-set-key (kbd "<menu> r 9") 'delete-non-matching-lines)
  (global-set-key (kbd "<menu> r 0") 'delete-duplicate-lines)

  (global-set-key (kbd "<menu> r SPC") 'rectangle-mark-mode)
  (global-set-key (kbd "<menu> r c") 'replace-rectangle)
  (global-set-key (kbd "<menu> r d") 'delete-rectangle)
  (global-set-key (kbd "<menu> r g") 'kill-rectangle)
  (global-set-key (kbd "<menu> r l") 'clear-rectangle)
  (global-set-key (kbd "<menu> r n") 'rectangle-number-lines)
  (global-set-key (kbd "<menu> r o") 'open-rectangle)
  (global-set-key (kbd "<menu> r r") 'yank-rectangle)
  (global-set-key (kbd "<menu> r y") 'delete-whitespace-rectangle)

  )

(global-set-key (kbd "<menu> s") 'save-buffer) ; 25468    1.58%  save-buffer

(progn
  (define-prefix-command 'xah-menu-t-keymap)
  (global-set-key (kbd "<menu> t") xah-menu-t-keymap)

  (global-set-key (kbd "<menu> t SPC") 'exchange-point-and-mark)
  (global-set-key (kbd "<menu> t <return>") 'pop-global-mark)

  (global-set-key (kbd "<menu> t 3") 'copy-to-register)
  (global-set-key (kbd "<menu> t 4") 'insert-register)
  (global-set-key (kbd "<menu> t 5") 'number-to-register)
  (global-set-key (kbd "<menu> t 6") 'increment-register)
  (global-set-key (kbd "<menu> t 7") 'point-to-register)
  (global-set-key (kbd "<menu> t 8") 'jump-to-register)

  (global-set-key (kbd "<menu> t q") 'xah-cut-line-or-region)
  (global-set-key (kbd "<menu> t j") 'xah-copy-line-or-region)
  (global-set-key (kbd "<menu> t k") 'yank)

  (global-set-key (kbd "<menu> t t") 'repeat-complex-command)
  (global-set-key (kbd "<menu> t h") 'repeat)

  (global-set-key (kbd "<menu> t f") 'frame-configuration-to-register)
  (global-set-key (kbd "<menu> t r") 'copy-rectangle-to-register)
  (global-set-key (kbd "<menu> t w") 'window-configuration-to-register)
  (global-set-key (kbd "<menu> t v") 'vc-register)

  )

(progn
;; u is for char insert

  (define-prefix-command 'xah-menu-u-keymap)
  (global-set-key (kbd "<menu> u") 'xah-menu-u-keymap)

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

(global-set-key (kbd "<menu> v") nil)

(progn
  (define-prefix-command 'xah-menu-w-keymap)
  (global-set-key (kbd "<menu> w") xah-menu-w-keymap)

  (global-set-key (kbd "<menu> w b") 'eval-buffer)
  (global-set-key (kbd "<menu> w r") 'eval-region)
  (global-set-key (kbd "<menu> w d") 'eval-defun)
  (global-set-key (kbd "<menu> w e") 'eval-expression)
  (global-set-key (kbd "<menu> w l") 'eval-last-sexp)

  )

(global-set-key (kbd "<menu> x") nil)

(global-set-key (kbd "<menu> y") nil)

(global-set-key (kbd "<menu> z") 'comment-dwim)


;;;; misc

;; (defun toggle-menu-key ()
;;   "toggle the value of `w32-apps-modifier' between 'meta and 'nil"
;;   (interactive)
;;   (if (eq w32-apps-modifier 'meta)
;;         (progn (setq w32-apps-modifier 'nil))
;;       (progn (setq w32-apps-modifier 'meta) )
;;       ))

;; ~/web/ergoemacs_org/emacs/gnu_emacs_keybinding_C-x.txt

;; some idea about command categories, in context to choosing keys for them

;; • whether a command has immediate effect, no prompt. ⁖ shell vs delete-matching-lines
;; • whether a command has is safe to run by mistake. ⁖ whitespace-mode vs eval-buffer
;; • whether a command is frequently needed ⁖ few times a min, hour, day

;; idea about key groups
;; all should be sequence of single keys. 2 to 3 keys. All should start with F7. And all commands should be globally useful.
;; • 2 keys vs 3 keys
;; • whether the key ends in a digit key 0 to 9. These probably should be most frequently used, or immediate effect.

;; (ergoemacs-ignore-prev-global) ; Do not honor previously defined global keys. 2013-06-24

;'indent-for-tab-command



;; ;; 2013-11-04 make emacs auto show suggestions when a prefix key is pressed
;; (require 'guide-key)
;; (setq guide-key/guide-key-sequence '("<menu> t" "<tab> t" ))
;; (guide-key-mode 0)

; 5605    0.35%  cua-set-mark; 944    0.06%  set-mark-command
;   6077    0.38%  ergoemacs-M-o
;;  toggle-input-method

;; ;; todo add:
  ;; (global-set-key (kbd "<menu> r 4") 'set-mark-command)

;; C-x C-p	mark-page

;; C-x C-l	downcase-region
;; C-x C-u	upcase-region

;; C-x C-t	transpose-lines
;; C-x C-o	delete-blank-lines
;; C-x C-q	toggle-read-only

;; C-x C-f	find-file
;; C-x C-r	find-file-read-only
;; C-x C-v	find-alternate-file
;; C-x C-w	write-file

;; C-x d	dired

;; C-x e	kmacro-end-and-call-macro
;; C-x q	kbd-macro-query
;; C-x C-k	kmacro-keymap


;; C-x C-c	save-buffers-kill-terminal
;; C-x C-d	list-directory
;; C-x C-n	set-goal-column
;; C-x C-z	suspend-frame
;; C-x ESC	Prefix Command
;; C-x $	set-selective-display
;; C-x '	expand-abbrev
;; C-x *	calc-dispatch
;; C-x -	shrink-window-if-larger-than-buffer
;; C-x .	set-fill-prefix

;; C-x +	balance-windows
;; C-x 3	split-window-horizontally

;; C-x 4	ctl-x-4-prefix
;; C-x 5	ctl-x-5-prefix
;; C-x 6	2C-command
;; C-x 8	Prefix Command
;; C-x ;	comment-set-column
;; C-x <	scroll-left
;; C-x =	what-cursor-position
;; C-x >	scroll-right
;; C-x [	backward-page
;; C-x ]	forward-page
;; C-x ^	enlarge-window
;; C-x `	next-error
;; C-x f	set-fill-column
;; C-x i	insert-file
;; C-x k	kill-buffer
;; C-x l	count-lines-page
;; C-x m	compose-mail
;; C-x n	Prefix Command
;; C-x o	other-window
;; C-x r	Prefix Command
;; C-x s	save-some-buffers

;; C-x v	vc-prefix-map
;; C-x {	shrink-window-horizontally
;; C-x }	enlarge-window-horizontally
;; C-x DEL	backward-kill-sentence
;; C-x C-+	text-scale-adjust
;; C-x C--	text-scale-adjust
;; C-x C-0	text-scale-adjust
;; C-x C-=	text-scale-adjust
;; C-x <C-left>	previous-buffer
;; C-x <C-right>	next-buffer
;; C-x <left>	previous-buffer
;; C-x <right>	next-buffer

;; C-x C-k C-a	kmacro-add-counter
;; C-x C-k C-c	kmacro-set-counter
;; C-x C-k C-d	kmacro-delete-ring-head
;; C-x C-k C-e	kmacro-edit-macro-repeat
;; C-x C-k C-f	kmacro-set-format
;; C-x C-k TAB	kmacro-insert-counter
;; C-x C-k C-k	kmacro-end-or-call-macro-repeat
;; C-x C-k C-l	kmacro-call-ring-2nd-repeat
;; C-x C-k RET	kmacro-edit-macro
;; C-x C-k C-n	kmacro-cycle-ring-next
;; C-x C-k C-p	kmacro-cycle-ring-previous
;; C-x C-k C-s	kmacro-start-macro
;; C-x C-k C-t	kmacro-swap-ring
;; C-x C-k C-v	kmacro-view-macro-repeat
;; C-x C-k SPC	kmacro-step-edit-macro
;; C-x C-k b	kmacro-bind-to-key
;; C-x C-k e	edit-kbd-macro
;; C-x C-k l	kmacro-edit-lossage
;; C-x C-k n	kmacro-name-last-macro
;; C-x C-k q	kbd-macro-query
;; C-x C-k r	apply-macro-to-region-lines
;; C-x C-k s	kmacro-start-macro

;; C-x RET C-\	set-input-method
;; C-x RET F	set-file-name-coding-system
;; C-x RET X	set-next-selection-coding-system
;; C-x RET c	universal-coding-system-argument
;; C-x RET f	set-buffer-file-coding-system
;; C-x RET k	set-keyboard-coding-system
;; C-x RET l	set-language-environment
;; C-x RET p	set-buffer-process-coding-system
;; C-x RET r	revert-buffer-with-coding-system
;; C-x RET t	set-terminal-coding-system
;; C-x RET x	set-selection-coding-system

;; C-x 4 C-f	find-file-other-window
;; C-x 4 C-o	display-buffer
;; C-x 4 .	find-tag-other-window
;; C-x 4 0	kill-buffer-and-window
;; C-x 4 a	add-change-log-entry-other-window
;; C-x 4 b	switch-to-buffer-other-window
;; C-x 4 c	clone-indirect-buffer-other-window
;; C-x 4 d	dired-other-window
;; C-x 4 f	find-file-other-window
;; C-x 4 m	compose-mail-other-window
;; C-x 4 r	find-file-read-only-other-window

;; C-x 5 C-f	find-file-other-frame
;; C-x 5 C-o	display-buffer-other-frame
;; C-x 5 .	find-tag-other-frame
;; C-x 5 0	delete-frame
;; C-x 5 1	delete-other-frames
;; C-x 5 2	make-frame-command
;; C-x 5 b	switch-to-buffer-other-frame
;; C-x 5 d	dired-other-frame
;; C-x 5 f	find-file-other-frame
;; C-x 5 m	compose-mail-other-frame
;; C-x 5 o	other-frame
;; C-x 5 r	find-file-read-only-other-frame

;; C-x 6 2	2C-two-columns
;; C-x 6 b	2C-associate-buffer
;; C-x 6 s	2C-split
;; C-x 6 <f2>	2C-two-columns
;; C-x 8 RET	ucs-insert
;; C-x a C-a	add-mode-abbrev
;; C-x a '	expand-abbrev
;; C-x a +	add-mode-abbrev
;; C-x a -	inverse-add-global-abbrev
;; C-x a e	expand-abbrev
;; C-x a g	add-global-abbrev
;; C-x a i	Prefix Command
;; C-x a l	add-mode-abbrev
;; C-x a n	expand-jump-to-next-slot
;; C-x a p	expand-jump-to-previous-slot

;; C-x n p	narrow-to-page

;; C-x r b	bookmark-jump
;; C-x r l	bookmark-bmenu-list
;; C-x r m	bookmark-set

;; C-x v +	vc-update
;; C-x v =	vc-diff
;; C-x v D	vc-root-diff
;; C-x v L	vc-print-root-log
;; C-x v a	vc-update-change-log
;; C-x v b	vc-switch-backend
;; C-x v c	vc-rollback
;; C-x v d	vc-dir
;; C-x v g	vc-annotate
;; C-x v h	vc-insert-headers
;; C-x v l	vc-print-log
;; C-x v m	vc-merge
;; C-x v r	vc-retrieve-tag
;; C-x v s	vc-create-tag
;; C-x v u	vc-revert
;; C-x v v	vc-next-action
;; C-x v ~	vc-revision-other-window

;; C-x a i g	inverse-add-global-abbrev
;; C-x a i l	inverse-add-mode-abbrev

;; ;; todo
;; select all, copy all, open, those standard keys

;; • add all emacs commands to my key sequence system

;; 'quoted-insert

;; 'toggle-input-method
