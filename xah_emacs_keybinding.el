;; -*- coding: utf-8 -*-
;; xah's emacs keybinding.

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



;; 2014-06-06 this file are keybinding of key sequences, starting with the menu key
;; the goal is to have a key system to completely replace emacs 1k+ keys.

;; design sketch

;; the keys are now hardcoded for dvorak.

;; some design principles
;; • each key sequence should have 2 or 3 keys. (counting the lead key) some command may have 4 keys, but any command used daily have max of 3 keys.

;; • the keys:

;; .p gc
;; eu ht

;; should be the most frequently used. Each is 3-keys sequence. ⁖ 【menu e 3】, 【menu u h】,

;; of these 8 keys, the {. p g} are special. Each used for 2 key sequences, for 3 of the most frequently used commands. 【menu .】 is for universal-argument. 【menu p】 is for query-replace. 【menu g】 is isearch.

;; basically, after the menu key, there are a total of 12 keys to start, 6 for each hand. These keys are on the home row or the row above, and are pressed by 2nd 3rd 4th fingers. (thumb is 1st finger) like this:

;; ,.p gcr
;; oeu htn

;; • the 【menu space ‹key›】 space is reserved for user to define their own commands
;; • the 【menu h ‹key›】 space is for emacs help. basically equivalent to 【C-h ‹key›】
;; • the 【menu u ‹key›】 space is for inserting brackets (){}[]""''“”‘’ and other brackets, and for inserting “=” “+” any unicode chars.
;; • the 【menu p】 is for query-replace
;; • the 【menu g】 is for isearch
;; • the 【menu .】 is for universal-argument.
;; • 【menu enter】 is for execute-extended-command
;; • 【menu menu】  is undecided.

;; • the last key can be a number. ⁖ 【menu 8】, 【menu e 3】. For numbers, 3 4 and 7 8 are easiest. (pressed by 2nd ＆ 3rd fingers)

;; • can a key ends with 【enter】 or 【space】 ? Yes, absolutely. These are top easy keys. There should be some scheme for them to make commands with such key sharing some theme/characteristics. ⁖ commands with keys ending in them should prompt… or they are all related to…
;; • can a key ends in 【menu】 key? I think so, but undecided.

;; • note: the binding should be based on command frequency and the key's ease.
;; Emacs vs vi: How to Compute a Keybinding's Efficiency? http://xahlee.info/kbd/efficiency_of_keybinding_emacs_vs_vim.html
;; Emacs's Command Frequency Statistics http://ergoemacs.org/emacs/command-frequency.html

;; • none of the key sequence should be mapped to a fast-repeat command.
;; Emacs: Fast-repeat vs Non-fast-repeat Commands ＆ Keys
;; http://xahlee.info/kbd/repeatable_vs_non-repeatable_keys_commands.html

;; the above is the sketch of the design. However, i realized that some exceptions is ok, or even optimal. One thing i learned is that a strict regularity or rule may not be optimal, as some exception or irregularity sometimes makes it more convenient, easier to remember, or make your fingers good because they don't always use the same keys. These reasoning may be fallacy, I don't have a solid analysis on it yet.

(when (string-equal system-type "windows-nt")
  (define-key key-translation-map (kbd "<apps>") (kbd "<menu>")))

(define-key key-translation-map (kbd "C-p") (kbd "<menu>")) ; Mac OS X don't do menu/app key.

(define-prefix-command 'xah-menu-keymap)
(global-set-key (kbd "<menu>") 'xah-menu-keymap)

(global-set-key (kbd "<menu> <return>") 'smex) ; todo check if  bound, else execute-extended-command

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
  (define-prefix-command 'xah-delete-keymap)
  (global-set-key (kbd "<delete>") xah-delete-keymap)
  (define-key 'xah-delete-keymap (kbd "t") 'xah-open-file-fast)
  )

(progn
  (define-prefix-command 'xah-menu-tab-keymap)
  (global-set-key (kbd "<menu> <tab>") xah-menu-tab-keymap)
  (global-set-key (kbd "<menu> <tab> y") 'yas-expand)
  (global-set-key (kbd "<menu> <tab> <tab>") 'indent-for-tab-command)
  (global-set-key (kbd "<menu> <tab> i") 'complete-symbol)
  (global-set-key (kbd "<menu> <tab> g") 'indent-rigidly)
  (global-set-key (kbd "<menu> <tab> r") 'indent-region)
  (global-set-key (kbd "<menu> <tab> e") 'expand-abbrev)
  )

(global-set-key (kbd "<menu> .") 'universal-argument)
(global-set-key (kbd "<menu> '") 'quoted-insert)

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

(global-set-key (kbd "<menu> 1") nil)
(global-set-key (kbd "<menu> 2") 'delete-window)
(global-set-key (kbd "<menu> 3") 'delete-other-windows)
(global-set-key (kbd "<menu> 4") 'split-window-vertically)
(global-set-key (kbd "<menu> 5") nil)
(global-set-key (kbd "<menu> 6") 'xah-select-current-block)
(global-set-key (kbd "<menu> 7") 'xah-select-current-line)
(global-set-key (kbd "<menu> 8") 'xah-select-text-in-bracket-or-quote)
(global-set-key (kbd "<menu> 9") 'ispell-word)
(global-set-key (kbd "<menu> 0") nil)

(progn
  (define-prefix-command 'xah-menu-a-keymap)
  (global-set-key (kbd "<menu> a") xah-menu-a-keymap)

  (global-set-key (kbd "<menu> a SPC") 'expand-abbrev)

  (global-set-key (kbd "<menu> a m") 'abbrev-mode)

  (global-set-key (kbd "<menu> a '") 'abbrev-prefix-mark)
  (global-set-key (kbd "<menu> a e") 'edit-abbrevs)
  (global-set-key (kbd "<menu> a r") 'expand-region-abbrevs)
  (global-set-key (kbd "<menu> a u") 'unexpand-abbrev)
  (global-set-key (kbd "<menu> a g") 'add-global-abbrev)
  (global-set-key (kbd "<menu> a a") 'add-mode-abbrev)
  (global-set-key (kbd "<menu> a v") 'inverse-add-global-abbrev)
  (global-set-key (kbd "<menu> a l") 'inverse-add-mode-abbrev)
  (global-set-key (kbd "<menu> a n") 'expand-jump-to-next-slot)
  (global-set-key (kbd "<menu> a p") 'expand-jump-to-previous-slot)
  )

(global-set-key (kbd "<menu> b") 'end-of-buffer)

(progn
  (define-prefix-command 'xah-menu-c-keymap)
  (global-set-key (kbd "<menu> c") xah-menu-c-keymap)
  (global-set-key (kbd "<menu> c SPC") nil)
  (global-set-key (kbd "<menu> c <return>") nil)

  (global-set-key (kbd "<menu> c o") 'xah-open-in-desktop)
  (global-set-key (kbd "<menu> c c") 'bookmark-bmenu-list)
  (global-set-key (kbd "<menu> c u") 'find-file-at-point)
  (global-set-key (kbd "<menu> c e") 'dired-jump)
  (global-set-key (kbd "<menu> c g") 'ido-switch-buffer)
  (global-set-key (kbd "<menu> c h") 'recentf-open-files)
  (global-set-key (kbd "<menu> c n") 'xah-new-empty-buffer)
  (global-set-key (kbd "<menu> c s") 'xah-open-in-external-app)
  (global-set-key (kbd "<menu> c t") 'ibuffer)
  (global-set-key (kbd "<menu> c p") 'find-file)

  )

  (global-set-key (kbd "<menu> d") 'beginning-of-buffer)

(progn
  ;; this is mode-specific
  (define-prefix-command 'xah-menu-e-keymap)
  (global-set-key (kbd "<menu> e") xah-menu-e-keymap)

  )

(global-set-key (kbd "<menu> f") nil)

(global-set-key (kbd "<menu> g") 'isearch-forward)

(progn
  (define-prefix-command 'xah-help-keymap)

  (global-set-key (kbd "<menu> h") xah-help-keymap)

  (define-key xah-help-keymap (kbd "1") nil)
  (define-key xah-help-keymap (kbd "2") nil)
  (define-key xah-help-keymap (kbd "3") 'man)

  (define-key xah-help-keymap (kbd "4") 'elisp-index-search)
  (define-key xah-help-keymap (kbd "5") 'apropos-variable)
  (define-key xah-help-keymap (kbd "6") 'apropos-value)
  (define-key xah-help-keymap (kbd "7") 'lookup-google)
  (define-key xah-help-keymap (kbd "8") 'lookup-wikipedia)
  (define-key xah-help-keymap (kbd "9") 'lookup-word-definition)
  (define-key xah-help-keymap (kbd "0") 'lookup-all-dictionaries)

  (define-key xah-help-keymap (kbd "a") 'apropos-command)
  (define-key xah-help-keymap (kbd "b") 'describe-bindings)
  (define-key xah-help-keymap (kbd "c") 'describe-char)
  (define-key xah-help-keymap (kbd "C") 'describe-coding-system)
  (define-key xah-help-keymap (kbd "d") 'apropos-documentation)
  (define-key xah-help-keymap (kbd "e") 'view-echo-area-messages)
  (define-key xah-help-keymap (kbd "f") 'describe-function)
  (define-key xah-help-keymap (kbd "F") 'Info-goto-emacs-command-node)
  (define-key xah-help-keymap (kbd "g") nil)
  (define-key xah-help-keymap (kbd "h") 'describe-face)
  (define-key xah-help-keymap (kbd "i") 'info)
  (define-key xah-help-keymap (kbd "I") 'describe-input-method)
  (define-key xah-help-keymap (kbd "j") nil)
  (define-key xah-help-keymap (kbd "k") 'describe-key)
  (define-key xah-help-keymap (kbd "K") 'Info-goto-emacs-key-command-node)
  (define-key xah-help-keymap (kbd "l") 'view-lossage)
  (define-key xah-help-keymap (kbd "L") 'describe-language-environment)
  (define-key xah-help-keymap (kbd "m") 'xah-describe-major-mode)
  (define-key xah-help-keymap (kbd "n") nil)
  (define-key xah-help-keymap (kbd "o") nil)
  (define-key xah-help-keymap (kbd "p") 'finder-by-keyword)
  (define-key xah-help-keymap (kbd "q") nil)
  (define-key xah-help-keymap (kbd "r") nil)
  (define-key xah-help-keymap (kbd "s") 'describe-syntax)
  (define-key xah-help-keymap (kbd "S") 'info-lookup-symbol)
  (define-key xah-help-keymap (kbd "t") nil)
  (define-key xah-help-keymap (kbd "u") nil)
  (define-key xah-help-keymap (kbd "v") 'describe-variable)
  (define-key xah-help-keymap (kbd "w") nil)
  (define-key xah-help-keymap (kbd "x") nil)
  (define-key xah-help-keymap (kbd "y") nil)
  (define-key xah-help-keymap (kbd "z") nil)
  )

(progn
  (define-prefix-command 'xah-menu-i-keymap) ; commands in goto-map
  (global-set-key (kbd "<menu> i") xah-menu-i-keymap)

  (define-key xah-menu-i-keymap (kbd "<tab>") 'move-to-column)
  (define-key xah-menu-i-keymap (kbd "c") 'goto-char)
  (define-key xah-menu-i-keymap (kbd "g") 'goto-line)
  (define-key xah-menu-i-keymap (kbd "n") 'next-error)
  (define-key xah-menu-i-keymap (kbd "p") 'previous-error)
  )

(global-set-key (kbd "<menu> j") 'xah-copy-all)

(global-set-key (kbd "<menu> k") 'xah-clean-whitespace)

(global-set-key (kbd "<menu> l") 'recenter-top-bottom)

(progn
  (define-prefix-command 'xah-menu-m-keymap) ; commands in search-map
  (global-set-key (kbd "<menu> m") xah-menu-m-keymap)

  (define-key xah-menu-m-keymap (kbd ".") 'isearch-forward-symbol-at-point)
  (define-key xah-menu-m-keymap (kbd "s") 'isearch-forward-symbol)
  (define-key xah-menu-m-keymap (kbd "w") 'isearch-forward-word)
  (define-key xah-menu-m-keymap (kbd "h .") 'highlight-symbol-at-point)
  (define-key xah-menu-m-keymap (kbd "h f") 'hi-lock-find-patterns)
  (define-key xah-menu-m-keymap (kbd "h l") 'highlight-lines-matching-regexp)
  (define-key xah-menu-m-keymap (kbd "h p") 'highlight-phrase)
  (define-key xah-menu-m-keymap (kbd "h r") 'highlight-regexp)
  (define-key xah-menu-m-keymap (kbd "h u") 'unhighlight-regexp)
  (define-key xah-menu-m-keymap (kbd "h w") 'hi-lock-write-interactive-patterns)
)

(progn
  ;; commands here shouldn't change the buffer immediately.
  ;; they turn on minor/major mode, change display, or prompt, etc.
  (define-prefix-command 'xah-harmless-keymap)
  (global-set-key (kbd "<menu> n") xah-harmless-keymap)

  (define-key xah-harmless-keymap (kbd "SPC") nil)
  (define-key xah-harmless-keymap (kbd "<return>") nil)

  (define-key xah-harmless-keymap (kbd "3") 'whitespace-mode)
  (define-key xah-harmless-keymap (kbd "4") 'linum-mode)
  (define-key xah-harmless-keymap (kbd "5") 'visual-line-mode)
  (define-key xah-harmless-keymap (kbd "6") 'calendar)
  (define-key xah-harmless-keymap (kbd "7") 'calc)
  (define-key xah-harmless-keymap (kbd "8") 'shell)
  (define-key xah-harmless-keymap (kbd "9") 'shell-command)
  (define-key xah-harmless-keymap (kbd "0") 'shell-command-on-region)

  (define-key xah-harmless-keymap (kbd "b") 'toggle-debug-on-error)
  (define-key xah-harmless-keymap (kbd "c") 'toggle-case-fold-search)
  (define-key xah-harmless-keymap (kbd "e") 'eshell)
  (define-key xah-harmless-keymap (kbd "h") 'widen)
  (define-key xah-harmless-keymap (kbd "n") 'narrow-to-region)
  (define-key xah-harmless-keymap (kbd "s") 'flyspell-buffer)
  (define-key xah-harmless-keymap (kbd "t") 'narrow-to-defun)

  )

(progn
  (define-prefix-command 'xah-menu-o-keymap)
  (global-set-key (kbd "<menu> o") xah-menu-o-keymap)
  )

(global-set-key (kbd "<menu> p") 'query-replace) ; 2746    0.17%  query-replace

(global-set-key (kbd "<menu> q") 'xah-cut-all)

(progn
  ;; kinda replacement related
  (define-prefix-command 'xah-edit-cmds-keymap)
  (global-set-key (kbd "<menu> r") xah-edit-cmds-keymap)

  (define-key xah-edit-cmds-keymap (kbd "1") 'kmacro-start-macro)
  (define-key xah-edit-cmds-keymap (kbd "2") 'kmacro-end-macro)
  (define-key xah-edit-cmds-keymap (kbd "3") 'apply-macro-to-region-lines)
  (define-key xah-edit-cmds-keymap (kbd "4") 'sort-lines)
  (define-key xah-edit-cmds-keymap (kbd "5") 'sort-numeric-fields)
  (define-key xah-edit-cmds-keymap (kbd "6") 'reverse-region)
  (define-key xah-edit-cmds-keymap (kbd "7") 'list-matching-lines)
  (define-key xah-edit-cmds-keymap (kbd "8") 'delete-matching-lines)
  (define-key xah-edit-cmds-keymap (kbd "9") 'delete-non-matching-lines)
  (define-key xah-edit-cmds-keymap (kbd "0") 'delete-duplicate-lines)

  (define-key xah-edit-cmds-keymap (kbd "SPC") 'rectangle-mark-mode)
  (define-key xah-edit-cmds-keymap (kbd "c") 'replace-rectangle)
  (define-key xah-edit-cmds-keymap (kbd "d") 'delete-rectangle)
  (define-key xah-edit-cmds-keymap (kbd "g") 'kill-rectangle)
  (define-key xah-edit-cmds-keymap (kbd "l") 'clear-rectangle)
  (define-key xah-edit-cmds-keymap (kbd "n") 'rectangle-number-lines)
  (define-key xah-edit-cmds-keymap (kbd "o") 'open-rectangle)
  (define-key xah-edit-cmds-keymap (kbd "r") 'yank-rectangle)
  (define-key xah-edit-cmds-keymap (kbd "y") 'delete-whitespace-rectangle)

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

  (global-set-key (kbd "<menu> t p") 'query-replace-regexp)
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

  (define-prefix-command 'xah-insert-keymap)
  (global-set-key (kbd "<menu> u") 'xah-insert-keymap)

  (define-key key-translation-map (kbd "<menu> u SPC") (kbd "_")) ; low line (underscore)
  (global-set-key (kbd "<menu> u RET") 'ucs-insert)

  (define-key key-translation-map (kbd "<menu> u .") nil)

  (define-key key-translation-map (kbd "<menu> u <down>") (kbd "↓"))
  (define-key key-translation-map (kbd "<menu> u <left>") (kbd "←"))
  (define-key key-translation-map (kbd "<menu> u <right>") (kbd "→"))
  (define-key key-translation-map (kbd "<menu> u <up>") (kbd "↑"))
  (define-key key-translation-map (kbd "<menu> u \\") (kbd "、")) ; IDEOGRAPHIC COMMA

  (global-set-key (kbd "<menu> u .") 'xah-insert-unicode)
  (global-set-key (kbd "<menu> u ,") nil)

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
  (global-set-key (kbd "<menu> u e") 'xah-insert-greater-less)
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
  (global-set-key (kbd "<menu> u p") 'xah-insert-single-angle-quote‹›)
  (global-set-key (kbd "<menu> u q") nil)
  (global-set-key (kbd "<menu> u r") 'xah-insert-tortoise-shell-bracket〔〕)
  (global-set-key (kbd "<menu> u s") nil)
  (global-set-key (kbd "<menu> u t") 'xah-insert-paren)
  (define-key key-translation-map (kbd "<menu> u u") (kbd "-")) ; minus

  (global-set-key (kbd "<menu> u v") nil)
  (global-set-key (kbd "<menu> u w") 'xah-insert-angle-bracket〈〉)
  (global-set-key (kbd "<menu> u x") nil)
  (global-set-key (kbd "<menu> u y") 'xah-insert-double-angle-quote«»)
  (global-set-key (kbd "<menu> u z") nil)

  )

(progn
  (define-prefix-command 'xah-menu-v-keymap)
  (global-set-key (kbd "<menu> v") xah-menu-v-keymap)

  (define-key xah-menu-v-keymap (kbd "+") 'vc-update)
  (define-key xah-menu-v-keymap (kbd "=") 'vc-diff)
  (define-key xah-menu-v-keymap (kbd "D") 'vc-root-diff)
  (define-key xah-menu-v-keymap (kbd "L") 'vc-print-root-log)
  (define-key xah-menu-v-keymap (kbd "a") 'vc-update-change-log)
  (define-key xah-menu-v-keymap (kbd "b") 'vc-switch-backend)
  (define-key xah-menu-v-keymap (kbd "c") 'vc-rollback)
  (define-key xah-menu-v-keymap (kbd "d") 'vc-dir)
  (define-key xah-menu-v-keymap (kbd "g") 'vc-annotate)
  (define-key xah-menu-v-keymap (kbd "h") 'vc-insert-headers)
  (define-key xah-menu-v-keymap (kbd "l") 'vc-print-log)
  (define-key xah-menu-v-keymap (kbd "m") 'vc-merge)
  (define-key xah-menu-v-keymap (kbd "r") 'vc-retrieve-tag)
  (define-key xah-menu-v-keymap (kbd "s") 'vc-create-tag)
  (define-key xah-menu-v-keymap (kbd "u") 'vc-revert)
  (define-key xah-menu-v-keymap (kbd "v") 'vc-next-action)
  (define-key xah-menu-v-keymap (kbd "~") 'vc-revision-other-window)

  )

(progn
  (define-prefix-command 'xah-menu-w-keymap)
  (global-set-key (kbd "<menu> w") xah-menu-w-keymap)

  (global-set-key (kbd "<menu> w .") 'eval-buffer)
  (global-set-key (kbd "<menu> w u") 'eval-region)
  (global-set-key (kbd "<menu> w m") 'eval-last-sexp)
  (global-set-key (kbd "<menu> w p") 'eval-expression)
  (global-set-key (kbd "<menu> w e") 'eval-defun)

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

;; • whether a command is frequently needed ⁖ few times a min, hour, day
;; • whether a command has immediate effect, no prompt. ⁖ kill-word vs shell, delete-matching-lines
;; • whether a command is safe to run by mistake. ⁖ whitespace-mode vs eval-buffer

;; idea about key groups
;; all should be sequence of single keys. 2 to 3 keys. All should start with F7. And all commands should be globally useful.
;; • 2 keys vs 3 keys
;; • whether the key ends in a digit key 0 to 9. These probably should be most frequently used, or immediate effect.



(progn
  ;; command dump. temp, rare, or whatever. put them here to have a key anyway
  (define-prefix-command 'xah-f10-keymap)
  (global-set-key (kbd "<f10>") 'xah-f10-keymap)

  (global-set-key (kbd "<f10> SPC") nil)
  (global-set-key (kbd "<f10> <return>") nil)

  (global-set-key (kbd "<f10> <f9>") nil)
  (global-set-key (kbd "<f10> <f10>") nil)
  (global-set-key (kbd "<f10> <f11>") nil)
  (global-set-key (kbd "<f10> <f12>") nil)

  (global-set-key (kbd "<f10> a") nil)
  (global-set-key (kbd "<f10> b") nil)
  (global-set-key (kbd "<f10> c") 'xah-css-mode)
  (global-set-key (kbd "<f10> d") nil)
  (global-set-key (kbd "<f10> e") 'xah-elisp-mode)
  (global-set-key (kbd "<f10> f") nil)
  (global-set-key (kbd "<f10> g") nil)
  (global-set-key (kbd "<f10> h") 'xah-html-mode)
  (global-set-key (kbd "<f10> i") nil)
  (global-set-key (kbd "<f10> j") 'xah-js-mode)
  (global-set-key (kbd "<f10> k") nil)
  (global-set-key (kbd "<f10> l") nil)
  (global-set-key (kbd "<f10> m") nil)
  (global-set-key (kbd "<f10> n") nil)
  (global-set-key (kbd "<f10> o") nil)
  (global-set-key (kbd "<f10> p") nil)
  (global-set-key (kbd "<f10> q") nil)
  (global-set-key (kbd "<f10> r") nil)
  (global-set-key (kbd "<f10> s") nil)
  (global-set-key (kbd "<f10> t") nil)
  (global-set-key (kbd "<f10> u") nil)
  (global-set-key (kbd "<f10> v") nil)
  (global-set-key (kbd "<f10> w") nil)
  (global-set-key (kbd "<f10> x") nil)
  (global-set-key (kbd "<f10> y") nil)
  (global-set-key (kbd "<f10> z") nil)
  )

;; C-x C-p	mark-page

;; C-x C-l	downcase-region
;; C-x C-u	upcase-region

;; C-x C-t	transpose-lines
;; C-x C-o	delete-blank-lines
;; C-x C-q	toggle-read-only

;; C-x C-r	find-file-read-only
;; C-x C-v	find-alternate-file
;; C-x C-w	write-file

;; C-x e	kmacro-end-and-call-macro
;; C-x q	kbd-macro-query
;; C-x C-k	kmacro-keymap

;; C-x C-c	save-buffers-kill-terminal
;; C-x C-d	list-directory
;; C-x C-n	set-goal-column
;; C-x C-z	suspend-frame
;; C-x ESC	Prefix Command
;; C-x $	set-selective-display
;; C-x *	calc-dispatch
;; C-x -	shrink-window-if-larger-than-buffer
;; C-x .	set-fill-prefix

;; C-x +	balance-windows
;; C-x 3	split-window-horizontally

;; C-x 4	ctl-x-4-prefix
;; C-x 5	ctl-x-5-prefix
;; C-x 6	2C-command
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

;; C-x n p	narrow-to-page

;; C-x r b	bookmark-jump
;; C-x r m	bookmark-set

;; ;; todo
;; select all, copy all, open, those standard keys

;; • add all emacs commands to my key sequence system

;; 'toggle-input-method

;; ;; 2013-11-04 make emacs auto show suggestions when a prefix key is pressed
;; (require 'guide-key)
;; (setq guide-key/guide-key-sequence '("<menu> t" "<tab> t" ))
;; (guide-key-mode 1)

(defun xah-open-in-external-app (&optional φfile)
  "Open the current φfile or dired marked files in external app.

The app is chosen from your OS's preference."
  (interactive)
  (let ( ξdoIt
         (ξfileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           ((not φfile) (list (buffer-file-name)))
           (φfile (list φfile)))))

    (setq ξdoIt (if (<= (length ξfileList) 5)
                   t
                 (y-or-n-p "Open more than 5 files? ") ) )

    (when ξdoIt
      (cond
       ((string-equal system-type "windows-nt")
        (mapc (lambda (fPath) (w32-shell-execute "open" (replace-regexp-in-string "/" "\\" fPath t t)) ) ξfileList))
       ((string-equal system-type "darwin")
        (mapc (lambda (fPath) (shell-command (format "open \"%s\"" fPath)) )  ξfileList) )
       ((string-equal system-type "gnu/linux")
        (mapc (lambda (fPath) (let ((process-connection-type nil)) (start-process "" nil "xdg-open" fPath)) ) ξfileList) ) ) ) ) )

(defun xah-open-in-desktop ()
  "Show current file in desktop (OS's file manager)."
  (interactive)
  (cond
   ((string-equal system-type "windows-nt")
    (w32-shell-execute "explore" (replace-regexp-in-string "/" "\\" default-directory t t)))
   ((string-equal system-type "darwin") (shell-command "open ."))
   ((string-equal system-type "gnu/linux")
    (let ((process-connection-type nil)) (start-process "" nil "xdg-open" "."))
    ;; (shell-command "xdg-open .") ;; 2013-02-10 this sometimes froze emacs till the folder is closed. ⁖ with nautilus
    ) ))

(defun xah-new-empty-buffer ()
  "Open a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall (and initial-major-mode))
    (setq buffer-offer-save t)))
;; note: emacs won't offer to save a buffer that's
;; not associated with a file,
;; even if buffer-modified-p is true.
;; One work around is to define your own my-kill-buffer function
;; that wraps around kill-buffer, and check on the buffer modification
;; status to offer save
;; This custome kill buffer is close-current-buffer.


;; personal

(progn
  ;; this should reserved for user-defined keys
  (define-prefix-command 'xah-user-keymap)
  (global-set-key (kbd "<menu> SPC") xah-user-keymap)

  (define-key xah-user-keymap (kbd "<menu>") 'xah-open-file-fast)
  (define-key xah-user-keymap (kbd "<return>") 'xah-run-current-file)
  (define-key xah-user-keymap (kbd "<backspace>") 'xah-delete-current-file)
  (define-key xah-user-keymap (kbd "<tab>") nil)
  (define-key xah-user-keymap (kbd "<delete>") nil)
  (define-key xah-user-keymap (kbd "<home>") nil)
  (define-key xah-user-keymap (kbd "<end>") nil)

  (define-key xah-user-keymap (kbd "-") 'xah-insert-form-feed)
  (define-key xah-user-keymap (kbd ".") 'title-case-string-region-or-line)

  (define-key xah-user-keymap (kbd "1") 'xah-copy-to-register-1)
  (define-key xah-user-keymap (kbd "2") 'xah-paste-from-register-1)
  (define-key xah-user-keymap (kbd "3") nil)
  (define-key xah-user-keymap (kbd "4") nil)
  (define-key xah-user-keymap (kbd "7") nil)
  (define-key xah-user-keymap (kbd "8") nil)

  (define-key xah-user-keymap (kbd "a") 'ace-jump-mode-pop-mark)
  (define-key xah-user-keymap (kbd "b") 'xah-shell-commands)
  (define-key xah-user-keymap (kbd "c") 'xah-cite)
  (define-key xah-user-keymap (kbd "d") 'insert-date)
  (define-key xah-user-keymap (kbd "e") nil)
  (define-key xah-user-keymap (kbd "f") 'xah-find-text)
  (define-key xah-user-keymap (kbd "g") 'ace-jump-mode)

  (define-key xah-user-keymap (kbd "h") nil)
  (define-key xah-user-keymap (kbd "i n") 'xah-insert-random-number)
  (define-key xah-user-keymap (kbd "i s") 'xah-insert-random-string)
  (define-key xah-user-keymap (kbd "i u") 'xah-insert-random-uuid)
  (define-key xah-user-keymap (kbd "i x") 'xah-insert-random-hex)
  (define-key xah-user-keymap (kbd "j") nil)
  (define-key xah-user-keymap (kbd "k") nil)
  (define-key xah-user-keymap (kbd "l") nil)
  (define-key xah-user-keymap (kbd "m") 'magit-status)
  (define-key xah-user-keymap (kbd "n") 'xah-make-backup)
  (define-key xah-user-keymap (kbd "o") 'xah-open-file-from-clipboard)
  (define-key xah-user-keymap (kbd "p") 'xah-copy-file-path)
  (define-key xah-user-keymap (kbd "q") nil)
  (define-key xah-user-keymap (kbd "r c") 'xah-escape-quotes)
  (define-key xah-user-keymap (kbd "r '") 'xah-replace-straight-quotes)
  (define-key xah-user-keymap (kbd "r ,") 'xah-remove-punctuation-trailing-redundant-space)
  (define-key xah-user-keymap (kbd "r .") 'xah-convert-english-chinese-punctuation)
  (define-key xah-user-keymap (kbd "r [") 'xah-remove-square-brackets)
  (define-key xah-user-keymap (kbd "r g") 'xah-convert-latin-alphabet-gothic)
  (define-key xah-user-keymap (kbd "r p") 'xah-convert-asian/ascii-space)
  (define-key xah-user-keymap (kbd "r w") 'xah-convert-fullwidth-chars)
  (define-key xah-user-keymap (kbd "s") nil)
  (define-key xah-user-keymap (kbd "t") nil)
  (define-key xah-user-keymap (kbd "u") 'xah-find-replace-text)
  (define-key xah-user-keymap (kbd "v") nil)
  (define-key xah-user-keymap (kbd "w") nil)
  (define-key xah-user-keymap (kbd "y") nil)
  (define-key xah-user-keymap (kbd "z") 'xah-toggle-read-novel-mode))

(substitute-key-definition 'find-file-at-point 'xah-open-file-at-cursor (current-global-map))
