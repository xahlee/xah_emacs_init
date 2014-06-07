;; -*- coding: utf-8 -*-
;; xah's emacs keybinding.

;; jump points to other files

;; • xah_emacs_init.el
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

(global-set-key (kbd "<menu> <return>") 'smex) ; 3459    0.21%  smex
                                        ;(global-set-key (kbd "<menu> <backspace>") 'delete-indentation)
(global-set-key (kbd "<menu> <backspace>") 'xah-delete-cut-text-block)
(global-set-key (kbd "<menu> <tab>") 'indent-for-tab-command)
(global-set-key (kbd "<menu> <f2>") 'xah-cut-all)
(global-set-key (kbd "<menu> <f3>") 'xah-copy-all)

;; (global-set-key (kbd "<menu> SPC") (lambda () (interactive) (insert "_"))) ; low line (underscore)

;; xah-cycle-camel-style-case

(global-set-key (kbd "<menu> .") 'universal-argument) ; ★★
(global-set-key (kbd "<menu> '") nil) ;
(global-set-key (kbd "<menu> ,") 'toggle-input-method)

(global-set-key (kbd "<menu> -") 'xah-insert-form-feed)
(global-set-key (kbd "<menu> /") nil)
(global-set-key (kbd "<menu> ;") nil)
(global-set-key (kbd "<menu> =") nil)
(global-set-key (kbd "<menu> [") nil)
(global-set-key (kbd "<menu> \\") 'xah-escape-quotes)
(global-set-key (kbd "<menu> `") 'xah-make-backup)

;; xah's
   ;; 5064    0.31%  delete-other-windows
   ;; 1384    0.09%  ergoemacs-move-cursor-next-pane
   ;; 2742    0.17%  split-window-vertically
   ;; 1136    0.07%  delete-window

;; all people
;; 18   other-window            19256   0.41    0.79
;; 34   move-cursor-next-pane   12183   0.26    0.50
;; 49   delete-other-windows    6968    0.15    0.29
;; 67	split-window-vertically	4035	0.09	0.17

(global-set-key (kbd "<menu> 0") 'end-of-buffer)
(global-set-key (kbd "<menu> 1") 'beginning-of-buffer)
(global-set-key (kbd "<menu> 2") 'delete-window) ;
(global-set-key (kbd "<menu> 3") 'delete-other-windows) ;
(global-set-key (kbd "<menu> 4") 'split-window-vertically) ;
(global-set-key (kbd "<menu> 5") 'shell-command) ;  ; 274    0.02%  shell-command
(global-set-key (kbd "<menu> 6") 'write-file)
(global-set-key (kbd "<menu> 7") 'xah-open-file-at-cursor) ;  find-file-at-point 4773    0.30%  xah-open-file-at-cursor
(global-set-key (kbd "<menu> 8") 'dired-jump)              ;  2377    0.15%  dired-jump
(global-set-key (kbd "<menu> 9") 'ispell-word)

(progn
  (define-prefix-command 'xah-menu-a-keymap)
  (global-set-key (kbd "<menu> a") xah-menu-a-keymap)
  )

(progn
  (define-prefix-command 'xah-menu-b-keymap)
  (global-set-key (kbd "<menu> b") xah-menu-b-keymap)
  )

(progn
  (define-prefix-command 'xah-menu-c-keymap)
  (global-set-key (kbd "<menu> c") xah-menu-c-keymap)
  )

(progn
  (define-prefix-command 'xah-menu-d-keymap)
  (global-set-key (kbd "<menu> d") xah-menu-d-keymap)
  )

(progn
;; these are all kinda non-risky commands. that is, they change display, or do prompt, etc. it's ok to accidentally run them
  (define-prefix-command 'xah-menu-e-keymap)
  (global-set-key (kbd "<menu> e") xah-menu-e-keymap)

  (global-set-key (kbd "<menu> e SPC") 'flyspell-buffer) ; 306    0.02%  flyspell-buffer

  (global-set-key (kbd "<menu> e 3") 'whitespace-mode)
  (global-set-key (kbd "<menu> e 4") 'linum-mode)
  (global-set-key (kbd "<menu> e 5") 'visual-line-mode)
  (global-set-key (kbd "<menu> e 6") 'calendar)
  (global-set-key (kbd "<menu> e 7") 'calc)
  (global-set-key (kbd "<menu> e 8") 'shell)
  (global-set-key (kbd "<menu> e b") 'toggle-debug-on-error)
  (global-set-key (kbd "<menu> e e") 'xah-elisp-mode)
  (global-set-key (kbd "<menu> e h") 'xah-html-mode)
  (global-set-key (kbd "<menu> e c") 'toggle-case-fold-search)
  (global-set-key (kbd "<menu> e v") 'desktop-save)
  (global-set-key (kbd "<menu> e t e") 'kill-rectangle)
  (global-set-key (kbd "<menu> e t u") 'replace-rectangle)
  (global-set-key (kbd "<menu> e t y") 'yank-rectangle)
  (global-set-key (kbd "<menu> e t n") 'rectangle-number-lines)
  (global-set-key (kbd "<menu> e t d") 'delete-rectangle)
  (global-set-key (kbd "<menu> e t c") 'clear-rectangle)
  (global-set-key (kbd "<menu> e t w") 'delete-whitespace-rectangle)
  (global-set-key (kbd "<menu> e t o") 'open-rectangle)

  )

(progn
  (define-prefix-command 'xah-menu-f-keymap)
  (global-set-key (kbd "<menu> f") xah-menu-f-keymap)
  )

(global-set-key (kbd "<menu> g") 'isearch-forward)

; 432    0.03%  list-matching-lines

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
  (global-set-key (kbd "<menu> h j") 'Info-goto-emacs-key-command-node)
  (global-set-key (kbd "<menu> h k") 'describe-key)
  (global-set-key (kbd "<menu> h K") 'Info-goto-emacs-key-command-node)
  (global-set-key (kbd "<menu> h l") 'view-lossage)
  (global-set-key (kbd "<menu> h L") 'describe-language-environment)
  (global-set-key (kbd "<menu> h m") 'xah-describe-major-mode)
  (global-set-key (kbd "<menu> h n") 'view-emacs-news)
  (global-set-key (kbd "<menu> h o") nil)
  (global-set-key (kbd "<menu> h p") 'finder-by-keyword)
  (global-set-key (kbd "<menu> h q") nil)
  (global-set-key (kbd "<menu> h r") 'info-emacs-manual)
  (global-set-key (kbd "<menu> h s") 'describe-syntax)
  (global-set-key (kbd "<menu> h S") 'info-lookup-symbol)
  (global-set-key (kbd "<menu> h t") nil)
  (global-set-key (kbd "<menu> h u") nil)
  (global-set-key (kbd "<menu> h v") 'describe-variable)
  (global-set-key (kbd "<menu> h w") 'where-is)
  (global-set-key (kbd "<menu> h x") nil)
  (global-set-key (kbd "<menu> h y") nil)
  (global-set-key (kbd "<menu> h z") nil)
  )

(progn
  (define-prefix-command 'xah-menu-i-keymap)
  (global-set-key (kbd "<menu> i") xah-menu-i-keymap)
  )

(progn
  (define-prefix-command 'xah-menu-j-keymap)
  (global-set-key (kbd "<menu> j") xah-menu-j-keymap)
  )

(global-set-key (kbd "<menu> k") 'xah-clean-whitespace)
(global-set-key (kbd "<menu> l") 'recenter-top-bottom)

(global-set-key (kbd "<menu> m") search-map)

(progn
  (define-prefix-command 'xah-menu-n-keymap)
  (global-set-key (kbd "<menu> n") xah-menu-n-keymap)
  (global-set-key (kbd "<menu> n h") 'narrow-to-region)
  (global-set-key (kbd "<menu> n t") 'narrow-to-defun)
  (global-set-key (kbd "<menu> n w") 'widen)
  )

(progn
  (define-prefix-command 'xah-menu-o-keymap)
  (global-set-key (kbd "<menu> o") xah-menu-o-keymap)
  (global-set-key (kbd "<menu> o c") 'bookmark-bmenu-list)
  (global-set-key (kbd "<menu> o g") 'ibuffer) ; 198    0.01%  ibuffer
  (global-set-key (kbd "<menu> o h") 'recentf-open-files) ;  333    0.02%  recentf-open-files
  (global-set-key (kbd "<menu> o t") 'ido-switch-buffer)  ; 33    0.00%  ido-switch-buffer
  (global-set-key (kbd "<menu> o d") 'xah-open-in-desktop) ; 325    0.02%  xah-open-in-desktop
  )

(global-set-key (kbd "<menu> p") 'query-replace) ; 2746    0.17%  query-replace

(progn
;; kinda replacement related
  (define-prefix-command 'xah-menu-r-keymap)
  (global-set-key (kbd "<menu> r") xah-menu-r-keymap)

  (global-set-key (kbd "<menu> r d") 'delete-matching-lines) ; ★★     317    0.02%  delete-matching-lines
  (global-set-key (kbd "<menu> r b") 'delete-non-matching-lines)

  (global-set-key (kbd "<menu> r f") 'xah-find-text)

  (global-set-key (kbd "<menu> r u") 'query-replace-regexp) ; 288    0.02%  query-replace-regexp

  (global-set-key (kbd "<menu> r 1") 'kmacro-start-macro)
  (global-set-key (kbd "<menu> r 2") 'kmacro-end-macro)
  (global-set-key (kbd "<menu> r 3") 'call-last-kbd-macro) ; control something
  (global-set-key (kbd "<menu> r 4") 'apply-macro-to-region-lines)
  (global-set-key (kbd "<f10>") 'call-last-kbd-macro)
  )

(global-set-key (kbd "<menu> s") 'save-buffer) ; 25468    1.58%  save-buffer

(progn
  ;; this should reserved for user-defined keys
  (define-prefix-command 'xah-menu-t-keymap)
  (global-set-key (kbd "<menu> t") xah-menu-t-keymap)

  (global-set-key (kbd "<menu> t .") 'title-case-string-region-or-line)
  (global-set-key (kbd "<menu> t 7") 'xah-copy-to-register-1)
  (global-set-key (kbd "<menu> t 8") 'xah-paste-from-register-1)

  (global-set-key (kbd "<menu> t a") nil)
  (global-set-key (kbd "<menu> t b") 'xah-shell-commands)
  (global-set-key (kbd "<menu> t c") 'xah-cite)
  (global-set-key (kbd "<menu> t d") 'insert-date)
  (global-set-key (kbd "<menu> t e") 'xah-find-replace-text)
  (global-set-key (kbd "<menu> t f") 'xah-open-file-from-clipboard)
  (global-set-key (kbd "<menu> t g") 'ace-jump-mode)
  (global-set-key (kbd "<menu> t h") nil)
  (global-set-key (kbd "<menu> t i n") 'xah-insert-random-number)
  (global-set-key (kbd "<menu> t i s") 'xah-insert-random-string)
  (global-set-key (kbd "<menu> t i u") 'xah-insert-random-uuid)
  (global-set-key (kbd "<menu> t i x") 'xah-insert-random-hex)
  (global-set-key (kbd "<menu> t i") nil)
  (global-set-key (kbd "<menu> t j") 'xah-copy-all)
  (global-set-key (kbd "<menu> t j") nil)
  (global-set-key (kbd "<menu> t k") nil)
  (global-set-key (kbd "<menu> t l") nil)
  (global-set-key (kbd "<menu> t m") 'magit-status)
  (global-set-key (kbd "<menu> t n") nil)
  (global-set-key (kbd "<menu> t o") nil)
  (global-set-key (kbd "<menu> t p") 'xah-copy-file-path) ;  2041    0.13%  xah-copy-file-path
  (global-set-key (kbd "<menu> t q") 'xah-cut-all)
  (global-set-key (kbd "<menu> t r '") 'xah-replace-straight-quotes)
  (global-set-key (kbd "<menu> t r ,") 'xah-remove-punctuation-trailing-redundant-space)
  (global-set-key (kbd "<menu> t r .") 'xah-convert-english-chinese-punctuation)
  (global-set-key (kbd "<menu> t r [") 'xah-remove-square-brackets)
  (global-set-key (kbd "<menu> t r g") 'xah-convert-latin-alphabet-gothic)
  (global-set-key (kbd "<menu> t r p") 'xah-convert-asian/ascii-space)
  (global-set-key (kbd "<menu> t r w") 'xah-convert-fullwidth-chars)
  (global-set-key (kbd "<menu> t r") 'xah-toggle-read-novel-mode)
  (global-set-key (kbd "<menu> t s") nil)
  (global-set-key (kbd "<menu> t t") nil)
  (global-set-key (kbd "<menu> t u") 'xah-open-file-fast)
  (global-set-key (kbd "<menu> t v") nil)
  (global-set-key (kbd "<menu> t w") nil)
  (global-set-key (kbd "<menu> t x") nil)
  (global-set-key (kbd "<menu> t y") 'yas/expand)
  (global-set-key (kbd "<menu> t z") nil)

  )

(progn
  (define-prefix-command 'xah-menu-u-keymap)
  (global-set-key (kbd "<menu> u") xah-menu-u-keymap)
  (global-set-key (kbd "<menu> u t") 'repeat-complex-command)
  )

(progn
  (define-prefix-command 'xah-menu-v-keymap)
  (global-set-key (kbd "<menu> v") xah-menu-v-keymap)
  (global-set-key (kbd "<menu> v b") 'eval-buffer)
  (global-set-key (kbd "<menu> v r") 'eval-region)
  (global-set-key (kbd "<menu> v d") 'eval-defun)
  (global-set-key (kbd "<menu> v e") 'eval-expression)
  (global-set-key (kbd "<menu> v l") 'eval-last-sexp)
  (global-set-key (kbd "<menu> v <backspace>") 'xah-delete-current-file)
  (global-set-key (kbd "<menu> v <return>") 'xah-run-current-file) ;  1494    0.09%  xah-run-current-file
  )

(progn
  (define-prefix-command 'xah-menu-w-keymap)
  (global-set-key (kbd "<menu> w") xah-menu-w-keymap)
  (global-set-key (kbd "<menu> w <left>") 'xah-goto-previous-overlay)
  (global-set-key (kbd "<menu> w <right>") 'xah-goto-next-overlay)
  (global-set-key (kbd "<menu> w <backspace>") 'xah-remove-overlays-region)
  (global-set-key (kbd "<menu> w <return>") 'xah-show-overlay-at-point)

  (global-set-key (kbd "<menu> w b") 'xah-make-overlay-bold-region)
  (global-set-key (kbd "<menu> w a") 'xah-show-all-overlays)

  (global-set-key (kbd "<menu> w 8") 'xah-syntax-bracket-forward)
  (global-set-key (kbd "<menu> w 7") 'xah-syntax-bracket-backward)
  (global-set-key (kbd "<menu> w c") 'xah-forward-comment)
  (global-set-key (kbd "<menu> w l") 'xah-scan-list)
  (global-set-key (kbd "<menu> w s") 'xah-scan-sexps)
  (global-set-key (kbd "<menu> w p") 'xah-parse-partial-sexp)

  )

(progn
  (define-prefix-command 'xah-menu-x-keymap)
  (global-set-key (kbd "<menu> x") xah-menu-x-keymap)
  )

(progn
  (define-prefix-command 'xah-menu-y-keymap)
  (global-set-key (kbd "<menu> y") xah-menu-y-keymap)
  )

(global-set-key (kbd "<menu> z") 'xc-comment-smart) ; 385    0.02%  xc-comment-smart


;;;; misc

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

;'indent-for-tab-command



;shell-command-on-region
; 16    0.00%  shell-command-on-region



;; ;; 2013-11-04 make emacs auto show suggestions when a prefix key is pressed
;; (require 'guide-key)
;; (setq guide-key/guide-key-sequence '("<menu> t" "<tab> t" ))
;; (guide-key-mode 0)

; 5605    0.35%  cua-set-mark; 944    0.06%  set-mark-command
;   6077    0.38%  ergoemacs-M-o
;;  toggle-input-method

 ;  42    0.00%  kmacro-start-macro
 ; 36    0.00%  kmacro-end-macro

;; ;; todo add:
  ;; (global-set-key (kbd "<menu> r 4") 'set-mark-command)

;; C-x C-@		pop-global-mark
;; C-x C-b		list-buffers
;; C-x C-c		save-buffers-kill-terminal
;; C-x C-d		list-directory
;; C-x C-e		eval-last-sexp
;; C-x C-f		find-file
;; C-x TAB		indent-rigidly
;; C-x C-k		kmacro-keymap
;; C-x C-l		downcase-region
;; C-x RET		Prefix Command
;; C-x C-n		set-goal-column
;; C-x C-o		delete-blank-lines
;; C-x C-p		mark-page
;; C-x C-q		toggle-read-only
;; C-x C-r		find-file-read-only
;; C-x C-s		save-buffer
;; C-x C-t		transpose-lines
;; C-x C-u		upcase-region
;; C-x C-v		find-alternate-file
;; C-x C-w		write-file
;; C-x C-x		exchange-point-and-mark
;; C-x C-z		suspend-frame
;; C-x ESC		Prefix Command
;; C-x $		set-selective-display
;; C-x '		expand-abbrev
;; C-x (		kmacro-start-macro
;; C-x )		kmacro-end-macro
;; C-x *		calc-dispatch
;; C-x +		balance-windows
;; C-x -		shrink-window-if-larger-than-buffer
;; C-x .		set-fill-prefix
;; C-x 0		delete-window
;; C-x 1		delete-other-windows
;; C-x 2		split-window-vertically
;; C-x 3		split-window-horizontally
;; C-x 4		ctl-x-4-prefix
;; C-x 5		ctl-x-5-prefix
;; C-x 6		2C-command
;; C-x 8		Prefix Command
;; C-x ;		comment-set-column
;; C-x <		scroll-left
;; C-x =		what-cursor-position
;; C-x >		scroll-right
;; C-x [		backward-page
;; C-x ]		forward-page
;; C-x ^		enlarge-window
;; C-x `		next-error
;; C-x a		Prefix Command
;; C-x b		switch-to-buffer
;; C-x d		dired
;; C-x e		kmacro-end-and-call-macro
;; C-x f		set-fill-column
;; C-x h		mark-whole-buffer
;; C-x i		insert-file
;; C-x k		kill-buffer
;; C-x l		count-lines-page
;; C-x m		compose-mail
;; C-x n		Prefix Command
;; C-x o		other-window
;; C-x q		kbd-macro-query
;; C-x r		Prefix Command
;; C-x s		save-some-buffers
;; C-x u		undo
;; C-x v		vc-prefix-map
;; C-x z		repeat
;; C-x {		shrink-window-horizontally
;; C-x }		enlarge-window-horizontally
;; C-x DEL		backward-kill-sentence
;; C-x C-SPC	pop-global-mark
;; C-x C-+		text-scale-adjust
;; C-x C--		text-scale-adjust
;; C-x C-0		text-scale-adjust
;; C-x C-=		text-scale-adjust
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
;; C-x ESC ESC	repeat-complex-command
;; C-x M-:		repeat-complex-command
;; C-x 4 C-f	find-file-other-window
;; C-x 4 C-o	display-buffer
;; C-x 4 .		find-tag-other-window
;; C-x 4 0		kill-buffer-and-window
;; C-x 4 a		add-change-log-entry-other-window
;; C-x 4 b		switch-to-buffer-other-window
;; C-x 4 c		clone-indirect-buffer-other-window
;; C-x 4 d		dired-other-window
;; C-x 4 f		find-file-other-window
;; C-x 4 m		compose-mail-other-window
;; C-x 4 r		find-file-read-only-other-window
;; C-x 5 C-f	find-file-other-frame
;; C-x 5 C-o	display-buffer-other-frame
;; C-x 5 .		find-tag-other-frame
;; C-x 5 0		delete-frame
;; C-x 5 1		delete-other-frames
;; C-x 5 2		make-frame-command
;; C-x 5 b		switch-to-buffer-other-frame
;; C-x 5 d		dired-other-frame
;; C-x 5 f		find-file-other-frame
;; C-x 5 m		compose-mail-other-frame
;; C-x 5 o		other-frame
;; C-x 5 r		find-file-read-only-other-frame
;; C-x 6 2		2C-two-columns
;; C-x 6 b		2C-associate-buffer
;; C-x 6 s		2C-split
;; C-x 6 <f2>	2C-two-columns
;; C-x 8 RET	ucs-insert
;; C-x a C-a	add-mode-abbrev
;; C-x a '		expand-abbrev
;; C-x a +		add-mode-abbrev
;; C-x a -		inverse-add-global-abbrev
;; C-x a e		expand-abbrev
;; C-x a g		add-global-abbrev
;; C-x a i		Prefix Command
;; C-x a l		add-mode-abbrev
;; C-x a n		expand-jump-to-next-slot
;; C-x a p		expand-jump-to-previous-slot
;; C-x n d		narrow-to-defun
;; C-x n n		narrow-to-region
;; C-x n p		narrow-to-page
;; C-x n w		widen
;; C-x r C-@	point-to-register
;; C-x r SPC	point-to-register
;; C-x r +		increment-register
;; C-x r b		bookmark-jump
;; C-x r c		clear-rectangle
;; C-x r d		delete-rectangle
;; C-x r f		frame-configuration-to-register
;; C-x r g		insert-register
;; C-x r i		insert-register
;; C-x r j		jump-to-register
;; C-x r k		kill-rectangle
;; C-x r l		bookmark-bmenu-list
;; C-x r m		bookmark-set
;; C-x r n		number-to-register
;; C-x r o		open-rectangle
;; C-x r r		copy-rectangle-to-register
;; C-x r s		copy-to-register
;; C-x r t		string-rectangle
;; C-x r w		window-configuration-to-register
;; C-x r x		copy-to-register
;; C-x r y		yank-rectangle
;; C-x r C-SPC	point-to-register
;; C-x v +		vc-update
;; C-x v =		vc-diff
;; C-x v D		vc-root-diff
;; C-x v L		vc-print-root-log
;; C-x v a		vc-update-change-log
;; C-x v b		vc-switch-backend
;; C-x v c		vc-rollback
;; C-x v d		vc-dir
;; C-x v g		vc-annotate
;; C-x v h		vc-insert-headers
;; C-x v i		vc-register
;; C-x v l		vc-print-log
;; C-x v m		vc-merge
;; C-x v r		vc-retrieve-tag
;; C-x v s		vc-create-tag
;; C-x v u		vc-revert
;; C-x v v		vc-next-action
;; C-x v ~		vc-revision-other-window
;; C-x a i g	inverse-add-global-abbrev
;; C-x a i l	inverse-add-mode-abbrev
;; C-x		Prefix Command
;; C-x @		Prefix Command
;; C-x @ S		event-apply-shift-modifier
;; C-x @ a		event-apply-alt-modifier
;; C-x @ c		event-apply-control-modifier
;; C-x @ h		event-apply-hyper-modifier
;; C-x @ m		event-apply-meta-modifier
;; C-x @ s		event-apply-super-modifier

;; ;; todo
;; select all, copy all, open, those standard keys
;; register stuff