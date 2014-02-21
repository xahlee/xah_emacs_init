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
;; • xah_emacs_keybinding_ergoemacs_vi.el
;; • xah_emacs_keybinding_functions.el
;; • xah_emacs_keybinding_mode_specific.el
;; • xah_emacs_keybinding_special_keys.el
;; • xah_emacs_keybinding_unset_keys.el
;; • xah_emacs_mouse_binding.el
;; • xah_emacs_unicode_input.el

;; ~/git/ergoemacs/ergoemacs/ergoemacs-keybindings/ergoemacs-variants.el

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



(require 'package)
(package-initialize)
(require 'highlight-symbol nil "noerror")



;; stars indicate frequency of use
;; ★★★ every minute
;; ★★ every hour
;; ★ few times a day

(global-set-key (kbd "<menu> <menu>") 'exchange-point-and-mark)
(global-set-key (kbd "<menu> <return>") 'smex) ; 3459    0.21%  smex
                                        ;(global-set-key (kbd "<menu> <backspace>") 'delete-indentation)
(global-set-key (kbd "<menu> <backspace>") 'xah-delete-cut-text-block)
(global-set-key (kbd "<menu> <tab>") 'indent-for-tab-command)
(global-set-key (kbd "<menu> <f2>") 'xah-cut-all)
(global-set-key (kbd "<menu> <f3>") 'xah-copy-all)

(progn
  (global-set-key (kbd "<menu> <f8>") 'highlight-symbol-at-point)
  (global-set-key (kbd "<C-f8>") 'highlight-symbol-prev)
  (global-set-key (kbd "<C-f9>") 'highlight-symbol-next)
  (global-set-key (kbd "<menu> <f9>") 'highlight-symbol-query-replace)
  )

;; xah-cycle-camel-style-case

(global-set-key (kbd "<menu> .") 'universal-argument) ; ★★
(global-set-key (kbd "<menu> '") nil) ;
(global-set-key (kbd "<menu> ,") 'toggle-input-method)

(global-set-key (kbd "<menu> -") nil)

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

(global-set-key (kbd "<menu> /") nil)
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
(global-set-key (kbd "<menu> ;") nil) ;
(global-set-key (kbd "<menu> =") nil)
(global-set-key (kbd "<menu> SPC") nil)
(global-set-key (kbd "<menu> [") nil)
(global-set-key (kbd "<menu> \\") 'xah-escape-quotes)
(global-set-key (kbd "<menu> `") 'make-backup)
(global-set-key (kbd "<menu> a") 'mark-whole-buffer) ;★★     408    0.03%  mark-whole-buffer
(global-set-key (kbd "<menu> b") 'xah-shell-commands)
(global-set-key (kbd "<menu> c") 'xah-open-file-fast)

(global-set-key (kbd "<menu> d") 'yas/expand)

(global-set-key (kbd "<menu> e") nil) ;
(global-set-key (kbd "<menu> e 3") 'whitespace-mode)
(global-set-key (kbd "<menu> e 4") 'linum-mode)
(global-set-key (kbd "<menu> e 5") 'visual-line-mode)
(global-set-key (kbd "<menu> e 6") 'calendar)
(global-set-key (kbd "<menu> e 7") 'calc)
(global-set-key (kbd "<menu> e 8") 'shell)
(global-set-key (kbd "<menu> e e") 'xah-elisp-mode)
(global-set-key (kbd "<menu> e h") 'xah-html-mode)
(global-set-key (kbd "<menu> e SPC") 'flyspell-buffer) ; 306    0.02%  flyspell-buffer
(global-set-key (kbd "<menu> e <backspace>") 'delete-current-file)
(global-set-key (kbd "<menu> e <return>") 'run-current-file) ;  1494    0.09%  run-current-file

(global-set-key (kbd "<menu> f") 'xah-copy-file-path) ;  2041    0.13%  xah-copy-file-path

                                        ; ~/git/xah_emacs_init/xah_emacs_keybinding_shift_switch.el
(global-set-key (kbd "<menu> g") 'list-matching-lines) ; 432    0.03%  list-matching-lines

(progn
  (global-set-key (kbd "<menu> h") nil)
  (global-set-key (kbd "<menu> h 0") 'lookup-all-dictionaries)
  (global-set-key (kbd "<menu> h 1") nil)
  (global-set-key (kbd "<menu> h 2") nil)
  (global-set-key (kbd "<menu> h 3") 'man)
  (global-set-key (kbd "<menu> h 4") 'elisp-index-search)
  (global-set-key (kbd "<menu> h 5") nil)
  (global-set-key (kbd "<menu> h 6") nil)
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
  (global-set-key (kbd "<menu> h m") 'xah-describe-major-mode)
  (global-set-key (kbd "<menu> h n") 'view-emacs-news)
  (global-set-key (kbd "<menu> h p") 'finder-by-keyword)
  (global-set-key (kbd "<menu> h r") 'info-emacs-manual)
  (global-set-key (kbd "<menu> h s") 'describe-syntax)
  (global-set-key (kbd "<menu> h S") 'info-lookup-symbol)
  (global-set-key (kbd "<menu> h v") 'describe-variable)
  (global-set-key (kbd "<menu> h w") 'where-is)
  )

(progn
  (global-set-key (kbd "<menu> i") nil)
  (global-set-key (kbd "<menu> i d") 'insert-date)
  (global-set-key (kbd "<menu> i n") 'ξ-insert-random-number)
  (global-set-key (kbd "<menu> i x") 'ξ-insert-random-hex)
  (global-set-key (kbd "<menu> i s") 'ξ-insert-random-string)
  (global-set-key (kbd "<menu> i u") 'ξ-insert-random-uuid)
  )

                                        ;(global-set-key (kbd "<menu> j") 'kmacro-start-macro) ;  42    0.00%  kmacro-start-macro
                                        ;(global-set-key (kbd "<menu> k") 'kmacro-end-macro)   ; 36    0.00%  kmacro-end-macro
(global-set-key (kbd "<menu> j") 'xah-copy-all)
(global-set-key (kbd "<menu> k") 'xah-clean-whitespace)
(global-set-key (kbd "<menu> l") 'recenter-top-bottom)

(global-set-key (kbd "<menu> m") nil)

(global-set-key (kbd "<menu> n") 'narrow-to-region)
(global-set-key (kbd "<menu> o") nil)
(global-set-key (kbd "<menu> o c") 'bookmark-bmenu-list)
(global-set-key (kbd "<menu> o g") 'ibuffer) ; 198    0.01%  ibuffer
(global-set-key (kbd "<menu> o h") 'recentf-open-files) ;  333    0.02%  recentf-open-files
(global-set-key (kbd "<menu> o t") 'ido-switch-buffer)  ; 33    0.00%  ido-switch-buffer

(global-set-key (kbd "<menu> o d") 'xah-open-in-desktop) ; 325    0.02%  xah-open-in-desktop

(global-set-key (kbd "<menu> p") 'query-replace) ; 2746    0.17%  query-replace

(global-set-key (kbd "<menu> q") 'xah-cut-all)
(global-set-key (kbd "<menu> r") nil)
(global-set-key (kbd "<menu> r [") 'remove-square-brackets)
(global-set-key (kbd "<menu> r '") 'xah-replace-straight-quotes)
(global-set-key (kbd "<menu> r ,") 'xah-remove-punctuation-trailing-redundant-space)
(global-set-key (kbd "<menu> r .") 'xah-convert-english-chinese-punctuation)
(global-set-key (kbd "<menu> r d") 'delete-matching-lines) ; ★★     317    0.02%  delete-matching-lines
(global-set-key (kbd "<menu> r f") 'xah-find-text)
(global-set-key (kbd "<menu> r g") 'xah-convert-latin-alphabet-gothic)
(global-set-key (kbd "<menu> r p") 'xah-convert-asian/ascii-space)
(global-set-key (kbd "<menu> r r") 'xah-find-replace-text)
(global-set-key (kbd "<menu> r u") 'query-replace-regexp) ; 288    0.02%  query-replace-regexp
(global-set-key (kbd "<menu> r w") 'xah-convert-fullwidth-chars)
(global-set-key (kbd "<menu> s") 'save-buffer) ; 25468    1.58%  save-buffer

(progn

  (global-set-key (kbd "<menu> t") nil)
  (global-set-key (kbd "<menu> t 2") 'make-frame-command)
  (global-set-key (kbd "<menu> t 3") 'xah-new-empty-buffer)
  (global-set-key (kbd "<menu> t 4") 'find-file)
  (global-set-key (kbd "<menu> t c") 'xah-cite)
  (global-set-key (kbd "<menu> t f") 'xah-open-file-from-clipboard)
  (global-set-key (kbd "<menu> t j") 'xah-copy-to-register-1)
  (global-set-key (kbd "<menu> t k") 'xah-paste-from-register-1)
  (global-set-key (kbd "<menu> t r") 'repeat-complex-command)
  (global-set-key (kbd "<menu> t s") 'title-case-string-region-or-line)
  )

(progn
  (global-set-key (kbd "<menu> u") nil) ;
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

  (global-set-key (kbd "<menu> u 1") "!")
  (global-set-key (kbd "<menu> u 2") "@")
  (global-set-key (kbd "<menu> u 3") "#") ;
  (global-set-key (kbd "<menu> u 4") "$") ;
  (global-set-key (kbd "<menu> u 5") "%") ;
  (global-set-key (kbd "<menu> u 6") "^")
  (global-set-key (kbd "<menu> u 7") "&") ;
  (global-set-key (kbd "<menu> u 8") "*") ;
  (global-set-key (kbd "<menu> u 9") "(") ;
  (global-set-key (kbd "<menu> u 0") ")")

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
  (global-set-key (kbd "<menu> u u") nil)
  (global-set-key (kbd "<menu> u w") 'insert-pair-angle-bracket〈〉)
  (global-set-key (kbd "<menu> u x") 'insert-pair-tortoise-shell-bracket〔〕)
  (global-set-key (kbd "<menu> u y") 'insert-pair-single-angle-quote‹›)
  )

(global-set-key (kbd "<menu> v") nil)
(global-set-key (kbd "<menu> w") 'widen)
(global-set-key (kbd "<menu> x") ctl-x-map)
(global-set-key (kbd "<menu> y") nil)
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

