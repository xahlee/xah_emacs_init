;; -*- coding: utf-8 -*-
;; 2014-01-29
;; emacs keybinding for special keys
;; Xah Lee

;; 〈Mac OS X: Keyboard Layout, Keymapping, Keybinding, Software ⌨〉
;; http://xahlee.info/kbd/Mac_OS_X_keymapping_keybinding_tools.html

(define-key key-translation-map (kbd "<f17>") (kbd "C-g"))



(global-set-key (kbd "<home>") 'eex-command-mode-activate)
(global-set-key (kbd "<end>") 'eex-insert-mode-activate)
;; (global-set-key (kbd "<return>") 'eex-insert-mode-activate)

;; (define-key key-translation-map (kbd "<henkan>") (kbd "<delete>")) ; henkan is the 変換 key on Japanese keyboard for “do convert”



(global-set-key (kbd "<f2>") 'xah-cut-line-or-region)
(global-set-key (kbd "<f3>") 'xah-copy-line-or-region)
(global-set-key (kbd "<f4>") 'yank)
(global-set-key (kbd "<C-f4>") 'yank-pop)
(global-set-key (kbd "<f5>") 'undo)
(global-set-key (kbd "<C-f5>") 'redo)

(global-set-key (kbd "<f11>") 'xah-previous-user-buffer)
(global-set-key (kbd "<f12>") 'xah-next-user-buffer)
(global-set-key (kbd "<C-f11>") 'xah-previous-emacs-buffer)
(global-set-key (kbd "<C-f12>") 'xah-next-emacs-buffer)



;; page up 93862    5.83%  xah-backward-block
;; page down 80008    4.97%  xah-forward-block

;; problem in org mode
;; (global-set-key (kbd "<C-tab>") 'xah-previous-user-buffer) ; page up
;; (global-set-key (kbd "<C-S-tab>") 'xah-next-user-buffer) ; page down

;; ;; problem with setting page up/down key to scroll 10 lines is: if a split screen is juts 5 lines high, it goes over. • page down no longer move cursor to end of bufffer
;; (global-set-key (kbd "<prior>") 'xah-scroll-down-10-lines) ; page up
;; (global-set-key (kbd "<next>") 'xah-scroll-up-10-lines) ; page down

(global-set-key (kbd "<C-prior>") 'xah-previous-user-buffer) ; page up
(global-set-key (kbd "<C-next>") 'xah-next-user-buffer) ; page down

(global-set-key (kbd "<S-prior>") 'scroll-down) ; page up
(global-set-key (kbd "<S-next>") 'scroll-up) ; page down

;; keys for moving to prev/next code section (form feed; ^L)
(global-set-key (kbd "<C-M-prior>") 'backward-page) ; Ctrl+Alt+PageUp
(global-set-key (kbd "<C-M-next>") 'forward-page)   ; Ctrl+Alt+PageDown

(global-set-key (kbd "<C-M-next>") 'forward-page)   ; Ctrl+Alt+PageDown

;  14181    0.88%  xah-backward-left-bracket
;  17177    1.07%  xah-forward-right-bracket



(global-set-key (kbd "<insert>") 'other-frame)



(global-set-key (kbd "<f14>") 'xah-close-current-buffer) ;truely ergo keyboard, upper right key

; 6067    0.38%  other-window

(global-set-key (kbd "<XF86Launch5>") 'xah-close-current-buffer) ; F14 upper right corner

(global-set-key (kbd "<XF86Cut>") 'xah-cut-line-or-region)
(global-set-key (kbd "<XF86Copy>") 'xah-copy-line-or-region)
(global-set-key (kbd "<XF86Paste>") 'yank)
(global-set-key (kbd "C-<XF86Paste>") 'yank-pop)
(global-set-key (kbd "<XF86Close>") 'xah-close-current-buffer)

(progn
  (define-prefix-command 'xah-menu-f10-keymap)
  (global-set-key (kbd "<f10>") xah-menu-f10-keymap)

  (global-set-key (kbd "<f10> <left>") 'xah-goto-previous-overlay)
  (global-set-key (kbd "<f10> <right>") 'xah-goto-next-overlay)
  (global-set-key (kbd "<f10> <backspace>") 'xah-remove-overlays-region)
  (global-set-key (kbd "<f10> <return>") 'xah-show-overlay-at-point)

  (global-set-key (kbd "<f10> b") 'xah-make-overlay-bold-region)
  (global-set-key (kbd "<f10> a") 'xah-show-all-overlays)

  (global-set-key (kbd "<f10> 8") 'xah-syntax-bracket-forward)
  (global-set-key (kbd "<f10> 7") 'xah-syntax-bracket-backward)
  (global-set-key (kbd "<f10> c") 'xah-forward-comment)
  (global-set-key (kbd "<f10> l") 'xah-scan-list)
  (global-set-key (kbd "<f10> s") 'xah-scan-sexps)
  (global-set-key (kbd "<f10> p") 'xah-parse-partial-sexp)
  )

