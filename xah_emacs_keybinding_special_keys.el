;; -*- coding: utf-8 -*-
;; 2014-01-29

(define-key key-translation-map (kbd "<apps>") (kbd "<menu>"))

(define-key key-translation-map (kbd "C-p") (kbd "<menu>")) ; Mac OS X don't do menu/app key.
;; 〈Mac OS X: Keyboard Layout, Keymapping, Keybinding, Software ⌨〉
;; http://xahlee.info/kbd/Mac_OS_X_keymapping_keybinding_tools.html

(define-key key-translation-map (kbd "<menu> <end>") (kbd "C-g"))
(define-key key-translation-map (kbd "<escape>") (kbd "C-g"))
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))



(global-set-key (kbd "<home>") 'eex-command-mode-activate)
(global-set-key (kbd "<end>") 'eex-insert-mode-activate)
;; (global-set-key (kbd "<return>") 'eex-insert-mode-activate)

;; (define-key key-translation-map (kbd "<henkan>") (kbd "<delete>")) ; henkan is the 変換 key on Japanese keyboard for “do convert”



(global-set-key (kbd "<S-backspace>") 'delete-char)
(global-set-key (kbd "<M-backspace>") 'backward-kill-word)



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

;  14181    0.88%  xah-backward-open-bracket
;  17177    1.07%  xah-forward-close-bracket



(global-set-key (kbd "<insert>") 'xah-switch-to-next-frame)



(global-set-key (kbd "<f14>") 'xah-close-current-buffer) ;truely ergo keyboard, upper right key

; 6067    0.38%  other-window

(global-set-key (kbd "<XF86Launch5>") 'xah-close-current-buffer) ; F14 upper right corner

(global-set-key (kbd "<XF86Cut>") 'xah-cut-line-or-region)
(global-set-key (kbd "<XF86Copy>") 'xah-copy-line-or-region)
(global-set-key (kbd "<XF86Paste>") 'yank)
(global-set-key (kbd "C-<XF86Paste>") 'yank-pop)
(global-set-key (kbd "<XF86Close>") 'xah-close-current-buffer)

