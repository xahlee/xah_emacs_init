;; -*- coding: utf-8 -*-

(define-key key-translation-map (kbd "<escape>") (kbd "C-g"))
;; (define-key key-translation-map (kbd "ESC") (kbd "C-g"))
;(global-set-key (kbd "<escape>") 'keyboard-quit)

(global-set-key (kbd "<home>") 'x6-command-mode-activate)
(global-set-key (kbd "<f19>") 'x6-command-mode-activate)
;; (global-set-key (kbd "<f7>") 'x6-command-mode-activate)
;; (global-set-key (kbd "<f8>") 'x6-insert-mode-activate)

(global-set-key (kbd "<end>") 'x6-insert-mode-activate)
;; (global-set-key (kbd "<return>") 'x6-insert-mode-activate)

(global-set-key (kbd "<f15>") 'other-frame)      ; capslock
(global-set-key (kbd "<f17>") 'other-frame)      ; middle

(define-key key-translation-map (kbd "s-t") (kbd "<menu>"))
(define-key key-translation-map (kbd "<apps>") (kbd "<menu>"))
(define-key key-translation-map (kbd "C-t") (kbd "<menu>"))  ; useful when in terminal or Mac
(define-key key-translation-map (kbd "<f6>") (kbd "<menu>"))  ; useful when in terminal or Mac
(define-key key-translation-map (kbd "<C-return>") (kbd "<menu>"))  ; useful when in terminal or Mac
                                        ;(define-key key-translation-map (kbd "C-8") (kbd "<menu>"))
;; (define-key key-translation-map (kbd "<henkan>") (kbd "<delete>")) ; henkan is the 変換 key on Japanese keyboard for “do convert”

(global-set-key (kbd "<tab>") nil)      ; mode specific



(global-set-key (kbd "<S-backspace>") 'delete-char)
(global-set-key (kbd "<M-backspace>") 'backward-kill-word)



(global-set-key (kbd "<f2>") 'xah-cut-line-or-region)
(global-set-key (kbd "<f3>") 'xah-copy-line-or-region)
(global-set-key (kbd "<f4>") 'yank)
(global-set-key (kbd "<C-f4>") 'yank-pop)
(global-set-key (kbd "<f5>") 'undo)
(global-set-key (kbd "<C-f5>") 'redo)

(global-set-key (kbd "<C-S-iso-lefttab>") 'xah-previous-user-buffer)
(global-set-key (kbd "<C-tab>") 'xah-next-user-buffer)

(global-set-key (kbd "C-S-t") 'xah-open-last-closed) ; 832    0.05%  xah-open-last-closed

(global-set-key (kbd "C-w") 'xah-close-current-buffer) ; 19318    1.20%  xah-close-current-buffer
(global-set-key (kbd "C-z") 'comment-dwim) ; 1214    0.08%  comment-dwim


(global-set-key (kbd "<prior>") 'xah-backward-block) ; page up 93862    5.83%  xah-backward-block
(global-set-key (kbd "<next>") 'xah-forward-block) ;  page down 80008    4.97%  xah-forward-block

(global-set-key (kbd "<S-prior>") 'scroll-down) ; page up
(global-set-key (kbd "<S-next>") 'scroll-up) ; page down

;; keys for moving to prev/next code section (form feed; ^L)
(global-set-key (kbd "<C-M-prior>") 'backward-page) ; Ctrl+Alt+PageUp
(global-set-key (kbd "<C-M-next>") 'forward-page)   ; Ctrl+Alt+PageDown

(global-set-key (kbd "<C-M-next>") 'forward-page)   ; Ctrl+Alt+PageDown

(global-set-key (kbd "<C-prior>") 'xah-previous-user-buffer)
(global-set-key (kbd "<C-next>") 'xah-next-user-buffer)
(global-set-key (kbd "<S-prior>") 'xah-previous-emacs-buffer)
(global-set-key (kbd "<S-next>") 'xah-next-emacs-buffer)

;(global-set-key (kbd "<home>") 'xah-backward-open-bracket) ;  14181    0.88%  xah-backward-open-bracket
;(global-set-key (kbd "<end>") 'xah-forward-close-bracket) ;  17177    1.07%  xah-forward-close-bracket


(global-set-key (kbd "]") 'xah-close-current-buffer)

(global-set-key (kbd "<XF86Launch8>") 'save-buffer)

(global-set-key (kbd "<insert>") 'xah-switch-to-next-frame)



(global-set-key (kbd "<f1> 6") 'lookup-all-dictionaries)
(global-set-key (kbd "<f1> 7") 'lookup-google)
(global-set-key (kbd "<f1> 8") 'lookup-wikipedia)
(global-set-key (kbd "<f1> 9") 'lookup-word-definition)
(global-set-key (kbd "<f1> 0") 'lookup-answers.com)
(global-set-key (kbd "<f1> [") 'lookup-word-dict-org)
(global-set-key (kbd "<f1> ]") 'lookup-wiktionary)


(global-set-key (kbd "<f9> SPC") 'flyspell-buffer) ; 306    0.02%  flyspell-buffer
(global-set-key (kbd "<f9> <f6>") 'visual-line-mode)
(global-set-key (kbd "<f9> <f7>") 'linum-mode)
(global-set-key (kbd "<f9> <f8>") 'whitespace-mode)
(global-set-key (kbd "<f9> <f9>") 'shell)
(global-set-key (kbd "<f9> <f10>") 'calc)
(global-set-key (kbd "<f9> <f11>") 'calendar)
(global-set-key (kbd "<f9> <f12>") nil)

(global-set-key (kbd "<f9> 0") nil)
(global-set-key (kbd "<f9> 1") nil)
(global-set-key (kbd "<f9> 2") nil)
(global-set-key (kbd "<f9> 3") 'xah-elisp-mode)
(global-set-key (kbd "<f9> 4") 'xah-php-mode)
(global-set-key (kbd "<f9> 5") 'xah-html-mode)
(global-set-key (kbd "<f9> 6") 'html-mode)
(global-set-key (kbd "<f9> 7") nil)
(global-set-key (kbd "<f9> 8") nil)
(global-set-key (kbd "<f9> 9") nil)

(global-set-key (kbd "<f9> <delete>") 'delete-current-file)
(global-set-key (kbd "<f9> C-d") 'delete-current-file)
(global-set-key (kbd "<f9> <return>") 'run-current-file) ;  1494    0.09%  run-current-file

(global-set-key (kbd "<f10>") 'split-window-vertically)
(global-set-key (kbd "<f11>") 'delete-other-windows)
;; delete-window
(global-set-key (kbd "<f12>") 'other-window); 6067    0.38%  other-window

