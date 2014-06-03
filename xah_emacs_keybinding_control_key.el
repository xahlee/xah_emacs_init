;; -*- coding: utf-8 -*-

(global-set-key (kbd "<C-f9>") 'σ-syntax-bracket-backward)
(global-set-key (kbd "<C-f10>") 'σ-syntax-bracket-forward)
(global-set-key (kbd "<C-f7>") 'xah-goto-previous-overlay)
(global-set-key (kbd "<C-f8>") 'xah-goto-next-overlay)

(global-set-key (kbd "C-1") 'xah-cycle-font-previous)
(global-set-key (kbd "C-2") 'xah-cycle-font-next)
(global-set-key (kbd "C-3") 'xah-cycle-hyphen-underscore-space)
(global-set-key (kbd "C-4") 'xah-cycle-camel-style-case)
(global-set-key (kbd "C-5") 'xah-cycle-font-2)
(global-set-key (kbd "C-6") 'tags-loop-continue)
(global-set-key (kbd "C-7") 'xah-backward-punct)
(global-set-key (kbd "C-8") 'xah-forward-punct)
(global-set-key (kbd "C-9") nil)

(global-set-key (kbd "C-&") 'xah-previous-emacs-buffer)
(global-set-key (kbd "C-*") 'xah-next-emacs-buffer)

(progn
  (require 'ido)
  (define-key ido-file-completion-map (kbd "C-o") 'ido-fallback-command))

;; ;; (global-set-key (kbd "C-a") nil) ; select all
;; (global-set-key (kbd "C-b") nil)

;; ;; (global-set-key (kbd "C-c") nil) ; mode specific
;; (global-set-key (kbd "C-d") nil)
;; (global-set-key (kbd "C-e") nil)
;; ;; (global-set-key (kbd "C-f") nil) ; find
;; ;; (global-set-key (kbd "C-g") nil) ; cancel
;; ;; (global-set-key (kbd "C-h") nil) ; help
;; ;; (global-set-key (kbd "C-i") nil) ; tab
;; ;; (global-set-key (kbd "C-j") nil) ; newline
;; (global-set-key (kbd "C-k") nil)
;; (global-set-key (kbd "C-l") nil)
;; ;; (global-set-key (kbd "C-m") nil) ; return
(global-set-key (kbd "C-S-n") 'make-frame-command) ; new window
(global-set-key (kbd "C-n") 'xah-new-empty-buffer) ; 1700    0.11%  xah-new-empty-buffer

(global-set-key (kbd "C-o") 'find-file) ; open. 765    0.05%  ido-find-file
(global-set-key (kbd "C-S-o") 'xah-open-in-external-app) ; 0.00%  xah-open-in-external-app
;; (global-set-key (kbd "C-p") nil)
;; ;; (global-set-key (kbd "C-q") nil) ; quote
;; (global-set-key (kbd "C-r") nil)
;; (global-set-key (kbd "C-s") nil)
;; (global-set-key (kbd "C-t") nil)
;; (global-set-key (kbd "C-u") nil)
;; (global-set-key (kbd "C-v") nil)
;; ;; (global-set-key (kbd "C-w") nil) ; close
;; ;; (global-set-key (kbd "C-x") nil) C-x
;; ;; (global-set-key (kbd "C-y") nil)

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
;(global-unset-key (kbd "C-S-t") )       ; xah-open-last-closed
;(global-unset-key (kbd "C-u") )       ; universal-argument

(global-set-key (kbd "C-S-t") 'xah-open-last-closed) ; 832    0.05%  xah-open-last-closed
(global-set-key (kbd "C-w") 'xah-close-current-buffer) ; 19318    1.20%  xah-close-current-buffer

;; 1214    0.08%  comment-dwim

(global-set-key (kbd "C-z") 'undo)
