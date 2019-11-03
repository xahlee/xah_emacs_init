;; -*- coding: utf-8; lexical-binding: t; -*-
;; 2013-01-09
;; Xah Lee

;; NUMBERIC KEYPAD. nice number pad conveniences as extra function keys



(global-set-key (kbd "<kp-subtract>") 'xah-close-current-buffer)
(global-set-key (kbd "<kp-divide>") 'xah-previous-user-buffer)
(global-set-key (kbd "<kp-multiply>") 'xah-next-user-buffer)

(global-set-key (kbd "<C-kp-divide>") 'xah-previous-emacs-buffer)
(global-set-key (kbd "<C-kp-multiply>") 'xah-next-emacs-buffer)


(global-set-key (kbd "<kp-next>") 'other-window)

(global-set-key (kbd "<kp-insert>") 'delete-window)

(global-set-key (kbd "<kp-end>") 'delete-other-windows)
(global-set-key (kbd "<kp-down>") 'split-window-vertically)
(global-set-key (kbd "<kp-next>") 'xah-open-file-at-cursor)

(global-set-key (kbd "<kp-left> <kp-left>") 'xah-convert-english-chinese-punctuation)
(global-set-key (kbd "<kp-left> <kp-space>") 'xah-remove-punctuation-trailing-redundant-space)
(global-set-key (kbd "<kp-left> <kp-right>") 'xah-convert-asian/ascii-space)

(global-set-key (kbd "<kp-space>") 'save-buffer)
(global-set-key (kbd "<kp-right>") 'repeat-complex-command)

(define-prefix-command 'xah-numpad-keymap)
(global-set-key (kbd "<kp-home>") 'xah-numpad-keymap)
(global-set-key (kbd "<kp-home> <kp-insert>") 'xah-open-file-fast)
(global-set-key (kbd "<kp-home> <kp-next>") 'xah-open-file-from-clipboard)
(global-set-key (kbd "<kp-home> <kp-home>") 'bookmark-bmenu-list)
(global-set-key (kbd "<kp-home> <kp-up>") 'ibuffer)
(global-set-key (kbd "<kp-home> <kp-prior>") 'recentf-open-files)

(global-set-key (kbd "<kp-up> <kp-up>") 'xah-run-current-file)

(global-set-key (kbd "<kp-prior>") 'isearch-forward)
(global-set-key (kbd "<C-kp-prior>") 'isearch-backward)

(defun xah-isearch-hook ()
  "Hook for `isearch-mode-hook'"
  (define-key isearch-mode-map (kbd "<C-kp-prior>") 'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "<kp-prior>") 'isearch-repeat-forward)
  )
(add-hook 'isearch-mode-hook 'xah-isearch-hook)

(global-set-key (kbd "<C-kp-insert>") 'tags-loop-continue)
