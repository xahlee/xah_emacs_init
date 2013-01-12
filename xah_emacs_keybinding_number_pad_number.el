;; -*- coding: utf-8 -*-
;; 2013-01-09
;; Xah Lee



;; NUMBERIC KEYPAD. nice number pad conveniences as extra function keys

(global-set-key (kbd "<kp-subtract>") 'ergoemacs-close-current-buffer)
(global-set-key (kbd "<kp-divide>") 'ergoemacs-previous-user-buffer)
(global-set-key (kbd "<kp-multiply>") 'ergoemacs-next-user-buffer)

(global-set-key (kbd "<C-kp-divide>") 'ergoemacs-previous-emacs-buffer)
(global-set-key (kbd "<C-kp-multiply>") 'ergoemacs-next-emacs-buffer)

(global-set-key (kbd "<kp-decimal>") 'other-window)
(global-set-key (kbd "<kp-0>") 'delete-window)
(global-set-key (kbd "<kp-1>") 'delete-other-windows)
(global-set-key (kbd "<kp-2>") 'split-window-vertically)
(global-set-key (kbd "<kp-3>") 'xah-open-file-at-cursor)

(global-set-key (kbd "<kp-4> <kp-4>") 'convert-english-chinese-punctuation)
(global-set-key (kbd "<kp-4> <kp-5>") 'remove-punctuation-trailing-redundant-space)
(global-set-key (kbd "<kp-4> <kp-6>") 'convert-ideographic/ascii-space)

(global-set-key (kbd "<kp-5>") 'save-buffer)
(global-set-key (kbd "<kp-6>") 'repeat-complex-command)

(global-set-key (kbd "<C-kp-4>") 'cycle-font-backward)
(global-set-key (kbd "<C-kp-5>") 'cycle-font-2)
(global-set-key (kbd "<C-kp-6>") 'cycle-font-forward)

(define-prefix-command 'xah-numpad-keymap)
(global-set-key (kbd "<kp-7>") 'xah-numpad-keymap)
(global-set-key (kbd "<kp-7> <kp-0>") 'xah-open-file-fast)
(global-set-key (kbd "<kp-7> <kp-3>") 'xah-open-file-from-clipboard)
(global-set-key (kbd "<kp-7> <kp-7>") 'bookmark-bmenu-list)
(global-set-key (kbd "<kp-7> <kp-8>") 'ibuffer)
(global-set-key (kbd "<kp-7> <kp-9>") 'recentf-open-files)

(global-set-key (kbd "<kp-8> <kp-8>") 'run-current-file)

(global-set-key (kbd "<kp-9>") 'isearch-forward)
(global-set-key (kbd "<C-kp-9>") 'isearch-backward)

(defun xah-isearch-hook ()
  "Hook for `isearch-mode-hook'"
  (define-key isearch-mode-map (kbd "<C-kp-9>") 'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "<kp-9>") 'isearch-repeat-forward)
  )
(add-hook 'isearch-mode-hook 'xah-isearch-hook)

(global-set-key (kbd "<C-kp-0>") 'tags-loop-continue)
