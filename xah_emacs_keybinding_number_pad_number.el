;; -*- coding: utf-8; lexical-binding: t; -*-
;; 2013-01-09
;; Xah Lee



;; NUMBERIC KEYPAD. nice number pad conveniences as extra function keys

(global-set-key (kbd "<kp-subtract>") 'xah-close-current-buffer)
(global-set-key (kbd "<kp-divide>") 'xah-previous-user-buffer)
(global-set-key (kbd "<kp-multiply>") 'xah-next-user-buffer)

(global-set-key (kbd "<C-kp-divide>") 'xah-previous-emacs-buffer)
(global-set-key (kbd "<C-kp-multiply>") 'xah-next-emacs-buffer)

(global-set-key (kbd "<kp-decimal>") 'other-window)
(global-set-key (kbd "<kp-0>") 'delete-window)
(global-set-key (kbd "<kp-1>") 'delete-other-windows)
(global-set-key (kbd "<kp-2>") 'split-window-vertically)
(global-set-key (kbd "<kp-3>") 'xah-open-file-at-cursor)

(global-set-key (kbd "<kp-4>") 'nil)
(global-set-key (kbd "<kp-5>") 'nil)
(global-set-key (kbd "<kp-6>") 'nil)
(global-set-key (kbd "<kp-7>") 'nil)
(global-set-key (kbd "<kp-8>") 'nil)
(global-set-key (kbd "<kp-9>") 'isearch-forward)

(defun xah-isearch-hook ()
  "Hook for `isearch-mode-hook'"
  (define-key isearch-mode-map (kbd "<C-kp-9>") 'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "<kp-9>") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "<C-f7>") 'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "<f7>") 'isearch-repeat-forward)
  )
(add-hook 'isearch-mode-hook 'xah-isearch-hook)

(global-set-key (kbd "<C-kp-0>") 'tags-loop-continue)
