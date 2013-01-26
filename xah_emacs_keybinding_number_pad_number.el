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

(global-set-key (kbd "<kp-4>") 'nil)
(global-set-key (kbd "<kp-5>") 'nil)
(global-set-key (kbd "<kp-6>") 'nil)
(global-set-key (kbd "<kp-7>") 'nil)
(global-set-key (kbd "<kp-8>") 'nil)
(global-set-key (kbd "<kp-9>") 'isearch-forward)
(global-set-key (kbd "<C-kp-9>") 'isearch-backward)
(global-set-key (kbd "<f6>") 'isearch-forward)
(global-set-key (kbd "<C-f6>") 'isearch-backward)

(defun xah-isearch-hook ()
  "Hook for `isearch-mode-hook'"
  (define-key isearch-mode-map (kbd "<C-kp-9>") 'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "<kp-9>") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "<C-f6>") 'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "<f6>") 'isearch-repeat-forward)
  )
(add-hook 'isearch-mode-hook 'xah-isearch-hook)

(global-set-key (kbd "<C-kp-0>") 'tags-loop-continue)
