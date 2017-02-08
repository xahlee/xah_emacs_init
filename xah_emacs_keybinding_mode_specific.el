;; -*- coding: utf-8; lexical-binding: t; -*-
;; 2013-09-02

(progn
  (require 'dired )
  (define-key dired-mode-map (kbd "6") 'dired-up-directory)
  (define-key dired-mode-map (kbd "-") 'xah-dired-rename-space-to-underscore)
  (define-key dired-mode-map (kbd "s") 'xah-dired-sort))

(progn
  (require 'info )
  (define-key Info-mode-map (kbd "<f5>") 'xah-view-emacs-manual-in-browser))

(when (boundp 'org-mode-hook)
  (defun xah-org-mode-setup ()
    "Modify keymaps used by `org-mode'."
    (local-set-key (kbd "<C-tab>") 'xah-next-user-buffer))
  (add-hook 'org-mode-hook 'xah-org-mode-setup))



(defun xah-rcirc-mode-keys ()
  "Modify keybindings for `rcirc'.
For `rcirc-mode-hook'."
  (local-set-key (kbd "<M-f11>") 'rcirc-insert-prev-input)
  (local-set-key (kbd "<M-f12>") 'rcirc-insert-next-input))
(add-hook 'rcirc-mode-hook 'xah-rcirc-mode-keys)

(setq rcirc-default-nick "mid2")
