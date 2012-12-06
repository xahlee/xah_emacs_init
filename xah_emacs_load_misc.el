;-*- coding: utf-8 -*-
; Xah Lee's elisp file for loading misc packages.

; 2007-06
;   Xah Lee
; ∑ http://xahlee.org/



; setting paths, loading modules, setup hooks etc.

(require 'xah-html-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . xah-html-mode))
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))

(require 'xah_file_util)


;; I-search with initial contents.
;; original source: http://platypope.org/blog/2007/8/5/a-compendium-of-awesomeness
(defvar isearch-initial-string nil)

(defun isearch-set-initial-string ()
  (remove-hook 'isearch-mode-hook 'isearch-set-initial-string)
  (setq isearch-string isearch-initial-string)
  (isearch-search-and-update))

(defun isearch-forward-at-point (&optional regexp-p no-recursive-edit)
  "Interactive search forward for the symbol at point."
  (interactive "P\np")
  (if regexp-p (isearch-forward regexp-p no-recursive-edit)
    (let* ((end (progn (skip-syntax-forward "w_") (point)))
           (begin (progn (skip-syntax-backward "w_") (point))))
      (if (eq begin end)
          (isearch-forward regexp-p no-recursive-edit)
        (setq isearch-initial-string (buffer-substring begin end))
        (add-hook 'isearch-mode-hook 'isearch-set-initial-string)
        (isearch-forward regexp-p no-recursive-edit)))))
