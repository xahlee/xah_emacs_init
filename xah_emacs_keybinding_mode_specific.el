;;-*- coding: utf-8 -*-
;; 2013-09-02

(defun xah-html-mode-keys ()
  "Modify keymaps used by `html-mode'."

  (define-key xhm-single-keys-keymap (kbd "4") 'xahsite-update-article-timestamp)
  (define-key xhm-single-keys-keymap (kbd "5") 'xhm-mark-unicode)

  (define-key xhm-single-keys-keymap (kbd "SPC b") 'xah-angle-brackets-to-html)
  (define-key xhm-single-keys-keymap (kbd "SPC c") 'xwe-chinese-linkify)
  (define-key xhm-single-keys-keymap (kbd "SPC d") 'xah-html-perldoc-ref-linkify)
  (define-key xhm-single-keys-keymap (kbd "SPC e") 'xah-html-emacs-ref-linkify)
  (define-key xhm-single-keys-keymap (kbd "SPC f") 'xah-html-full-size-img-linkify)
  (define-key xhm-single-keys-keymap (kbd "SPC g") 'xah-clojure-word-ref-linkify)
  (define-key xhm-single-keys-keymap (kbd "SPC j") 'xah-image-file-to-html-figure-tag)
  (define-key xhm-single-keys-keymap (kbd "SPC p") 'xah-html-php-ref-linkify)
  (define-key xhm-single-keys-keymap (kbd "SPC r") 'xah-add-to-related-links)
  (define-key xhm-single-keys-keymap (kbd "SPC t") 'xwe-word-etymology-linkify)
  (define-key xhm-single-keys-keymap (kbd "SPC z") 'xah-amazon-linkify)

  (define-key xhm-single-keys-keymap (kbd "a") 'xwe-annotate)
  (define-key xhm-single-keys-keymap (kbd "b") 'make-blogger-entry)
  (define-key xhm-single-keys-keymap (kbd "c") 'xah-brackets-to-html)
  (define-key xhm-single-keys-keymap (kbd "d") 'xah-html-insert-date-tag)
  (define-key xhm-single-keys-keymap (kbd "e") 'xah-make-atom-entry)
  (define-key xhm-single-keys-keymap (kbd "f") 'xah-copy-url-current-file)
  (define-key xhm-single-keys-keymap (kbd "g") 'xah-browse-url-of-buffer)
  (define-key xhm-single-keys-keymap (kbd "h") 'xah-all-linkify)
  (define-key xhm-single-keys-keymap (kbd "i") 'xah-html-image-linkify)
  (define-key xhm-single-keys-keymap (kbd "n") 'xah-ref-span-tag)
  (define-key xhm-single-keys-keymap (kbd "z a") 'xah-html-insert-keywords-tag)
  (define-key xhm-single-keys-keymap (kbd "z b") 'xah-html-insert-lyrics-header)
  (define-key xhm-single-keys-keymap (kbd "z c") 'xah-html-insert-lyrics-table)
  (define-key xhm-single-keys-keymap (kbd "z d") 'xah-html-insert-screen-filler)
  (define-key xhm-single-keys-keymap (kbd "z f") 'xah-html-insert-midi)

  (define-key xhm-keymap (kbd "<delete>") xhm-single-keys-keymap)
)

(add-hook 'html-mode-hook 'xah-html-mode-keys)
(add-hook 'xah-html-mode-hook 'xah-html-mode-keys)
(add-hook 'nxml-mode-hook 'xah-html-mode-keys)

(defun xah-css-mode-setup ()
  "Modify keymaps used by `xah-css-mode'."
  (require 'xah-css-mode)
  (define-key xcm-single-keys-keymap (kbd "s") 'xah-sync-css)
  (define-key xcm-keymap (kbd "<delete>") xcm-single-keys-keymap)
  )

(defun xah-css-mode-setup ()
  "Modify keymaps used by `xah-css-mode'."
  (require 'xah-css-mode)
  (define-key xcm-single-keys-keymap (kbd "s") 'xah-sync-css)
  (define-key xcm-keymap (kbd "<delete>") xcm-single-keys-keymap)
  )
(add-hook 'xah-css-mode-hook 'xah-css-mode-setup)

(defun xah-elisp-mode-setup ()
  "Modify keymaps used by `xah-elisp-mode'."
  (require 'xah-elisp-mode)
  (define-key xem-keymap (kbd "<delete>") xem-single-keys-keymap)
  )
(add-hook 'xah-elisp-mode-hook 'xah-elisp-mode-setup)

(defun xah-clojure-mode-setup ()
  "Modify keymaps used by `xah-clojure-mode'."
  (define-key xcj-keymap (kbd "<delete>") xcj-single-keys-keymap)
)

(add-hook 'xah-clojure-mode-hook 'xah-clojure-mode-setup)



(defun xah-rcirc-mode-keys ()
  "Modify keybindings for `rcirc'.
For `rcirc-mode-hook'."
  (local-set-key (kbd "<f11>") 'rcirc-insert-prev-input)
  (local-set-key (kbd "<f12>") 'rcirc-insert-next-input)
  )
(add-hook 'rcirc-mode-hook 'xah-rcirc-mode-keys)

(defun xah-Info-mode-keys ()
  "Modify keybindings for `Info-mode'.
For `Info-mode-hook'."
  (local-set-key (kbd "<menu> e g") 'xah-view-emacs-manual-in-browser)
  (local-set-key (kbd "<delete> e g") 'xah-view-emacs-manual-in-browser)
  (local-set-key (kbd "<mouse-8>") 'Info-history-back)
  )
(add-hook 'Info-mode-hook 'xah-Info-mode-keys)

(defun xah-eval-defun ()
  "like `eval-defun' but doesn't need proper indentation for it to work.
Still, the code isn't 100% correct.
"
  (interactive)
  (save-excursion
    (search-backward "(defun")
    ;;    (mark-sexp)
    ;;    (eval-region (region-beginning) (region-end))
    (forward-sexp)
    (call-interactively 'eval-last-sexp)
    )
  )
