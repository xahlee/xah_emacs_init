;;-*- coding: utf-8 -*-
;; 2013-09-02

(defun xah-html-mode-keys ()
  "Modify keymaps used by `html-mode'."

  (define-key xhm-keymap (kbd "<delete>") xhm-single-keys-keymap)

  (local-set-key (kbd "<backspace>") 'delete-backward-char)

  (local-set-key (kbd "<delete> 4") 'xahsite-update-article-timestamp)
  (local-set-key (kbd "<delete> 5") 'xhm-mark-unicode)

  (local-set-key (kbd "<delete> SPC b") 'xah-angle-brackets-to-html)
  (local-set-key (kbd "<delete> SPC c") 'xwe-chinese-linkify)
  (local-set-key (kbd "<delete> SPC d") 'xah-html-perldoc-ref-linkify)
  (local-set-key (kbd "<delete> SPC e") 'xah-html-emacs-ref-linkify)
  (local-set-key (kbd "<delete> SPC f") 'xah-html-full-size-img-linkify)
  (local-set-key (kbd "<delete> SPC g") 'xah-clojure-word-ref-linkify)
  (local-set-key (kbd "<delete> SPC j") 'xah-image-file-to-html-figure-tag)
  (local-set-key (kbd "<delete> SPC p") 'xah-html-php-ref-linkify)
  (local-set-key (kbd "<delete> SPC r") 'xah-add-to-related-links)
  (local-set-key (kbd "<delete> SPC t") 'xwe-word-etymology-linkify)
  (local-set-key (kbd "<delete> SPC z") 'xah-amazon-linkify)

  (local-set-key (kbd "<delete> a") 'xwe-annotate)
  (local-set-key (kbd "<delete> b") 'make-blogger-entry)
  (local-set-key (kbd "<delete> c") 'xah-brackets-to-html)
  (local-set-key (kbd "<delete> d") 'xah-html-insert-date-tag)
  (local-set-key (kbd "<delete> e") 'xah-make-atom-entry)
  (local-set-key (kbd "<delete> f") 'xah-copy-url-current-file)
  (local-set-key (kbd "<delete> g") 'xah-browse-url-of-buffer)
  (local-set-key (kbd "<delete> h") 'xah-all-linkify)
  (local-set-key (kbd "<delete> i") 'xah-html-image-linkify)
  (local-set-key (kbd "<delete> n") 'xah-ref-span-tag)
  (local-set-key (kbd "<delete> z a") 'xah-html-insert-keywords-tag)
  (local-set-key (kbd "<delete> z b") 'xah-html-insert-lyrics-header)
  (local-set-key (kbd "<delete> z c") 'xah-html-insert-lyrics-table)
  (local-set-key (kbd "<delete> z d") 'xah-html-insert-screen-filler)
  (local-set-key (kbd "<delete> z f") 'xah-html-insert-midi)

)

(add-hook 'html-mode-hook 'xah-html-mode-keys)
(add-hook 'xah-html-mode-hook 'xah-html-mode-keys)
(add-hook 'nxml-mode-hook 'xah-html-mode-keys)

(defun xah-css-mode-setup ()
  "Modify keymaps used by `xah-css-mode'."
  (local-set-key (kbd "<delete> s") 'xah-sync-css))

(add-hook 'xah-css-mode-hook 'xah-css-mode-setup)



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
  (local-set-key (kbd "<delete> g") 'xah-view-emacs-manual-in-browser)
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
