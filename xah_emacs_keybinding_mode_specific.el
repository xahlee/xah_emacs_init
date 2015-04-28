;;-*- coding: utf-8 -*-
;; 2013-09-02

(defun xah-html-mode-keys ()
  "Modify keymaps used by `html-mode'."

  (define-key xhm-single-keys-keymap (kbd "e") 'xah-make-atom-entry)
  (define-key xhm-single-keys-keymap (kbd "g") 'xah-browse-url-of-buffer)
  (define-key xhm-single-keys-keymap (kbd "h") 'xah-all-linkify)

  (define-key xhm-single-keys-keymap (kbd "SPC a") nil)
  (define-key xhm-single-keys-keymap (kbd "SPC b") 'xah-make-blogger-entry)
  (define-key xhm-single-keys-keymap (kbd "SPC c") 'xah-angle-brackets-to-html)
  (define-key xhm-single-keys-keymap (kbd "SPC d") 'xah-html-insert-date-tag)
  (define-key xhm-single-keys-keymap (kbd "SPC e") nil)
  (define-key xhm-single-keys-keymap (kbd "SPC f") 'xah-copy-url-current-file)
  (define-key xhm-single-keys-keymap (kbd "SPC g") nil)
  (define-key xhm-single-keys-keymap (kbd "SPC h") 'xah-html-image-linkify)
  (define-key xhm-single-keys-keymap (kbd "SPC i") nil)
  (define-key xhm-single-keys-keymap (kbd "SPC j") nil)
  (define-key xhm-single-keys-keymap (kbd "SPC k") nil)
  (define-key xhm-single-keys-keymap (kbd "SPC l") nil)
  (define-key xhm-single-keys-keymap (kbd "SPC m") nil)
  (define-key xhm-single-keys-keymap (kbd "SPC n") 'xah-add-reference-span-tag)
  (define-key xhm-single-keys-keymap (kbd "SPC o") nil)
  (define-key xhm-single-keys-keymap (kbd "SPC p") nil)
  (define-key xhm-single-keys-keymap (kbd "SPC q") nil)
  (define-key xhm-single-keys-keymap (kbd "SPC r d") 'xah-html-perldoc-ref-linkify)
  (define-key xhm-single-keys-keymap (kbd "SPC r e") 'xah-html-emacs-ref-linkify)
  (define-key xhm-single-keys-keymap (kbd "SPC r f") 'xah-html-full-size-img-linkify)
  (define-key xhm-single-keys-keymap (kbd "SPC r g") 'xah-clojure-word-ref-linkify)
  (define-key xhm-single-keys-keymap (kbd "SPC r j") 'xah-image-file-to-html-figure-tag)
  (define-key xhm-single-keys-keymap (kbd "SPC r p") 'xah-html-php-ref-linkify)
  (define-key xhm-single-keys-keymap (kbd "SPC r r") 'xah-add-to-related-links)
  (define-key xhm-single-keys-keymap (kbd "SPC r z") 'xah-amazon-linkify)
  (define-key xhm-single-keys-keymap (kbd "SPC s") nil)
  (define-key xhm-single-keys-keymap (kbd "SPC u") 'xahsite-update-article-timestamp)
  (define-key xhm-single-keys-keymap (kbd "SPC v") nil)
  (define-key xhm-single-keys-keymap (kbd "SPC w") nil)
  (define-key xhm-single-keys-keymap (kbd "SPC x") nil)
  (define-key xhm-single-keys-keymap (kbd "SPC y") nil)
  (define-key xhm-single-keys-keymap (kbd "SPC z") nil)

  (define-key xhm-single-keys-keymap (kbd "w a") 'xwe-annotate)
  (define-key xhm-single-keys-keymap (kbd "w e") 'xwe-bold-word)
  (define-key xhm-single-keys-keymap (kbd "w c") 'xwe-chinese-linkify)
  (define-key xhm-single-keys-keymap (kbd "w m") 'xwe-move-word-to-page)
  (define-key xhm-single-keys-keymap (kbd "w t") 'xwe-word-etymology-linkify)

  (define-key xhm-single-keys-keymap (kbd "w n") 'xwe-new-word-entry )
  (define-key xhm-single-keys-keymap (kbd "w i") 'xwe-insert-word-entry )
  (define-key xhm-single-keys-keymap (kbd "w d") 'xwe-add-definition )
  (define-key xhm-single-keys-keymap (kbd "w s") 'xwe-add-source )
  (define-key xhm-single-keys-keymap (kbd "w c") 'xwe-add-comment )
  (define-key xhm-single-keys-keymap (kbd "w g") 'xwe-search-next-unbold )
  (define-key xhm-single-keys-keymap (kbd "w p") 'xwe-query-find-then-bold )
  (define-key xhm-single-keys-keymap (kbd "w u") 'xwe-find-word-usage )

  (define-key xhm-single-keys-keymap (kbd "SPC z a") 'xah-html-insert-keywords-tag)
  (define-key xhm-single-keys-keymap (kbd "SPC z b") 'xah-html-insert-lyrics-header)
  (define-key xhm-single-keys-keymap (kbd "SPC z c") 'xah-html-insert-lyrics-table)
  (define-key xhm-single-keys-keymap (kbd "SPC z d") 'xah-html-insert-screen-filler)
  (define-key xhm-single-keys-keymap (kbd "SPC z f") 'xah-html-insert-midi)

  )

(add-hook 'xah-html-mode-hook 'xah-html-mode-keys)

(defun xah-css-mode-setup ()
  "Modify keymaps used by `xah-css-mode'."
  (require 'xah-css-mode)
  (define-key xcm-single-keys-keymap (kbd "s") 'xah-sync-css)
  )
(add-hook 'xah-css-mode-hook 'xah-css-mode-setup)



(defun xah-rcirc-mode-keys ()
  "Modify keybindings for `rcirc'.
For `rcirc-mode-hook'."
  (local-set-key (kbd "<f11>") 'rcirc-insert-prev-input)
  (local-set-key (kbd "<f12>") 'rcirc-insert-next-input)
  )
(add-hook 'rcirc-mode-hook 'xah-rcirc-mode-keys)



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
